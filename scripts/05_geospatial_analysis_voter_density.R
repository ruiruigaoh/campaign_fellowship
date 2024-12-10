## LOAD PACKAGES ----

library(tidyverse)
library(vroom)
library(ggmap)
library(sf)
library(viridis) # for more color options for plotting
library(ggrepel) # for label repelling
library(ggsflabel) # for sf-specific label repelling
library(patchwork) # for easy grid plotting


## READ IN GEOCODED DATA ----

q_voters_gc <- vroom("../data/geocoding/q_voters_geocodio_output.csv",
                     .name_repair = janitor::make_clean_names)

## CLEAN UP GEOCODED DATA ----

# remove unneeded columns added by the geocoding service, Geocodio
q_voters_gc <- q_voters_gc %>% select(!city_2:source)

# write to output
vroom_write(q_voters_gc, "../data/geocoding/q_voters_geocodio_output_clean.csv", 
            col_names = TRUE, 
            delim = ",")


## DENSITY ANALYSIS (NEIGHBORHOOD LEVEL) ----

# filter for only registered dems and convert the df into a shapefile
dems <- q_voters_gc |>
  filter(political_party == "DEM") |>
  st_as_sf(coords = c("longitude", "latitude"), remove = FALSE, crs = 4326)

# create a helper function for density analysis
get_density <- function(df, ...) {
  df |>
    group_by(...) |>
    summarize(
      n_dems = ifelse("n_dems" %in% colnames(df), sum(n_dems), n()),
      geometry = ifelse(n_distinct(geometry) > 1, st_union(geometry), geometry),
      Shape_Area = ifelse(n_distinct(Shape_Area) > 1, sum(Shape_Area), max(Shape_Area)),
      .groups = "drop"
    ) |>
    mutate(
      # convert sq. meters unit into sq. km
      shape_area_km = Shape_Area / 1e+6,
      # calculate density per sq. km
      dem_std = round(n_dems / shape_area_km),
      density_percentile = cume_dist(dem_std)
    ) |>
    arrange(desc(dem_std))
}

# load nyc 2020 neighborhood tabulation areas (NTAs)
# (this data will be used to map the voters to specific neighborhoods)
neighborhoods <- st_read("../data/geocoding/nynta2020_23d/nynta2020.shp")

# map voters to specific neighborhoods
dems_neighborhoods <- dems |>
  st_transform(crs = st_crs(neighborhoods)) |>
  st_join(neighborhoods) |>
  select(
    county_emsid:middle, 
    address:apartment_number,
    neighborhood, 
    zip_code, 
    birth_date, 
    gender, 
    latitude:accuracy_type,
    unit_type, 
    NTA2020, 
    NTAName, 
    Shape_Area
  )

# join back with neighborhoods df to get back neighborhood geometry information
dems_neighborhoods <- dems_neighborhoods |>
  st_drop_geometry() |>
  left_join(neighborhoods[c("NTA2020", "geometry")],
            by = "NTA2020"
  )

# convert back to a shapefile object
dems_neighborhoods <- st_as_sf(dems_neighborhoods)

# apply the custom function 'get_density' to get voter density
dems_neighborhoods <- dems_neighborhoods |>
  get_density(NTA2020, NTAName)

# calculate the median number of democrat voters per neighborhood
med_dem_neighborhood <- round(median(dems_neighborhoods$n_dems))

# plot the top 20 neighborhoods with highest density of registered dems
density_lab <- "density (no. Dems / sq. km)"
voter_density_plot <- dems_neighborhoods |>
  arrange(desc(dem_std)) |>
  slice_head(n = 20) |>
  ggplot() +
  geom_point(
    aes(x = dem_std, y = fct_reorder(NTAName, dem_std), size = shape_area_km),
    shape = 21, fill = "lightblue"
  ) +
  labs(x = density_lab, y = "")

# as comparison, plot the top 20 neighborhoods by raw count of registered dems
voter_count_plot <- dems_neighborhoods %>% 
  arrange(desc(n_dems)) %>% 
  slice_head(n = 20) %>% 
  ggplot() +
  geom_point(
    aes(x = n_dems, y = fct_reorder(NTAName, n_dems), size = shape_area_km),
    shape = 21, fill = "lightblue"
  ) +
  labs(x = "number of voters", y = "")

# save plots
ggsave("../results/plots/voter_density_plot.pdf", voter_density_plot, width = 6)
ggsave("../results/plots/voter_count_plot.pdf", voter_count_plot, width = 6)


## DENSITY ANALYSIS (TRACT LEVEL)

# census tracts generally have a population of between 1.2K to 8K people. 
# the spatial size of census tracts varies depending on density (less dense tracts tend to be larger).
# census tracts are smaller than neighborhoods.
# census tracts can be further broken down into block groups, which comprise clusters of blocks within the same tract.

# load block group-level shapefile earlier downloaded from NHGIS
block_grps <- st_read("../data/geocoding/nhgis_ny_blockgroups_2020/NY_blck_grp_2020.shp")

# map voters to their block groups
dems_block_grps <- dems %>% 
  st_transform(crs = st_crs(block_grps)) %>% 
  st_join(block_grps) %>% 
  select(
    county_emsid:first, 
    address:apartment_number, 
    neighborhood,
    birth_date, 
    gender, 
    latitude:accuracy_type, 
    unit_type,
    geometry, 
    COUNTYFP:Shape_Area
  )

# merge back with block_grps to get block group geometries
dems_block_grps <- dems_block_grps |>
  st_drop_geometry() |>
  left_join(block_grps[c("GEOID", "geometry")],
            by = "GEOID"
  )

# convert back into a sf object
dems_block_grps <- st_as_sf(dems_block_grps)

# calculate voter density at the tract level
dems_tracts <- dems_block_grps |>
  get_density(TRACTCE)

# calculate median number of dem voters at tract level
med_dem_tract <- round(median(dems_tracts$n_dems))

# create a wrapper function for plotting histograms that show distribution of voter density across neighborhoods
hist_plot <- function(df, geo, x_var) {
  p <- ggplot(df) +
    geom_histogram(aes(x = {{ x_var }}), color = "gray", fill = "lightgray") +
    ggtitle(paste("Distributions at the", geo, "level")) +
    theme_classic()
  return(p)
}

# bucket tracts into different bins based on their density percentile
breaks <- c(seq(0, 0.9, by = 0.1), 0.95, 1)
tags <- sapply(c("< 10", seq(10, 90, by = 10), 95), paste0, "th")
dems_tracts <- dems_tracts |>
  mutate(density_pct_bin = cut(
    density_percentile,
    breaks = breaks,
    include.lowest = TRUE,
    right = TRUE,
    labels = tags
  ))

# visualize tracts in the top 30th percentile
tracts_density_plot <- ggplot(dems_tracts) +
  geom_sf() +
  geom_sf(
    data = dems_tracts |> filter(density_pct_bin %in% c("95th", "90th", "80th", "70th")),
    aes(fill = density_pct_bin)
  ) +
  scale_fill_brewer(palette = "YlOrRd") +
  labs(title = "Tracts with the highest density of registered Democrats",
       fill = "Density percentile") +
  theme(plot.title = element_text(face = "bold")) +
  theme_void()

# get the centroids of the tracts belonging to the top 5th percentile
densest_centroids <- dems_tracts %>% 
  filter(density_pct_bin == "95th") %>% 
  st_centroid()

# locate these centroids within neighborhoods geometries
densest_neighborhoods <- densest_centroids %>% 
  st_transform(crs = st_crs(neighborhoods)) %>% 
  st_join(neighborhoods[c("NTA2020", "NTAName", "geometry")])

# get centroids of top neighborhoods for labeling purposes
neighborhood_centroids <- neighborhoods %>% 
  filter(NTA2020 %in% c(densest_neighborhoods$NTA2020)) %>% 
  st_centroid()

# overlay neighborhoods, with labels, on top of the top 30th percentile tracts
densest_neighborhoods_plot <- tracts_density_plot +
  geom_sf(
    data = neighborhoods  %>% 
      filter(NTA2020 %in% c(densest_neighborhoods$NTA2020)),
    color = "blue", fill = NA, linewidth = 0.5
  ) +
  geom_sf(
    data = neighborhood_centroids,
    color = "blue", size = 0.5
  ) +
  geom_sf_label_repel(
    data = neighborhood_centroids,
    aes(label = NTAName),
    size = 3, min.segment.length = 0,
    force = 60,
    label.padding = unit(0.2, "lines"),
    segment.size = 0.3, segment.alpha = 0.8,
    segment.color = "blue",
    direction = "y"
  ) +
  labs(title = "Neighborhoods with the highest density tracts",
       subtitle = "Top 5th percentile") +
  theme(plot.title = element_text(face = "bold"))

# save plots
ggsave("../results/plots/voter_density_tract_level.pdf")

## DENSITY ANALYSIS (BLOCK GROUP LEVEL) ----

# calculate voter density at the block group level
dems_block_grps <- dems_block_grps |>
  get_density(TRACTCE, GEOID)

# plot the distribution of voter density 
hist_plot(dems_block_grps, "block group", dem_std) +
  xlab(density_lab)

# calculate median number of dem voters in a block group
med_dem_block_grp <- round(median(dems_block_grps$n_dems))

# visualize voter densities at the block group level
ggplot(dems_block_grps) +
  geom_sf(aes(fill = density_percentile)) +
  scale_fill_viridis(option = "viridis") +
  theme_void() +
  labs(
    title = "Density analysis of registered Democrats in Queens",
    subtitle = "At the block group level", 
    caption = paste("Median number of reg. Democrats per block group:", med_dem_block_grp),
    fill = "Density percentile"
  ) +
  theme(
    plot.title = element_text(face = "bold"),
    plot.caption = element_text(size = 10)
  )

# save plots
ggsave("../results/plots/voter_density_block_group_level.pdf")








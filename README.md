# Maximizing success in a historical political race: data-driven digital marketing and canvassing

From January to May of 2024, I leveraged my analytical and technical skills by working as a data analyst on a local grassroots judicial campaign in Queens, NY, leading up to the June 2024 Democratic primary election. The candidate, [Judge Wendy Li](https://www.voteforwendyli.com/), was running in a competitive election for the position of Surrogate's Court Judge in Queens. Her win was posed to be historic and would have been the first time a woman and minority occupied this seat in New York. 

The scope of the project was divided into two main objectives:

1) **Enhance the campaign's outreach strategies** by predicting a voter's ethnicity based on their geographic location and surname, and then producing voter lists for the marketing team for targeted outreach strategies by segmenting voters based on various demographic factors (race, political party, voting history). The tool used to perform the racial category prediction is the [wru](https://github.com/kosukeimai/wru) package from Imai et al., which implements the Bayesian prediction methods proposed in their 2016 _Political Analysis_ paper. 

2) Develop a **data-driven canvassing strategy** to optimize ballot access signature collection efforts by identifying areas and neighborhoods in Queens with the **highest target voter density** so that volunteers can be most effectively deployed in canvassing efforts.

---

## Technical overview

**Programming language:** R.

**Main packages:** _tidyverse, wru, ggplot2, sf, ggmap._

The scripts for segmenting voter lists based on demographic factors can be found [here](https://github.com/ruiruigaoh/campaign_fellowship/tree/main/scripts).


library(dplyr)
data <- read.csv("Data/EconMap_2.4_ssp1.csv") %>% 
  filter(year == 2014) %>% 
  select(gdp_05, capital, labor_force, secondary_educ, tfp, energy_productivity)




library(readxl)
library(tidyverse)
library(BSol.mapR)

#########################################################
#                   Life Expectancy                     #
#########################################################

msoa_life_exp <- read_excel("data/REQ3292_LifeExpectancy_2011MSOAs_England_2016_to_2020.xlsx") |> 
  rename(MSOA11CD = Code,
         MSOA11NM = "Code Name",
         life_expectancy = "Life Expectancy at birth (in years)",
         sex = Sex) |> 
  select(MSOA11CD, 
         MSOA11NM, 
         sex,
         life_expectancy, 
  ) |> 
  filter(grepl("Birmingham", MSOA11NM)) |> 
  select(MSOA11CD, sex, life_expectancy) |> 
  rename(MSOA11 = MSOA11CD,
         Sex = sex)
  

titles_msoa = list(
  "Males" = "Average Male Life Expectancy (2016 to 2020)", 
  "Females" = "Average Female Life Expectancy (2016 to 2020)", 
  "Persons" = "Average Life Expectancy (2016 to 2020)"
)

# Plot life expectancy maps
for (sex_i in names(titles_msoa)) {
  life_exp_i <- msoa_life_exp %>%
    filter(Sex == sex_i)
  map <- plot_map(
    life_exp_i,
    "life_expectancy",
    map_type = "MSOA11",
    area_name = "Birmingham",
    style = "cont",
    breaks = c(70, 75, 80, 85, 90, 95),
    map_title = titles_msoa[[sex_i]]
  )
  save_name_i <- file.path(
    "output", "Life Expectancy Maps",
    paste("brum_life_exp_msoa_",sex_i, ".png", sep = "")
  )
  save_map(map,save_name_i)
}

# some MSOA values missing due to suppression of low numbers of deaths

#########################################################
#                     Unemployment                      #
# (Percentage of economically active people unemployed) #
#########################################################

claimant_count_pct_bham <- claimant_count_2016_20 |> 
  rename(MSOA11NM = geography_name) |> 
  filter(grepl("Birmingham", MSOA11NM)) |> 
  group_by(MSOA11NM, MSOA11CD, sex) |> 
  summarise(claimant_count = mean (claimant_count)) |> 
  left_join(pop_est_mean,
             by = c("MSOA11CD", "sex")) |> 
  mutate(claimant_count_pct = (claimant_count/pop_est)*100) |> 
  ungroup() |> 
  select(MSOA11CD, sex, claimant_count_pct) |> 
  rename(MSOA11 = MSOA11CD)

titles_msoa_unemp = list(
  "Male" = "% of Male Residents Claiming \n Unemployment Benefits (2016 to 2020)", 
  "Female" = "% of Female Residents Claiming \n Unemployment Benefits (2016 to 2020)"
)

# Plot life expectancy maps
for (sex_i in names(titles_msoa_unemp)) {
  unemp_i <- claimant_count_pct_bham %>%
    filter(sex == sex_i)
  map <- plot_map(
    unemp_i,
    "claimant_count_pct",
    map_type = "MSOA11",
    area_name = "Birmingham",
    style = "cont",
    breaks = c(0, 3, 6, 9, 12, 15, 18),
    map_title = titles_msoa_unemp[[sex_i]]
  )
  save_name_i <- file.path(
    "output",
    paste("brum_unemp_msoa_",sex_i, ".png", sep = "")
  )
  save_map(map,save_name_i)
}


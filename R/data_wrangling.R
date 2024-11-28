library(tidyverse)
library(readxl)

# Import data -------------------------------------------------------------

# LSOA to MSOA lookup (from ONS Geoportal - https://geoportal.statistics.gov.uk/datasets/797715a33bf54ba2885e1e20d04c5513/about)
# loaded data into excel and used power query to group by 2011 LSOA and 2011 MSOA
lsoa_msoa_lookup <- read_excel("data/lsoa_msoa_lookup.xlsx")

# import indices of deprivation scores and just keep relevant indices
# data from https://www.gov.uk/government/statistics/english-indices-of-deprivation-2019
iod_scores <- read_excel("data/IoD2019_Scores.xlsx",
                         sheet = "IoD2019 Scores") |> 
  rename(LSOA11CD = "LSOA code (2011)",
         LSOA11NM = "LSOA name (2011)",
         income_score = "Income Score (rate)",
         education_score = "Education, Skills and Training Score",
         crime_score = "Crime Score",
         housing_score = "Barriers to Housing and Services Score",
         environment_score = "Living Environment Score") |> 
  select(LSOA11CD, LSOA11NM, income_score, education_score, crime_score, housing_score, environment_score)

# import population denominators for weighting IoD scores before aggregation
# data from https://www.gov.uk/government/statistics/english-indices-of-deprivation-2019
iod_population_denominators <- read_excel("data/IoD2019_Population_Denominators.xlsx",
                                          sheet = "Population Denominators") |> 
  rename(LSOA11CD = "LSOA code (2011)",
         LSOA11NM = "LSOA name (2011)",
         total_pop = "Total population: mid 2015 (excluding prisoners)") |> 
  select(LSOA11CD, LSOA11NM, total_pop)

# claimant count for adults aged 16+ for all England MSOAs between 2016 and 2020
claimant_count_2016_20 <- read.csv("data/claimant_count_16+_msoa_england_2016-20.csv") |> 
  rename(MSOA11CD = geography_code)

# mid year population estimates for working age adults (16-64) for all England MSOAs between 2016 and 2020
pop_est_2016_20 <- read.csv("data/pop_est_16-64_msoa_england_2016-20.csv")|> 
  rename(MSOA11CD = geography_code)

# life expectancy data for all England MSOAs in 2016-2020 period
life_expectancy <- read_excel("data/REQ3292_LifeExpectancy_2011MSOAs_England_2016_to_2020.xlsx") |> 
  rename(MSOA11CD = Code,
         MSOA11NM = "Code Name",
         life_expectancy = "Life Expectancy at birth (in years)",
         #life_expectancy_95_ci_lower = "Lower 95% confidence interval",
         #life_expectancy_95_ci_upper = "Upper 95% confidence interval",
         sex = Sex) |> 
  select(MSOA11CD, 
         MSOA11NM, 
         sex,
         life_expectancy, 
         #life_expectancy_95_ci_lower, 
         #life_expectancy_95_ci_upper, 
          ) |> 
  filter(sex != "Persons") |> 
  mutate(sex = case_when(sex == "Males" ~ 1,
                         sex == "Females" ~ 0)) #|> 
  # pivot_wider(names_from = sex,
  #             values_from = life_expectancy) |> 
  # rename(male_life_expectancy = Male,
  #        female_life_expectancy = Female)


# Wrangle data ------------------------------------------------------------

# aggregate IoD scores to MSOA level by merging with MSOA lookup table, then summing population weighted scores from LSOAs and dividing by MSOA population
# as described in Appendix A of English IoD 2019 Research report (https://assets.publishing.service.gov.uk/media/5d8b364ced915d03709e3cf2/IoD2019_Research_Report.pdf)
iod_scores_msoa <- iod_scores |> 
  left_join(iod_population_denominators,
            by = c("LSOA11CD", "LSOA11NM")) |> 
  left_join(lsoa_msoa_lookup,
            by = "LSOA11CD") |> 
  group_by(MSOA11CD, MSOA11NM) |> 
  summarise(income_score_avg = sum(income_score*total_pop)/sum(total_pop),
            education_score_avg = sum(education_score*total_pop)/sum(total_pop),
            crime_score_avg = sum(crime_score*total_pop)/sum(total_pop),
            housing_score_avg = sum(housing_score*total_pop)/sum(total_pop),
            environment_score_avg = sum(environment_score*total_pop)/sum(total_pop))

# find mean monthly claimant count for males and females in each MSOA across all 5 years
claimant_count_mean <- claimant_count_2016_20 |> 
  group_by(MSOA11CD, sex) |> 
  summarise(claimant_count = mean (claimant_count))

# find mean population estimate for males and females in each MSOA across all 5 years
pop_est_mean <- pop_est_2016_20 |> 
  group_by(MSOA11CD, sex) |> 
  summarise(pop_est = mean(population_estimate))

# calculate claimant count % for each MSOA by merging claimant count and pop est means and mutating
claimant_count_pct <- left_join(pop_est_mean, claimant_count_mean,
                                by = c("MSOA11CD", "sex")) |> 
  mutate(claimant_count_pct = (claimant_count/pop_est)*100) |> 
  select(MSOA11CD, sex, claimant_count_pct) |> 
  # pivot_wider(names_from = sex,
  #             values_from = claimant_count_pct) |> 
  # rename(female_claimant_pct = Female,
  #        male_claimant_pct = Male)
  mutate(sex = case_when(sex == "Male" ~ 1,
                         sex == "Female" ~ 0))

# join all variables into one df and remove rows containing NA
all_variables <- life_expectancy |> 
  left_join(claimant_count_pct,
            by = c("MSOA11CD", "sex")) |> 
  left_join(iod_scores_msoa,
            by = c("MSOA11CD", "MSOA11NM")) |> 
  drop_na()
str(all_variables)

# factorise variables where relevant
all_variables$MSOA11CD <- as_factor(all_variables$MSOA11CD)
all_variables$MSOA11NM <- as_factor(all_variables$MSOA11NM)
all_variables$sex <- as_factor(all_variables$sex)


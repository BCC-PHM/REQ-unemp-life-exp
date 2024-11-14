library(readxl)
library(dplyr)
library(stringr)
library(BSol.mapR)

#########################################################
#                   Life Expectancy                     #
#########################################################

# Load data
ward_life_exp <- read_excel(
  file.path("//svwvap1126.addm.ads.brm.pri", "PHSensitive$","Intelligence", 
            "2. Requests", "REQ3292 - Life Expectancy and Worklessness for Jen",
            "REQ2024_LifeExpectancy_2018Wards_2020_to_2022.xlsm"),
  sheet = "Life Expectancy Analysis",
  skip = 13,
) %>%
  mutate(
    Ward = str_to_title(`2018 Ward Name`)
  ) %>%
  select(
    Ward, Sex, `Life Expectancy at Birth`
  )

titles = list(
  "Males" = "Average Male Life Expectency (2020 to 2022)", 
  "Females" = "Average Female Life Expectency (2020 to 2022)", 
  "Persons" = "Average Life Expectency (2020 to 2022)"
)

# Plot life expectancy maps
for (sex_i in names(titles)) {
  life_exp_i <- ward_life_exp %>%
    filter(Sex == sex_i)
  map <- plot_map(
    life_exp_i,
    "Life Expectancy at Birth",
    map_type = "Ward",
    area_name = "Birmingham",
    style = "cont",
    breaks = c(70, 75, 80, 85, 90),
    map_title = titles[[sex_i]]
  )
  save_name_i <- file.path(
    "output", "Life Expectancy Maps",
    paste("brum_life_exp_",sex_i, ".png", sep = "")
  )
  save_map(map,save_name_i)
}

#########################################################
#                     Unemployment                      #
# (Percentage of economically active people unemployed) #
#########################################################

econ_activity <- read_excel(
  "data/census21-brum-ward-econ-activity.xlsx"
  ) %>%
  mutate(
    # Remove "(Birmingham)" from ward names
    Ward = gsub('\\s+\\(Birmingham\\)', '', `Electoral wards and divisions`),
    Unemployed = case_when(
      grepl(
        "Unemployed",
        `Economic activity status (7 categories)`
        ) ~ Observation,
      TRUE ~ 0
    ),
    `Economically Active` = case_when(
      grepl(
        "Economically active",
        `Economic activity status (7 categories)`
      ) ~ Observation,
      TRUE ~ 0
    )
  ) %>%
  group_by(
    Ward
  ) %>%
  summarise(
    `Unemployed %` = 100 * sum(Unemployed) / sum(`Economically Active`)
  )

unemp_map <- plot_map(
  econ_activity,
  "Unemployed %",
  map_type = "Ward",
  area_name = "Birmingham",
  style = "cont",
  breaks = c(0, 5, 10, 15, 20, 25, 30),
  map_title = "Percentage of Economically Active Population who are Unemployed"
)
save_name_i <- "output/birmnigham_unemployment.png"
save_map(unemp_map,save_name_i)
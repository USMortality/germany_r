options(vsc.dev.args = list(width = 1200, height = 670, res = 144))

install.packages("pacman")
pacman::p_load(
  tsibble,
  tidyverse,
  readxl,
  fabletools,
  fable,
  tsibbledata,
  reshape2,
  dplyr,
  ggplot2,
  stringr,
  clock,
  fable,
  scales
)

source("./util.r")

# Import and convert data

## Population
population <- as_tibble(read.csv("./data/12411-0005_flat.csv", sep = ";"))
# Use 31.12.1999 for 2000, etc.
population <- population %>%
  select(5, 12, 14) %>%
  setNames(c(c("year", "age", "population"))) %>%
  mutate(year = as.integer(right(year, 4)) + 1) %>%
  mutate(age = gsub("ALT0", "", age))

population_grouped <- population %>%
  mutate(
    # Create categories
    age_group = case_when(
      age == "" ~ "Total",
      age >= 0 & age <= 29 ~ "0-29",
      age >= 30 & age <= 34 ~ "30-34",
      age >= 35 & age <= 39 ~ "35-39",
      age >= 40 & age <= 44 ~ "40-44",
      age >= 45 & age <= 49 ~ "45-49",
      age >= 50 & age <= 54 ~ "50-54",
      age >= 55 & age <= 59 ~ "55-59",
      age >= 60 & age <= 64 ~ "60-64",
      age >= 65 & age <= 69 ~ "65-69",
      age >= 70 & age <= 74 ~ "70-74",
      age >= 75 & age <= 79 ~ "75-79",
      age >= 80 & age <= 84 ~ "80-84",
      age >= 85 & age <= 89 ~ "85-89",
      age >= 90 & age <= 94 ~ "90-94",
      age >= 95 ~ "95+"
    ),
    # Convert to factor
    age_group = factor(
      age_group,
      level = c(
        "Total", "0-29", "30-34", "35-39", "40-44", "45-49", "50-54", "55-59",
        "60-64", "65-69", "70-74", "75-79", "80-84", "85-89", "90-94", "95+"
      )
    )
  ) %>%
  group_by(year, age_group) %>%
  summarise(population = sum(population))

## Deaths
deaths1 <- read_excel(
  "./data/sonderauswertung-sterbefaelle-endgueltige-daten.xlsx",
  sheet = "D_2000_2015_KW_AG_Ins",
  skip = 8
)
deaths2 <- read_excel(
  "./data/sonderauswertung-sterbefaelle.xlsx",
  sheet = "D_2016_2022_KW_AG_Ins",
  skip = 8
)
# Combine and rename
deaths <- rbind(deaths1, deaths2) %>%
  select(seq(2, 53 + 3)) %>%
  setNames(c(c("year", "age_group"), seq(1, 53))) %>%
  arrange(year, age_group)

deaths <- as_tibble(melt(
  deaths,
  id = c("year", "age_group"),
  variable.name = "week",
  value.name = "deaths"
)) %>%
  mutate(deaths = as.integer(deaths)) %>%
  filter(!is.na(deaths)) %>%
  mutate(
    age_group = case_when(
      age_group == "0-30" ~ "0-29",
      age_group == "30-35" ~ "30-34",
      age_group == "35-40" ~ "35-39",
      age_group == "40-45" ~ "40-44",
      age_group == "45-50" ~ "45-49",
      age_group == "50-55" ~ "50-54",
      age_group == "55-60" ~ "55-59",
      age_group == "60-65" ~ "60-64",
      age_group == "65-70" ~ "65-69",
      age_group == "70-75" ~ "70-74",
      age_group == "75-80" ~ "75-79",
      age_group == "80-85" ~ "80-84",
      age_group == "85-90" ~ "85-89",
      age_group == "90-95" ~ "90-94",
      age_group == "95 u. mehr" ~ "95+",
      age_group == "Insgesamt" ~ "Total"
    )
  )

# Calculate mortality
mortality <- deaths %>%
  inner_join(population_grouped, by = c("year", "age_group")) %>%
  mutate(week = as.integer(week)) %>%
  mutate(date = paste0(year, "-W", str_pad(week, 2, pad = "0"), "-1")) %>%
  mutate(date = date_parse(date, format = "%G-W%V-%u")) %>%
  mutate(mortality = deaths / population * 100000)

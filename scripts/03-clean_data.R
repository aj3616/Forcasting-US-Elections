#### Preamble ####
# Purpose: Cleans the raw US election data
# Author: Amy Jin
# Date: 18 October 2024
# Contact: amyzh.jin@mail.utoronto,ca
# License: MIT
# Pre-requisites: [...UPDATE THIS...]
# Any other information needed? [...UPDATE THIS...]

#### Workspace setup ####
library(tidyverse)

#### Clean data ####
raw_data <- read_csv("data/01-raw_data/president_polls.csv")

## filter out variables of interests
filtered_data <- raw_data %>%
  select(pollster_id, pollster, state,
         start_date, end_date, sample_size, population, race_id,
         cycle, election_date, candidate_id)

# TODO: discuss whether to keep NA of states and sample size
na_counts <- filtered_data %>%
  summarise(across(everything(), ~ sum(is.na(.))))

#### Save data ####
write_csv(filtered_data, "data/02-analysis_data/analysis_data.csv")


cleaned_data <-
  raw_data |>
  janitor::clean_names() |>
  select(wing_width_mm, wing_length_mm, flying_time_sec_first_timer) |>
  filter(wing_width_mm != "caw") |>
  mutate(
    flying_time_sec_first_timer = if_else(flying_time_sec_first_timer == "1,35",
                                   "1.35",
                                   flying_time_sec_first_timer)
  ) |>
  mutate(wing_width_mm = if_else(wing_width_mm == "490",
                                 "49",
                                 wing_width_mm)) |>
  mutate(wing_width_mm = if_else(wing_width_mm == "6",
                                 "60",
                                 wing_width_mm)) |>
  mutate(
    wing_width_mm = as.numeric(wing_width_mm),
    wing_length_mm = as.numeric(wing_length_mm),
    flying_time_sec_first_timer = as.numeric(flying_time_sec_first_timer)
  ) |>
  rename(flying_time = flying_time_sec_first_timer,
         width = wing_width_mm,
         length = wing_length_mm
         ) |> 
  tidyr::drop_na()

#### Save data ####
write_csv(cleaned_data, "outputs/data/analysis_data.csv")

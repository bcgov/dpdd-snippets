# Copyright 2021 Province of British Columbia
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
# http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and limitations under the License.


library(readr)
library(cansim)
library(janitor)
library(dplyr)
library(ggplot2)
library(scales)
library(tidyr)


## get data --------------------------------------------------------------------

#bc population estimate data by CSD from Statistics Canada
bcpop_csd_raw <- get_cansim("17-10-0005-01") %>%
  normalize_cansim_values() %>%
  clean_names()

#LHA data sourced manually from BC Stats https://bcstats.shinyapps.io/popApp/
bcpop_lha_raw <- read_csv("bcpop/data/Population_Estimates.csv") %>%
  clean_names()


## bc totals by year  ----------------------------------------------------------
bcpop_tot_lhadata <- bcpop_lha_raw %>%
  filter(region == "0",
         gender == "T") %>%
  select(local_health_area, year, total)

bcpop_tot_csddata <- bcpop_csd_raw %>%
  filter(geo == "British Columbia",
         sex == "Both sexes",
         age_group == "All ages") %>%
  select(ref_date, geo, sex, age_group, value)

#bcpop growth past 5 years (2015-2019)
bcpop_tot_lhadata %>%
  filter(year %in% c("2015", max(year))) %>%
  mutate(change = total - lag(total),
         change_percent = total / lag(total) - 1) %>%
  filter(year == max(year)) %>%
  pull(change_percent) %>%
  percent()


## regional totals by gender & year --------------------------------------------

bcpop_lha <- bcpop_lha_raw %>%
  mutate(age_0_to_14 = rowSums(across(x0:x14), na.rm = T),
         age_15_to_24 = rowSums(across(x15:x24), na.rm = T),
         age_25_to_54 = rowSums(across(x25:x54), na.rm = T),
         age_55_plus = rowSums(across(x55:x90), na.rm = T)) %>%
  pivot_longer(cols = c("age_0_to_14",
                        "age_15_to_24",
                        "age_25_to_54",
                        "age_55_plus",
                        "total"),
               names_to = "age_group",
               values_to = "estimate") %>%
  mutate(estimate = as.integer(estimate)) %>%
  select(year, region, local_health_area, gender, age_group, estimate)

  #  group_by(year, region, local_health_area, gender, age_group) %>%
  # mutate(pop_change = estimate - lag(estimate),
  #        pop_change_percent = estimate / dplyr::lag(estimate) - 1)


bcpop_lha %>%
   filter(region != "0",
          age_group == "total") %>%
  ggplot(aes(year, estimate, colour = gender)) +
  geom_point() +
  geom_line() +
  facet_wrap(facets = vars(local_health_area)) +
  labs(x = NULL, y = NULL) +
  theme_minimal()

bcpop_lha %>%
  filter(region == "143",
          age_group == "total") %>%
  ggplot(aes(year, estimate, colour = gender)) +
  geom_point() +
  geom_line() +
  labs(x = NULL, y = NULL) +
  theme_minimal()

bcpop_lha %>%
  filter(gender == "T",
         age_group == "total",
         region %in% c("426", "516")) %>%
  ggplot(aes(year, estimate)) +
  geom_line() +
  facet_wrap(facets = vars(local_health_area)) +
  labs(x = NULL, y = NULL) +
  theme_minimal()

bcpop_lha %>%
  filter(region == "143",
         age_group == "total",
         year %in% c("2015", max(year))) %>%
  group_by(gender) %>%
  mutate(change = estimate - lag(estimate),
         change_percent = estimate / lag(estimate) - 1) %>%
  filter(year == max(year))




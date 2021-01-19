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
library(stringr)


## get data --------------------------------------------------------------------

#bc population estimate data from Statistics Canada (1971-current)
bcpop_raw <- get_cansim("17-10-0005-01") %>%
  normalize_cansim_values() %>%
  clean_names()

#by CD from Statistics Canada (2001-current)
bcpop_cd_raw <- get_cansim("17-10-0139-01") %>%
  normalize_cansim_values() %>%
  clean_names()

#by CSD from Statistics Canada (2001-current, no demographic variables)
bcpop_csd_raw <- get_cansim("17-10-0142-01") %>%
  normalize_cansim_values() %>%
  clean_names()

#LHA data sourced manually from
#BC Stats https://bcstats.shinyapps.io/popApp/ (1986 -current)
bcpop_lha_raw <- read_csv("bcpop/data/Population_Estimates.csv") %>%
  clean_names()


## some data checks  -----------------------------------------------------------

bcpop_tot <- bcpop_raw %>%
  filter(geo == "British Columbia",
         sex == "Both sexes",
         age_group == "All ages") %>%
  select(year = ref_date, geo, sex, age_group, value)

bcpop_tot_cddata <- bcpop_cd_raw %>%
  filter(geo == "British Columbia",
         sex == "Both sexes",
         age_group == "All ages") %>%
  select(year = ref_date, geo, sex, age_group, value)

bcpop_tot_csddata <- bcpop_csd_raw %>%
  filter(geo_uid == "59") %>%
  select(year = ref_date, geo, value)

bcpop_tot_lhadata <- bcpop_lha_raw %>%
  filter(region == "0",
         gender == "T") %>%
  select(year, local_health_area, total)

#qa/qc check
if(bcpop_tot_lhadata %>%
   filter(year == "2019") %>%
   pull() != bcpop_tot_csddata %>%
   filter(ref_date == "2019") %>%
   pull()) stop("CSD & LHA Totals don't add up") #check version/date of data sets


## tidy data -------------------------------------------------------------------

#bcstats LHA data
bcpop_lha_all <- bcpop_lha_raw %>%
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
  mutate(estimate = estimate) %>%
  select(year, region, local_health_area, gender, age_group, estimate)
  # group_by(year, region, local_health_area, gender, age_group)
  # mutate(pop_change = estimate - lag(estimate),
  # pop_change_percent = estimate / lag(estimate) - 1)


#statscan cd data
bcpop_cd <- bcpop_cd_raw %>%
  filter(
    geo != "British Columbia",
    str_starts(geo_uid, "59")
    ) %>%
  mutate(geo = str_remove(geo, ", British Columbia")) %>%
  filter(!age_group %in% c("All ages", "Median age", "Average age", "65 years and older")) %>%
  filter(!grepl(" to ", age_group)) %>%
  mutate(age_group = trimws(gsub('years|year|and older', '', age_group))) %>%
  mutate(age_group = as.numeric(age_group)) %>%  ## THIS NEEDS TO GENERATE NO WARNINGS
  mutate(age_group = case_when(
    between(age_group, 0, 14) ~ "age_0_to_14",
    between(age_group, 15, 24) ~ "age_15_to_24",
    between(age_group, 25, 54) ~ "age_25_to_54",
    age_group >= 55 ~ "age_55_plus")) %>%
  group_by(ref_date, geo, geo_uid, sex, age_group) %>%
  summarise(value = sum(value)) %>%
  select(year = ref_date, geo, geo_uid, sex, age_group, value)

#qa/qc check
if(bcpop_cd %>% filter(year == "2020",
                       sex == "Both sexes") %>%
   group_by(year) %>%
   summarise(sum(value)) %>%
   pull() != bcpop_tot_cddata %>%
   filter(year == "2020") %>%
   pull(value)) stop("Totals don't add up")


#statscan csd data
bcpop_csd <- bcpop_csd_raw %>%
  filter(geo != "British Columbia",
         str_starts(geo_uid, "59")) %>%
   mutate(geo = str_remove(geo, ", British Columbia")) %>%
   select(year = ref_date, geo, geo_uid, value)

#qa/qc check
if(bcpop_csd %>%
   filter(year == "2020") %>%
   group_by(year) %>%
   summarise(sum(value)) %>%
   pull()  != bcpop_tot_cddata %>%
   filter(year == "2020") %>%
   pull(value)) stop("Totals don't add up")


## bcpop change by LHA  --------------------------------------------------------

#bcpop growth past 5 years (2015-2019)
bcpop_tot_lhadata %>%
  filter(year %in% c("2015", max(year))) %>%
  mutate(change = total - lag(total),
         change_percent = total / lag(total) - 1) %>%
  filter(year == max(year)) %>%
  pull(change_percent) %>%
  percent()


## regional totals by gender & year --------------------------------------------

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




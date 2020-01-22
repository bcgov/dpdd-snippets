# Copyright 2020 Province of British Columbia
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


## Load libraries ------------------------------------------------------------
library(dplyr)
library(data.table)
library(dtplyr)
library(tidyfast) #https://github.com/TysonStanley/tidyfast
library(tidydt) #https://github.com/markfairbanks/tidydt
library(conflicted)
library(bench)
library(ggplot2)

## COMPARE SPEED of DT & DT WRAPPER PACKAGES  --------------------------------

## function to make some fake data
create_fake_data <- function(number_of_rows) {
fd <-   data.frame(sample(1:100, number_of_rows, replace = TRUE),
                       sample(seq(as.Date('1990/01/01'), as.Date('2010/01/01'),
                                  by = "day"), number_of_rows, replace = TRUE),
                       sample(c("male", "female", "unknown"), number_of_rows,
                              replace = TRUE),
                       sample(c("a", "b", "c"), number_of_rows, replace = TRUE))

colnames(fd) <- c("some_number", "date", "gender", "some_letter")

fd <- fd %>%
  mutate(some_letter = as.character(some_letter),
         gender = as.character(gender))
}


## for testing
# number_of_rows <- 1E3


## microbenching filter & summarise  ----------------------------------------
compare_fs <- bench::press(number_of_rows = c(1E3, 1E4, 1E5, 1E6, 1E7, 1E8),
                           {
                             fake_data <- create_fake_data(number_of_rows)
                             bench::mark(
                               "dplyr" =
                                 fake_data %>%
                                 dplyr::filter(date > as.Date("1999-12-31")) %>%
                                 group_by(gender, some_letter) %>%
                                 summarise(
                                   mean_age = mean(some_number),
                                   sum_age = sum(some_number)
                                 ),

                               "dtplyr" =
                                 fake_data %>%
                                 lazy_dt() %>%
                                 dplyr::filter(date > as.Date("1999-12-31")) %>%
                                 group_by(gender, some_letter) %>%
                                 summarise(
                                   mean_age = mean(some_number),
                                   sum_age = sum(some_number)
                                 ) %>%
                                 as_tibble(),
                               "data.table" =
                                 as.data.table(fake_data)[date > as.Date("1999-12-31"),
                                                          .(mean_age = mean(some_number),
                                                          sum_age = sum(some_number)),
                                                          by = .(gender, some_letter)],
                               "tidydt" =
                                 fake_data %>%
                                 as_dt() %>%
                                 dt_filter(date > as.Date("1999-12-31")) %>%
                                 dt_summarise(
                                   mean_age = mean(some_number),
                                   sum_age = sum(some_number),
                                   by = list(gender, some_letter)
                                 )
                             )
                           })
compare_fs

#plot
plot_fs <- autoplot(compare_fs) +
  labs(title = "filter & summarize")


## microbenching case_when() & equiv  ---------------------------------------
compare_cw <- bench::press(number_of_rows = c(1E3, 1E4, 1E5, 1E6, 1E7, 1E8),
                           {
                             fake_data <- create_fake_data(number_of_rows)
                             bench::mark(
                               "dplyr" =
                                 fake_data %>%
                                 mutate(
                                   new_variable = case_when(
                                     some_letter == "a" ~ "d",
                                     some_letter == "b" ~ "e",
                                     some_letter == "c" ~ "f",
                                     TRUE ~ some_letter
                                   )
                                 ) %>%
                                 group_by(new_variable) %>%
                                 count(),
                               "dtplyr" =
                                 fake_data %>%
                                 lazy_dt() %>%
                                 mutate(
                                   new_variable = case_when(
                                     some_letter == "a" ~ "d",
                                     some_letter == "b" ~ "e",
                                     some_letter == "c" ~ "f",
                                     TRUE ~ some_letter
                                   )
                                 ) %>%
                                 group_by(new_variable) %>%
                                 count() %>%
                                 as_tibble(),
                               "data.table-fifelse" =
                                 as.data.table(fake_data)[, new_variable := fifelse(some_letter == "a", "d", fifelse(some_letter == "b", "e", "f"))][, .(n = .N), by = new_variable],
                               "data.table-match" =
                                 as.data.table(fake_data)[, new_variable := c("d", "e", "f")[match(some_letter, c("a", "b", "c"))]][, .(n = .N), by = new_variable],
                               "tidyfast" =
                                 fake_data %>%
                                 mutate(
                                   new_variable = dt_case_when(
                                     some_letter == "a" ~ "d",
                                     some_letter == "b" ~ "e",
                                     some_letter == "c" ~ "f",
                                     TRUE ~ some_letter
                                   )
                                 ) %>%
                                 group_by(new_variable) %>%
                                 count()
                             )
                           })
compare_cw

#plot
plot_cw <- autoplot(compare_cw) +
  labs(title = "case_when() & equiv")


## save plots -----------------------------------------------------------
ggsave("out/dt_pkgs_filter_summarise.png", plot_fs)
ggsave("out/dt_pkgs_case_when.png", plot_cw)







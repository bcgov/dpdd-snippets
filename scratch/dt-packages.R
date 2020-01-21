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
library(tictoc) #timing
library(data.table)
library(dtplyr)
library(tidyfast) #https://github.com/TysonStanley/tidyfast
library(tidydt) #https://github.com/markfairbanks/tidydt
library(conflicted)
library(microbenchmark)
library(ggplot2)
library(purrr)
library(patchwork)

# conflict_prefer("filter", "dplyr")

## COMPARE SPEED of DT & PACKAGES

## FILTER & SUMMARISE  ------------------------------------------------------

## data sizes to test
sizes <- c(1E3, 1E4, 1E5, 1E6) #, 1E7, 1E8)
names <- c("1 x thousand:\n1filter/summarise", "10 x thousand:\n1filter/summarise",
           "100 x thousand:\n1filter/summarise", "1 x million:\n1filter/summarise")
           # "10 x million:\n1filter/summarise", "100 x million:\n1filter/summarise")
df <- data.frame(sizes, names)

## map over data sizes df
compare1 <- map2(.x = df$sizes, .y = df$names, ~{

## make a fake, biggish data data frame
number_of_rows <- .x #increase this to really test things
# number_of_rows <- 1E3

fake_data <- data.frame(sample(1:100, number_of_rows, replace = TRUE),
                       sample(seq(as.Date('1990/01/01'), as.Date('2010/01/01'), by = "day"),
                              number_of_rows, replace = TRUE),
                       sample(c("male", "female", "unknown"), number_of_rows, replace = TRUE),
                       sample(c("a", "b", "c"), number_of_rows, replace = TRUE))

colnames(fake_data) <- c("some_number", "date", "gender", "some_letter")

fake_data <- fake_data %>%
  mutate(some_letter = as.character(some_letter),
         gender = as.character(gender))

## make dtplyr and data.table and other data objects
fake_data_dtplyr <- lazy_dt(fake_data) #dtplyr
fake_data_dt <- as.data.table(fake_data) #data.table
fake_data_tidydt <- as_dt(fake_data) #tidydt

## microbenching filter & summarise
compare_filter_summarize <- microbenchmark("dplyr" = {
  fake_data %>%
    dplyr::filter(date > as.Date("1999-12-31")) %>%
    group_by(gender, some_letter) %>%
    summarise(mean_age = mean(some_number),
              sum_age = sum(some_number))
},
"dtplyr" = {
  fake_data_dtplyr %>%
    dplyr::filter(date > as.Date("1999-12-31")) %>%
    group_by(gender, some_letter) %>%
    summarise(mean_age = mean(some_number),
              sum_age = sum(some_number)) %>%
  as_tibble()
},
"data.table" = {
  fake_data_dt[date > as.Date("1999-12-31"), .(mean_age = mean(some_number),
    sum_age = sum(some_number)), by = .(gender, some_letter)]
},
"tidydt" = {
  fake_data_tidydt %>%
  dt_filter(date > as.Date("1999-12-31")) %>%
  dt_summarise(mean_age = mean(some_number), sum_age = sum(some_number),
               by = list(gender, some_letter))
})
compare_filter_summarize

#plot speeds
.y <- autoplot(compare_filter_summarize) +
  labs(title = .y)
})



## CASE_WHEN()  --------------------------------------------------------------

## data sizes to test
sizes <- c(1E3, 1E4, 1E5, 1E6) #, 1E7, 1E8)
names <- c("1 x thousand:\ncase_when", "10 x thousand:\ncase_when",
           "100 x thousand:\ncase_when", "1 x million:\ncase_when")
           # "10 x million:\ncase_when", "100 x million:\ncase_when")
df <- data.frame(sizes, names)

## map over data sizes df
compare2 <- map2(.x = df$sizes, .y = df$names, ~{

## make a fake, biggish data data frame
number_of_rows <- .x #increase this to really test things
# number_of_rows <- 1E3

fake_data <- data.frame(sample(1:100, number_of_rows, replace = TRUE),
                       sample(seq(as.Date('1990/01/01'), as.Date('2010/01/01'), by = "day"),
                              number_of_rows, replace = TRUE),
                       sample(c("male", "female", "unknown"), number_of_rows, replace = TRUE),
                       sample(c("a", "b", "c"), number_of_rows, replace = TRUE))

colnames(fake_data) <- c("some_number", "date", "gender", "some_letter")

fake_data <- fake_data %>%
  mutate(some_letter = as.character(some_letter),
         gender = as.character(gender))

## make dtplyr and data.table and other data objects
fake_data_dtplyr <- lazy_dt(fake_data) #dtplyr
fake_data_dt <- as.data.table(fake_data) #data.table

## microbenching case_when()
compare_case_when <- microbenchmark("dplyr" = {
 fake_data %>%
    mutate(new_variable = case_when(some_letter == "a" ~ "d",
                                    some_letter == "b" ~ "e",
                                    some_letter == "c" ~ "f",
                                    TRUE ~ some_letter)) %>%
    group_by(new_variable) %>%
    count()
},
"dtplyr" = {
 fake_data_dtplyr %>%
   mutate(new_variable = case_when(some_letter == "a" ~ "d",
                                    some_letter == "b" ~ "e",
                                    some_letter == "c" ~ "f",
                                    TRUE ~ some_letter)) %>%
    group_by(new_variable) %>%
    count() %>%
    as_tibble()
},
"data.table-match" = {
fake_data_dt[, new_variable := c("d", "e", "f")[match(some_letter, c("a", "b", "c"))]][, .(n = .N), by = new_variable]
},
"data.table-fifelse" = {
fake_data_dt[, new_variable := fifelse(some_letter == "a", "d", fifelse(some_letter == "b", "e", "f"))][, .(n = .N), by = new_variable]
},
"tidyfast" = {
 fake_data %>%
   mutate(new_variable = dt_case_when(some_letter == "a" ~ "d",
                                    some_letter == "b" ~ "e",
                                    some_letter == "c" ~ "f",
                                    TRUE ~ some_letter)) %>%
    group_by(new_variable) %>%
    count()
})
compare_case_when

#plot speeds
.y <- autoplot(compare_case_when, log = FALSE) +
  labs(title = .y)
})


## multiplot and save plots ------------------------------------------------
plot <- compare1[[1]] + compare1[[2]] + compare1[[3]] +
        compare1[[4]] #+ compare1[[5]] + compare1[[6]]

plot2 <- compare2[[1]] + compare2[[2]] + compare2[[3]] +
        compare2[[4]] #+ compare2[[5]] + compare2[[6]]

ggsave("out/dt_pkgs_filter_summarise.png", plot)
ggsave("out/dt_pkgs_case_when.png", plot2)



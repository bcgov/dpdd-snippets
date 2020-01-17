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


## Load libraries
library(dplyr)
library(tictoc) #timing
library(data.table)
library(dtplyr)
library(tidyfast) #https://github.com/TysonStanley/tidyfast
library(tidydt) #https://github.com/markfairbanks/tidydt
library(conflicted)


## make some fake, biggish data
number_of_rows <- 1E8 ## increase this to really test things

fake_data <- data.frame(sample(1:100, number_of_rows, replace = TRUE),
                       sample(seq(as.Date('1990/01/01'), as.Date('2010/01/01'), by = "day"),
                              number_of_rows, replace = TRUE),
                       sample(c("male", "female", "unknown"), number_of_rows, replace = TRUE),
                       sample(c("a", "b", "c"), number_of_rows, replace = TRUE))

colnames(fake_data) <- c("some_number", "date", "some_letter", "gender")


## in memory dplyr test ----------------------------------------------------
tic()
fake_data %>%
  dplyr::filter(date > as.Date("1999-12-31")) %>%
  group_by(gender, some_letter) %>%
  summarise(mean_age = mean(some_number), sum_age = sum(some_number))
toc()


## in memory dtplyr test ----------------------------------------------------
tic()
fake_data %>%
  dplyr::filter(date > as.Date("1999-12-31")) %>%
  group_by(gender, some_letter) %>%
  summarise(mean_age = mean(some_number), sum_age = sum(some_number))
toc()

## in memory data.table test -----------------------------------------------

fake_data_dt = as.data.table(fake_data)

tic()
fake_data_dt[date > as.Date("1999-12-31"), mean_age = mean(some_number),
             sum_age = sum(some_number), by = gender, some_number]
toc()

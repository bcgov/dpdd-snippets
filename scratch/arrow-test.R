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

## get arrow v0.16 ------------------------------------------------------------
# options(repos = c(
#   "https://dl.bintray.com/ursalabs/arrow-r",
#   getOption("repos")
#   ))
# install.packages("arrow")
#should be on CRAN in days?


## load libraries ------------------------------------------------------------
library(dplyr)
library(arrow)
library(tictoc)


##  make some fake data
# create_fake_data <- function(number_of_rows) {
# fd <-   data.frame(sample(1:100, number_of_rows, replace = TRUE),
#                        sample(seq(as.Date('1990/01/01'), as.Date('2010/01/01'),
#                                   by = "day"), number_of_rows, replace = TRUE),
#                        sample(c("male", "female", "unknown"), number_of_rows,
#                               replace = TRUE),
#                        sample(c("a", "b", "c"), number_of_rows, replace = TRUE),
#                    stringsAsFactors = FALSE)
#
# colnames(fd) <- c("some_number", "date", "gender", "some_letter")
# fd
# }
#
# fd <- create_fake_data(1E3)
#
# write_parquet(fd, "data/one_hundred_million_rows3.parquet")


## arrow ------------------------------------------------------------

ds <- open_dataset("data")

tic()
ds %>%
  filter(gender != "unknown",
         # date > as.Date("2004-12-31"), #crashing rstudio
         some_letter == "c") %>%
  group_by(gender) %>%
  collect() %>%
  summarize(mean_numner = mean(some_number),
            sum_number = sum(some_number))

toc()

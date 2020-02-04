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
# install.packages("arrow", repos = "https://dl.bintray.com/ursalabs/arrow-r")

#should be on CRAN in days?


## load libraries ------------------------------------------------------------
library(dplyr)
library(arrow)
library(tictoc)


## fake data -----------------------------------------------------------------

##  make some fake data
# create_fake_data <- function(number_of_rows) {
# fd <-   data.frame(sample(1:100, number_of_rows, replace = TRUE),
#                        sample(seq(as.Date('1990-01-01'), as.Date('2010-01-01'),
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
# fd <- create_fake_data(1E8)
#
# write_parquet(fd, "data/one_hundred_million_rows3.parquet")



## arrow ------------------------------------------------------------

ds <- open_dataset("data")

tic()
ds %>%
  dplyr::filter(gender != "unknown",
         # date > as.Date("2000-01-01"), #crashing rstudio
         some_letter == "c") %>%
  select(gender, some_number) %>%
  group_by(gender) %>%
  collect() %>%
  summarise(mean_number = mean(some_number),
            sum_number = sum(some_number))
toc()




## reprex  ------------------------------------------------------------

# install.packages("arrow", repos = "https://dl.bintray.com/ursalabs/arrow-r")
library(dplyr)
library(arrow) #arrow v0.16
library(here)

if(!exists(here("tmp"))) dir.create(here("tmp"))

# use shipped parquet data, add date column and write to tmp folder
read_parquet(system.file("v0.7.1.parquet", package="arrow")) %>%
  dplyr::mutate(date = sample(seq(as.Date('1990-01-01'), as.Date('2010-01-01'),
                                   by = "day"), 10, replace = TRUE)) %>%
  write_parquet(here("tmp/parquet_with_date_col.parquet"))


# read and filter file from disk
read_parquet(here("tmp/parquet_with_date_col.parquet")) %>%
  dplyr::filter(date > as.Date("2000-01-01"))


# creat dataset object, filter and collect result
ds <- open_dataset(here("tmp"))

# crashes RStudio with collect()
ds %>%
  dplyr::filter(date > as.Date("2000-01-01")) %>%
  collect()



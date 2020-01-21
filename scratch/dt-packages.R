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

sizes <- c(1E3, 1E4, 1E5, 1E6, 1E7, 1E8)
names <- c("1 x thousand", "10 x thousand", "100 x thousand",
           "1 x million", "10 x million", "100 x million")
df <- data.frame(sizes, names)

compare1 <- map2(.x = df$sizes, .y = df$names, ~{

## make a fake, biggish data data frame --------------------------------------
number_of_rows <- .x #increase this to really test things

fake_data <- data.frame(sample(1:100, number_of_rows, replace = TRUE),
                       sample(seq(as.Date('1990/01/01'), as.Date('2010/01/01'), by = "day"),
                              number_of_rows, replace = TRUE),
                       sample(c("male", "female", "unknown"), number_of_rows, replace = TRUE),
                       sample(c("a", "b", "c"), number_of_rows, replace = TRUE))

colnames(fake_data) <- c("some_number", "date", "some_letter", "gender")


## make dtplyr and data.table and other data objects ------------------------
fake_data_dtplyr <- lazy_dt(fake_data) #dtplyr
fake_data_dt <- as.data.table(fake_data) #data.table
fake_data_tidydt <- as_dt(fake_data) #tidydt


## microbenching filter & summarise -----------------------------------------

#filter & summarizing
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
.y <- autoplot(compare_filter_summarize, log = FALSE) +
  labs(title = .y)
})


#compare plots
plot <- compare1[[1]] + compare1[[2]] + compare1[[3]] +
        compare1[[4]] + compare1[[5]] + compare1[[6]]

ggsave("out/dt-packages_nolog.png", plot)



## microbenching case_when ---------------------------------------------------
compare_case_when <- microbenchmark("dplyr" = {

},
"dtplyr" = {

},
"data.table" = {

},
"tidydt" = {

})
compare_case_when

#plot speeds
autoplot(compare_case_when)




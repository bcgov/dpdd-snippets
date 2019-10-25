# Copyright 2019 Province of British Columbia
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


#This script is for examining trends and patterns in open
#Vancouver Police Department (VPD) crime data.


#Load libraries
library(here)
#library(tidyverse)
library(colorRamps)
library(ggplot2)

#Source functions
source(here("yvr-crime/R/functions.R"))


#Initial priming and data wrangling:
#Make True to Prime, Leave False to skip
env_prime = T
if(env_prime){
  #update file path to find crime data
  dat = read.csv('yvr-crime/data/crimedata_csv_all_years.csv')
  #NOTE1: HUNDRED_BLOCK = X NK_LOC ST is default location value used for incidents
  # with unknown location and is geolocated to 312 Main Street
  # which is X = 492757.5, Y = 5458792
  # The number of instances in which this occurs is 2476/619474 ~ 0.4%, and is mostly prior to 2006.
  # Because all of this analysis depends on location and region, they are removed.
  # Run next two comments to verify the above.
  # length(dat[dat[,"HUNDRED_BLOCK"]=="X NK_LOC ST",1])
  # table(dat[dat[,"HUNDRED_BLOCK"]=="X NK_LOC ST",c("TYPE", "YEAR")])
  dat = dat[dat[,"HUNDRED_BLOCK"]!="X NK_LOC ST",]
  #NOTE2: There are many instances (~10%) which have no location or
  # neighbourhoood data due to privacy. All instances of these types have no location data.
  # They are removed for now, unless a better handling method is thought of for the future.
  # Run the next comment line to see this table
  #table(dat[dat[,"NEIGHBOURHOOD"]=="",c("YEAR", "TYPE")])
  dat = dat[dat[,"NEIGHBOURHOOD"]!="",]
  CATEGORY = rep(0, times=length(dat[,1]))
  dat = cbind(dat, CATEGORY)
  dat[which(dat[,'NEIGHBOURHOOD']%in%c('Strathcona', 'Central Business District')),'CATEGORY'] = 'DTES'
  dat[which(!(dat[,'NEIGHBOURHOOD']%in%c('Strathcona', 'Central Business District'))),'CATEGORY'] = 'Not DTES'
}

#Plot a count of crimes over time for each neighbourhood
#It is important to note that using count data here when including the incomplete
#2019 year results in a false drop for 2019 in frequency plots
ggplot(data=dat)+
  geom_line(mapping = aes(x = YEAR, color = NEIGHBOURHOOD), stat = 'bin', binwidth = 1)+
  ggtitle("Number of Recorded Crime Instances by Neighbourhood Over Time")+
  xlab("Year")+
  ylab("Count")

#Plot - group the central business district and strathcona into one group and all
#other regions into another to compare the trends.
ggplot(data=dat)+
  geom_bar(mapping = aes(x = YEAR, color = CATEGORY), binwidth = 1)


#Plot by region
ggplot(data = as.data.frame(as_proportion(dat)))+
  geom_line(mapping = aes(x = YEAR, y = Freq, group = NEIGHBOURHOOD, color = NEIGHBOURHOOD))+
  ggtitle("Proportion of Recorded Crime Instances in Each Neighbourhood Over Time")+
  xlab("Year")+
  ylab("Proportion")

#Plot - add another catagory for to give an example of an alternate grouping as follows.
ggplot(data = as.data.frame(as_proportion(data=dat, groups='CATEGORY')))+
  geom_line(mapping = aes(x = YEAR, y = Freq, group = CATEGORY, color = CATEGORY))+
  ggtitle("Proportion of Recorded Crime Instances In Regions Intersecting DTES Over Time")+
  xlab('Year')+
  ylab('Proportion')

#Plot - map all of the X, Y coordinates in the remaining data
ggplot(data = dat[dat[,'X']>480000&dat[,"X"]<500000,])+
  geom_point(mapping = aes(x = X, y= Y))

#Plot - heat map
ggplot(data = dat[dat[,'X']>480000&dat[,"X"]<500000,])+
  stat_bin2d(mapping = aes(x = X, y = Y), bins=150)+
  scale_fill_gradientn(colours=colorRamps::matlab.like2(50))

#Plot - coordinates
ggplot(data = dat[dat[,'X']>491300&dat[,"X"]<491500&dat[,"Y"]<5459000&dat[,"Y"]>5458900,])+
  stat_bin2d(mapping = aes(x = X, y = Y), bins=150)+
  scale_fill_gradientn(colours=colorRamps::matlab.like2(50))
dat[dat[,'X']>491300&dat[,"X"]<491500&dat[,"Y"]<5459000&dat[,"Y"]>5458900,]

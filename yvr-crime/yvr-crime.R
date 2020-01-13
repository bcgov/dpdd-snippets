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
  library(tidyverse)
  library(colorRamps)
  source('functions.R')
  #update file path to find crime data
  dat = read.csv('yvr-crime/data/crimedata_csv_all_years.csv')
  #NOTE1: HUNDRED_BLOCK = X NK_LOC ST is default location value used for incidents with unknown location and is geolocated to 312 Main Street
  # which is X = 492757.5, Y = 5458792
  # The number of instances in which this occurs is ~ 0.4%, and is mostly prior to 2006.
  # Because all of this analysis depends on location and region, they are removed. Run next two comments to verify the above.
  # length(dat[dat[,"HUNDRED_BLOCK"]=="X NK_LOC ST",1])
  # table(dat[dat[,"HUNDRED_BLOCK"]=="X NK_LOC ST",c("TYPE", "YEAR")])
  dat = dat[dat[,"HUNDRED_BLOCK"]!="X NK_LOC ST",]
  #NOTE2: There are ~10% remaining instances which have no location or neighbourhoood data due to privacy, mostly of types "Offence Against a Person"
  # and "Homicide". In fact, all instances of these types have no location data. Unfortunately, these types are likely highly relavent to the analysis of the DTES,
  #but the policy is to withhold this information for these types. They are removed for now, unless a better handling method is thought of for the future.
  #Run the next comment line to see this table
  #table(dat[dat[,"NEIGHBOURHOOD"]=="",c("YEAR", "TYPE")])
  dat = dat[dat[,"NEIGHBOURHOOD"]!="",]
  CATEGORY = rep(0, times=length(dat[,1]))
  dat = cbind(dat, CATEGORY)
  #ESTIMATED DEFINITION FOR DTES
  x_min = 491900 #Approx. x-coord. of Homer St. @ Pender
  x_max = 493360 #Approx. x-coord. of Princess Ave.
  y_min = 5458630 #Approx. y-coord of Pender St.
  y_max = 5458950 #Approx. y-coord of Powell St.
  dat[dat[,"NEIGHBOURHOOD"]=="Central Business District", "CATEGORY"] = "CBD excluding DTES"
  dat[dat[,"NEIGHBOURHOOD"]=="Strathcona", "CATEGORY"] = "Strathcona excluding DTES"
  dat[!(dat[,"CATEGORY"]%in%c("CBD excluding DTES","Strathcona excluding DTES")), "CATEGORY"] = "All Else"
  dat[dat[,'X']>x_min&dat[,"X"]<x_max&dat[,"Y"]<y_max&dat[,"Y"]>y_min, "CATEGORY"] = 'DTES'
  #dat[!(dat[,'X']>x_min&dat[,"X"]<x_max&dat[,"Y"]<y_max&dat[,"Y"]>y_min), "CATEGORY"] = 'Not DTES'
}

#Plot a count of crimes over time for each neighbourhood
#It is important to note that using count data here when including the incomplete
#2019 year results in a false drop for 2019 in frequency plots
ggplot(data=dat)+
  geom_line(mapping = aes(x = YEAR, color = NEIGHBOURHOOD), stat = 'bin', binwidth = 1)+
  ggtitle("Number of Recorded Crime Instances by Neighbourhood Over Time")+
  xlab("Year")+
  ylab("Count")

#Plot - the same count as a stacked barchart showing that all regions, including CBD, experience the same overall trend
ggplot(data=dat)+
  geom_bar(mapping = aes(x = YEAR, color = NEIGHBOURHOOD), binwidth = 1)


#Plot - proportion of total by neightbourhood
ggplot(data = as.data.frame(as_proportion(dat)))+
  geom_line(mapping = aes(x = YEAR, y = Freq, group = NEIGHBOURHOOD, color = NEIGHBOURHOOD))+
  ggtitle("Proportion of Recorded Crime Instances in Each Neighbourhood Over Time")+
  xlab("Year")+
  ylab("Proportion")

#Heatmap plot - all yvr-crime: shows a lot of density in CBD outside of the DTES
ggplot(data = dat[dat[,'X']>480000&dat[,"X"]<500000,])+
  stat_bin2d(mapping = aes(x = X, y = Y), bins=150)+
  scale_fill_gradientn(colours=colorRamps::matlab.like2(50))

#Heat-map plot - the DTES area, as defined by the boundaries added to the env_prime. (top of page)
ggplot(data = dat[dat$CATEGORY=="DTES",])+
  stat_bin2d(mapping = aes(x = X, y = Y), bins = 100)+
  scale_fill_gradientn(colours=colorRamps::matlab.like2(50))

#plot - proportion plot as before, but using an alternate grouping isolating the DTES as in the previous heatmap
ggplot(data = as.data.frame(as_proportion(data=dat[dat[,"YEAR"]>2011,], groups='CATEGORY')))+
  geom_line(mapping = aes(x = YEAR, y = Freq, group = CATEGORY, color = CATEGORY))+
  ggtitle("Proportion of Recorded Crime Instances In Regions Over Time")+
  xlab('Year')+
  ylab('Proportion')+
  scale_y_continuous(breaks = seq(0,1,0.05))

#Values for the trends above can be checked by running:
as_proportion(data=dat[dat[,"YEAR"]>2011,], groups='CATEGORY')

#plot - Comparing the proportion of different types of crime in each area
ggplot(data=dat)+
  geom_bar(mapping = aes(x = CATEGORY, fill = TYPE), position = 'fill')+
  ggtitle("Proportion of Crime in Each Area By Type")+
  ylab("Proportion")+
  xlab("Region")

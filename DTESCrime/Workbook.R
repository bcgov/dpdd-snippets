#This script is a workbook for producing plots to investigate the crime problem in the downtown east side (DTES) of Vancouver
#as stated in the article : https://www.cbc.ca/news/canada/british-columbia/dtes-vancouver-statistics-anecdotes-1.5253897
#Made by Graham Quee, 2019.10.16
#Last Editted by Graham Quee 2019.10.25 

#Initial priming and data wrangling:
#Make True to Prime, Leave False to skip
env_prime = T
if(env_prime){
  library(tidyverse)
  library(colorRamps)
  source('functions.R')
  #update file path to find crime data
  dat = read.csv('crimedata_csv_all_years.csv')
  #NOTE1: HUNDRED_BLOCK = X NK_LOC ST is default location value used for incidents with unknown location and is geolocated to 312 Main Street
  # which is X = 492757.5, Y = 5458792
  # The number of instances in which this occurs is 2476/619474 ~ 0.4%, and is mostly prior to 2006.
  # Because all of this analysis depends on location and region, they are removed. Run next two comments to verify the above.
  # length(dat[dat[,"HUNDRED_BLOCK"]=="X NK_LOC ST",1])
  # table(dat[dat[,"HUNDRED_BLOCK"]=="X NK_LOC ST",c("TYPE", "YEAR")])
  dat = dat[dat[,"HUNDRED_BLOCK"]!="X NK_LOC ST",]
  #NOTE2: There are 62098 remaining instances (~10%) which have no location or neighbourhoood data due to privacy, mostly of types "Offence Against a Person"
  # and "Homicide". In fact, all instances of these types have no location data. Unfortunately, these types are likely highly relavent to the analysis of the DTES,
  #but the policy is to withhold this information for these types. They are removed for now, unless a better handling method is thought of for the future.
  #Run the next comment line to see this table
  #table(dat[dat[,"NEIGHBOURHOOD"]=="",c("YEAR", "TYPE")])
  dat = dat[dat[,"NEIGHBOURHOOD"]!="",]
  CATEGORY = rep(0, times=length(dat[,1]))
  dat = cbind(dat, CATEGORY)
  dat[which(dat[,'NEIGHBOURHOOD']%in%c('Strathcona', 'Central Business District')),'CATEGORY'] = 'DTES'
  dat[which(!(dat[,'NEIGHBOURHOOD']%in%c('Strathcona', 'Central Business District'))),'CATEGORY'] = 'Not DTES'
}

#Plot 1 is a count of crimes over time for each neighbourhood, of which the central business district (CBD) is a clear outlier
#It is important to note that using count data here when including the incomplete 2019 year results in a false drop for 2019 in frequency plots
ggplot(data=dat)+
  geom_line(mapping = aes(x = YEAR, color = NEIGHBOURHOOD), stat = 'bin', binwidth = 1)+
  ggtitle("Number of Recorded Crime Instances by Neighbourhood Over Time")+
  xlab("Year")+
  ylab("Count")

#Plot 2 groups the central business district and strathcona (which are mentioned in the article to be the
#two districts which intersect the DTES) into one group and all other regions into another to compare the trends.
#Though it does appear that these two combined regions do experience a larger increase in crime frequency near the end compared to the
#soft increase for the total of the non-DTES group, it is not exactly extreme. Poking around in the two groups leads me to believe that
#both the number of instances and increase in instances is much less dramatic for strathcona then for the CBD. If you look at the map at..
# https://geodash.vpd.ca/Html5Viewer/?disclaimer=on&viewer=VPDPublicRefresh_gvh&x=93&y=56
# (protip: click layers and hide crime instances and traffic cameras)
#... it may explain why. Though the strathcona region does intersect the DTES (hastings and main is on the boundary), it does not extend east of
#oppenhiemer park nor south of Pender. So the proportion of strathcona that is really DTES is tiny compared to CBD, which is basically all DTES.
#This poses a problem for grouping different regions like this. 
ggplot(data=dat)+
  geom_bar(mapping = aes(x = YEAR, color = CATEGORY), binwidth = 1)

#I've spent a good chunk of time looking for some population data, but nothing I have found divides Vancouver into similar regions the police use.
#However, I think there is a better way of looking at this in terms of the actual assertion made in the article. If we calculate the proportion
# of instances that occur in a single region out of the total, then it automatically controls for any fluctuation that occurs in the total crime of Vancouver 
# regardless of its cause, i.e. population changes, global enforcement changes. These values should be stable unless an area is actually getting worse by 
#comparison to all other regions. I've written a function defined in functions.R which results in a proportion table for any grouping we want,
#and can be reverted to a df using as.data.frame().

#The default case is by region which is done here
# Again, the trend for the CBD region is pretty damning, but I don't see an appreciable change in strathcona, meaning that the recent increase we see
#in strathcona in the total counts is likely not significantly different from the overall increase. This overall increase is likely due to something
#like population increase as we expected, so it would still be good to get some population data in the mix to at least validate that for the totals.
#The biggest thing we cannot control for in this data is increased inforcement in a specific group. Since these are records of interactions between 
#a crime instance and inforcement and not just crime, these trends are mass action and could fluctuate with either.
ggplot(data = as.data.frame(as_proportion(dat)))+
  geom_line(mapping = aes(x = YEAR, y = Freq, group = NEIGHBOURHOOD, color = NEIGHBOURHOOD))+
  ggtitle("Proportion of Recorded Crime Instances in Each Neighbourhood Over Time")+
  xlab("Year")+
  ylab("Proportion")

#In the env_prime, I have added another catagory for the two region DTES from the article to give an example of an alternate grouping as follows.
#Here we can see that even when including strathcona in the DTES, these two regions do experience a marked increase in the proportion of crime against
#the non-DTES group.
ggplot(data = as.data.frame(as_proportion(data=dat, groups='CATEGORY')))+
  geom_line(mapping = aes(x = YEAR, y = Freq, group = CATEGORY, color = CATEGORY))+
  ggtitle("Proportion of Recorded Crime Instances In Regions Intersecting DTES Over Time")+
  xlab('Year')+
  ylab('Proportion')

#This plot maps all of the X, Y coordinates in the remaining data
ggplot(data = dat[dat[,'X']>480000&dat[,"X"]<500000,])+
  geom_point(mapping = aes(x = X, y= Y))

#I would like to turn the above into a heat map, as the below does, but the results are much less clear than I thought. The largest contributor
#is not even in the DTES, and is largest by a lot, which makes me think that there is another aggregation point caused by fudging the locations.
#I'm trying to find out what that is now.
ggplot(data = dat[dat[,'X']>480000&dat[,"X"]<500000,])+
  stat_bin2d(mapping = aes(x = X, y = Y), bins=150)+
  scale_fill_gradientn(colours=colorRamps::matlab.like2(50))

#SO. It turns out that what I've said above is mostly not correct in terms of its relation to the DTES. I had not noticed that in the crime map,
#(linked above). There are actually two seperated regions that are side by side, both called the central buisiness district. The claim that either
#the CBD or Strathcona regions are representative of the DTES is completely false, because they are way to large. The CBD specifically is most of DT,
#and the heat map is showing that a way larger contribution to these instances comes from the Granville and Georgia area. (seen in the plot below and corresponding)
#dataframe. I think the best way to proceed is to get an educated opinion on what the boundaries of the DTES are in terms of coordinates to do this seperation,
#and ideally to try and obtain data from the VPD that includes location data on that excluded 10% (no idea how hard that would be).
#I will actually be continuing to work on this from both sides (IDD and corrections) because Leigh has just been contacted to provide her input as well.
ggplot(data = dat[dat[,'X']>491300&dat[,"X"]<491500&dat[,"Y"]<5459000&dat[,"Y"]>5458900,])+
  stat_bin2d(mapping = aes(x = X, y = Y), bins=150)+
  scale_fill_gradientn(colours=colorRamps::matlab.like2(50))
dat[dat[,'X']>491300&dat[,"X"]<491500&dat[,"Y"]<5459000&dat[,"Y"]>5458900,]

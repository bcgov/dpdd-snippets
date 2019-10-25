#generate proportion plots for any grouping on the Vancouver police open crime dataset
#data =  a dataframe of crime data including "YEAR" and grouping labels
#groups =  the string used for the header of the grouping labels
#written by Graham Quee, 2019.10.17

as_proportion = function(data, groups = "NEIGHBOURHOOD"){
  #strip data down to year and group column and generate a table
  prop = data[,c(groups, "YEAR")]
  prop = table(prop)

  #turn the table into a proportion out of the totals by calculating the sum for each year across group and dividing the values for that year
totals = colSums(prop)
for(i in 1:ncol(prop)){
  prop[,i] = prop[,i]/totals[i]
}
prop
}



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



#generate proportion plots for any grouping on the Vancouver police open crime dataset
#data =  a dataframe of crime data including "YEAR" and grouping labels
#groups =  the string used for the header of the grouping labels
as_proportion = function(data, groups = "NEIGHBOURHOOD"){
  #strip data down to year and group column and generate a table
  prop = data[,c(groups, "YEAR")]
  prop = table(prop)

  #turn the table into a proportion out of the totals by calculating the sum for
  #each year across group and dividing the values for that year
totals = colSums(prop)
for(i in 1:ncol(prop)){
  prop[,i] = prop[,i]/totals[i]
}
prop
}



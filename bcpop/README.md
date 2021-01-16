<!--
Copyright 2021 Province of British Columbia

Licensed under the Apache License, Version 2.0 (the "License");
you may not use this file except in compliance with the License.
You may obtain a copy of the License at

http://www.apache.org/licenses/LICENSE-2.0

Unless required by applicable law or agreed to in writing, software distributed under the License is distributed on an "AS IS" BASIS,
WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
See the License for the specific language governing permissions and limitations under the License.
-->

[![img](https://img.shields.io/badge/Lifecycle-Maturing-007EC6)](https://github.com/bcgov/repomountie/blob/master/doc/lifecycle-badges.md)

### B.C. Population Estimates by Administrative Geospatial Areas

This folder contains an R script for looking at and comparing population size and demographics among administrative geospatial units in British Columbia.

#### Data

 - B.C. population estimates by Local Health Authority were sourced manually from BCStats ShinyApp [https://bcstats.shinyapps.io/popApp/](https://bcstats.shinyapps.io/popApp/) and saved locally in `/data`
 - B.C. population estimates by Census Subdivision were sourced from Statistics Canada ([Table: 17-10-0005-01](https://www150.statcan.gc.ca/t1/tbl1/en/tv.action?pid=1710000501&pickMembers%5B0%5D=1.11&pickMembers%5B1%5D=2.1&cubeTimeFrame.startYear=2010&cubeTimeFrame.endYear=2020&referencePeriods=20100101%2C20200101) using the [{cansim}](https://mountainmath.github.io/cansim/index.html) R package


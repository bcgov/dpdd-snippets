<!--
Copyright 2019 Province of British Columbia

Licensed under the Apache License, Version 2.0 (the "License");
you may not use this file except in compliance with the License.
You may obtain a copy of the License at

http://www.apache.org/licenses/LICENSE-2.0

Unless required by applicable law or agreed to in writing, software distributed under the License is distributed on an "AS IS" BASIS,
WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
See the License for the specific language governing permissions and limitations under the License.
-->

[![img](https://img.shields.io/badge/Lifecycle-Experimental-339999)](https://github.com/bcgov/repomountie/blob/master/doc/lifecycle-badges.md)

### Vancouver Police Department (VPD) Open Crime Data

This folder contains an R script for examining trends and patterns in open Vancouver Police Department (VPD) crime data.

The open Vancouver Police Department (VPD) crime data can be downloaded from the Vancouver Police Website at: https://geodash.vpd.ca/opendata/

The download will include a copy of the legal disclaimer and a discription of the dataset and its attributes, including the
the modifications made to the data for privacy reasons. 
A visualization of subsets of the data can be viewed on a map of Vancouver at: https://geodash.vpd.ca/

The analyis is in the `yvr-crime.R` file, and any reusable functions are sourced from `R/functions.R`.

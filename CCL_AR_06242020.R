#####################
# Comparative Competition Law Task
# Prep
# Created by: Anahi Rodriguez
# created on 19 June 2020
#####################

#clear environment
rm(list = ls())

#load packages
library(tidyverse)
library(haven)

#set wd
setwd("~/Google Drive/Master IPE Data/SUMMER 2020/rawdata")
list.files()

#load data
ccl <- read_dta("CCL_Law_Data_Ver1.dta")

#prep
ccl <- ccl %>%
  select(-code_cown, -code_cowc) %>% #remove cow variables
  mutate(year = as.numeric(year)) #make years numeric instead of integers

#load file paths


#Checks
#make sure data is in country year format 
names(ccl)
#make sure years are numeric
str(ccl$year)

#check for duplicates
n_occur <- data.frame(table(ccl$country, ccl$year))
print(n_occur[n_occur$Freq > 1,])


#appending IDs
#load the append_ids function
source(paste(prepscripts,"append_ids.R",sep=""))

#append Ids
ccl <- append_ids(ccl, breaks = F)

#every country recieved a code
#duplicates for Canada from 1969 - 2010, Germany for 1990, Yemen 1990, and Czechoslovakia 1992

#####remove all duplicates using filter based on one condition

#delete all region rows for canada 1969 - 2010
ccl_canada <- ccl %>%
  filter(ccl$country == "Canada") #look at all canada observations

#remove duplicate canada observations
ccl <- ccl %>%
  filter(ccl$countryname_raw != "CAN") #remove all canada observations that have CAN as region


#####removing duplicates for country year observations where two criteria are needed

#add a new column to use to remove duplicates 
ccl['for_duplicates'] = 0

#use these objects to decide which country year observation to remove
#look at all german observations for 1990
ccl_germany <- ccl %>%
  filter(ccl$country == "German Federal Republic" & ccl$year == 1990)
  #remove observation where countryname_raw == "Germany Federal Republic" because other observation
  #has data for a competition law coded in that year

#look at all Czechoslovakia observations for 1992
ccl_czec <- ccl %>%
  filter(ccl$country == "Czechoslovakia" & ccl$year == 1992)
  #remove observation where countryname_raw == "Czech Republic" because Czech republic did not exist till 1993

#set value to 1 for duplicates that need to be removed
ccl[5896,119] <- 1 #set Germany 1990 duplicate to 1 
ccl[6819,119] <- 1 #set Czechoslovakia 1992 duplicate to 1

#remove duplicates
ccl <- ccl %>%
  filter(for_duplicates != 1)

#delete for_duplicates column
ccl <- ccl %>%
  select(-for_duplicates)

####remove duplicates for country year observations with the same data

#use these objects to decide which country year observation to remove
#look at all Yemen observations for 1990
ccl_yemen <- ccl %>%
  filter(ccl$country == "Yemen (Arab Republic of Yemen)" & ccl$year == 1990)
  #remove either observation -- both have the same data for every variable
ccl <- ccl %>%
  distinct(year, country, .keep_all = TRUE)

#rerun append IDs
ccl <- append_ids(ccl, breaks = F)

#append suffix
ccl <- append_suffix(ccl, "CCL")
names(ccl)

#save prepped data
save(ccl, file = paste(preppeddata, "PREPPED_CCL_AR_06242020.rds"))




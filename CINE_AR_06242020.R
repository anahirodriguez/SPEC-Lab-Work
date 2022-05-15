#####################
# CINE Task
# Contract Intensive Economy measures
# Prep
# Created by: Anahi Rodriguez
# created on 12 June 2020
# edited on 16 June 2020
#####################

#clear environment
rm(list = ls())

#set wd
setwd("~/Google Drive/Master IPE Data/SUMMER 2020/rawdata/dataverse_files")
list.files()


#load packages
library(foreign)
library(dplyr)
library(tidyr)
library(Hmisc)
library(readxl)
library(haven)
library(labelled)

#load data
cine <- read_dta("CINEvMar2019b.dta")

#prep
#remove unecesarry variables
cine <- cine %>%
  select(-ccode, -version, country = "cname")

#add/remove labels
var_label(cine$country) <- NULL
var_label(cine$year) <- NULL

label(cine)

#load file paths

#Checks
#make sure data is in country year format 
names(cine)
#make sure years are numeric
str(cine$year)
#check for duplicates
n_occur <- data.frame(table(cine$country, cine$year))
print(n_occur[n_occur$Freq > 1,])
#no duplicates found

#appending IDs
#load the append_ids function
source(paste(prepscripts,"append_ids.R",sep=""))

#append Ids
cine <- append_ids(cine, breaks = F)
# "German Fed Rep" did not receive a gwno code, germanfedrep is not on ids list - germanyfedrep is!
# yemen is duplicated for year 1990


#rename german fed rep
cine$country[cine$country == "German Fed Rep"] <- "Germany Fed Rep"

#use these objects to decide which country year observation to remove
#look at all Yemen observations for 1990
cine_yemen <- cine %>%
  filter(cine$country == "Yemen (Arab Republic of Yemen)" & cine$year == 1990)
#remove Yemen duplicate observation where countryname_raw == "Yemen, Rep." for year 1990 because Yemen was 
#unified at the start of 1990

#will remove second observation for Yemen 1990
cine <- cine %>%
  distinct(year, country, .keep_all = TRUE)


#rerun append IDs
cine <- append_ids(cine, breaks = F)

#append suffix
cine <- append_suffix(cine, "CINE")
names(cine)

#save prepped data
save(cine, file = paste(preppeddata, "PREPPED_CINE_AR_06242020.rds"))


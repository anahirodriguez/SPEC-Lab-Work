#####################
# Updating regression table 2
# Federalism Paper
# Created by: Anahi Rodriguez
# 17 July 2020
# edited on 23 July 2020
#####################


#clear environment
rm(list = ls())

library(dplyr)
library(stargazer)
library(fastDummies)
library(foreign)
library(tidyr)
library(tidyverse)
library(naniar)
library(pglm)
library(lmtest)
library(haven)

#setwd
setwd("~/Google Drive/SPEC Powersharing S20/Merge and Clean Scripts/Data")


#load in data
load("/Users/anahi/Google Drive/SPEC Powersharing S20/Merge and Clean Scripts/Data/ merged_idccontrols_with_thetas_AR_07142020.RData")

#drop any observatiosn before the year 1975
merged <- merged %>%
  filter(year >= 1975)


#create year2, year2^2, and year2^3 variables
merged <- merged %>%
  mutate(year2 = year -1990, 
         yearsq = (year2)^2,
         yearcube = (year2)^3)

#arrange by gwno year
merged <- merged %>%
  arrange(gwno, year)


#create dummy variables for every gwno
merged <- merged %>%
  dummy_cols(select_columns ="gwno")


#create additive indices
merged <- merged %>%
  mutate(subpolicy2 = subtax + subed,
         subpolicy = subtax + subed + subpolice)


#still need civil conflict variable

#create some civil wr variables
#merged <- merged %>%
# mutate(civilwar = ) #which variables to use to create civilconflict variable


# /**************************
#   create some civil war variables
# ***************************/
#   gen civilwar = civwar
#   replace civilwar =ethwar if ethwar > civwar & ethwar != . 
#   
#   gen civilwar_bg = 0
#   replace civilwar_bg = 1 if civilwar > 3 & civilwar != .
#   
#   gen civilwarbg5 = (civilwar_bg==1 | L.civilwar_bg==1 | L2.civilwar_bg==1 | L3.civilwar_bg==1 | L4.civilwar_bg==1)
#   replace civilwarbg5 = . if civilwar_bg==. | L.civilwar_bg==. | L2.civilwar_bg==. | L3.civilwar_bg==. | L4.civilwar_bg==.
#   
#   gen civilwarbg10 = (civilwarbg5==1 | L.civilwarbg5==1 | L2.civilwarbg5==1 | L3.civilwarbg5==1 | L4.civilwarbg5==1)
#   replace civilwarbg10 = . if civilwarbg5==. | L.civilwarbg5==. | L2.civilwarbg5==. | L3.civilwarbg5==. | L4.civilwarbg5==.
#   
#   gen civilconflict = 0
#   replace civilconflict = 1 if civilwar > 0 & civilwar != . 
#   
#   gen civilconflictbg5 = (civilconflict==1 | L.civilconflict==1 | L2.civilconflict==1 | L3.civilconflict==1 | L4.civilconflict==1)
#   replace civilwarbg5 = . if civilconflict==. | L.civilconflict==. | L2.civilconflict==. | L3.civilconflict==. | L4.civilconflict==.
#   
###########

#control for missing years 
merged <- merged %>%
  group_by(gwno) %>% #selects by country
  tidyr::complete(year = full_seq(1975:2019, 1)) #completes the sequence of years from 1970 to 2019, adding rows with missing values for any missing year


#create 3 year lagged variables for powersharing variables
merged <- merged %>%
  group_by(gwno) %>% #groups by country
  arrange(desc(year),.by_group=TRUE) %>% #arrange by year in descending order
  mutate(F3.subtax = dplyr::lag(subtax, 3, default = NA),
         F3.subed = dplyr::lag(subed, 3, default = NA),
         F3.subpolice = dplyr::lag(subpolice, 3, default = NA)) %>% #creates a new variable which contains the values of the sub policy variables lagged by 3 rows
  ungroup() #ungroups so future analysis with plm is not affected.



#remove certain values from dataframe 
values_remove <- c(-33:-11, 2)
merged <- merged %>%
  replace_with_na_at(.vars = c("F3.subtax", 'F3.subed', "F3.subpolice", "subed", "subpolice", "subtax"),
                     condition = ~.x %in% values_remove) 
#removes all values from -33:-11 and 2 and makes assigns them NA

#creating regressions


subed_cen <- merged %>% #create subset of data to account for federalization for subed variable
  arrange(gwno, year) %>%
  filter(subed == 0) %>% #only use data from countries that are not decentralized in order to measure federalization
  select(gwno, year, subed, F3.subed, democracy_BX, state, stconst, lngdppc_WDI_PW,
         growth_WDI_PW, auton_DPI, trade_WDI, cl_FH, year2, yearsq, yearcube) #add civil conflict variable still


?glm

#run regressions

#subed -- use pglm for panel logit regressions
subed <- glm(F3.subed ~ democracy_BX + state + stconst + lngdppc_WDI_PW + growth_WDI_PW +
                auton_DPI + trade_WDI + cl_FH + year2 + yearsq + yearcube, #formula including all variables (-recent civil war)
              family = binomial(link = "logit") , data = subed_cen) #these two should be standard anyway
summary(subed)

subtax_cen <- merged %>% #create subset of data to account for federalization for subtax variable
  arrange(gwno, year) %>%
  filter(subtax == 0) %>% #only use data from countries that are not decentralized in order to measure federalization
  select(gwno, year, subtax, F3.subtax, democracy_BX, state, stconst,lngdppc_WDI_PW,
         growth_WDI_PW, auton_DPI, trade_WDI, cl_FH, year2, yearsq, yearcube) #add civil conflict variable still


subtax <- glm(F3.subtax ~ democracy_BX + state + stconst + lngdppc_WDI_PW + growth_WDI_PW +
                 auton_DPI + trade_WDI + cl_FH + year2 + yearsq + yearcube, #formula including all variables (-recent civil conflict)
               family = binomial(link = "logit") , data = subtax_cen) #these two should be standard anyway
summary(subtax)


subpolice_cen <- merged %>% #create subset of data to account for federalization for subpolice variable
  arrange(gwno, year) %>%
  filter(subpolice == 0) %>% #only use data from countries that are not decentralized in order to measure federalization
  select(gwno, year, subpolice, F3.subpolice, democracy_BX, state, stconst, lngdppc_WDI_PW,
         growth_WDI_PW, auton_DPI, trade_WDI, cl_FH, year2, yearsq, yearcube) #add civil conflict variable still


subpolice <- glm(F3.subpolice ~ democracy_BX + state + stconst + lngdppc_WDI_PW + growth_WDI_PW +
                    auton_DPI + trade_WDI + cl_FH + year2 + yearsq + yearcube, #formula including all variables (-recent civil conflict)
                  family = binomial(link = "logit") , data = subpolice_cen) 
summary(subpolice)


#setwd for proper saving of file
setwd("~/Google Drive/Federalism/Federalism article/")


#use stargazer to create table
stargazer(subed, subtax, subpolice, #choose models
          title = "Regime Type, Subnational Elections, Constituency Alignment and Extensions of subnational Policy Authority",
          covariate.labels = c("Democracy (Boix et al.)", "State/Provincial Elections", #name variables [same order as above^^]
                               "Constituency Allignment", "GDP Per Caita (logged)",
                               "GDP Growth", "Autonomous regions", "Trade (Share of GDP)",
                               "Civil Liberties"),
          column.labels = c("Education", "Tax", "Police"), #add labels to each column denoting which is which MUST BE IN ORDER
          notes.label = "Significance levels",
          type = "html",
          out = "tables/table2_glm_federalism_AR_07232020.html")







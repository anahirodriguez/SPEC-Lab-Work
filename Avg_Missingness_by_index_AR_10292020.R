#####################
# Maps of missingness in data by indices
# Powersharing Data
# Created by: Anahi Rodriguez
# 29 October 2020
#####################


#clear environment
rm(list = ls())

#install and load packages
library(dplyr)
library(ggplot2)
library(readxl)
library(maps)
library(countrycode)


#setwd
setwd("~/Google Drive/Master IPE Data")
list.files()


#load data
load("IDC_1975-2019_Aug26_2020.Rdata")


#subset data
merged <- select(merged, -c(gwno, ccode, ifs, ifscode, gwabbrev, pr_DPI, countryname_raw_IDC,
                            intax_IDC, proptax_IDC, saletax_IDC, vattax_IDC))


#changing all values but NA to 0
merged[, 3:61][merged[,3:61] >= 0] <- 0
merged[, 3:61][merged[,3:61] <= 0] <- 0



#replace NA with 1
merged[is.na(merged)] <- 1


#find sums of every country year variable per index
for_sums_dispersive <- select(merged, c(subed_IDC, subtax_IDC, subpolice_IDC, stconst_IDC, 
                                        state_IDC, admined_IDC, curriced_IDC))
for_sums_constraining <- select(merged, c(relconstd_IDC, relconstp_IDC, milleg_IDC, jconst_IDC, 
                                          jrevman_IDC, jtenure_IDC, partynoethnic_IDC))
for_sums_inclusive <- select(merged, c(gcnew_IDC, mveto_IDC, resseats_IDC, miman_IDC, resman_IDC))


#create new variables for sum of info missing per index & create new variables which to divide by
#dispersive and constraining each have 7 variables and inclusive has 5
merged <- merged %>%
  mutate(total_dispersive = rowSums(for_sums_dispersive),
         total_constraining = rowSums(for_sums_constraining),
         total_inclusive = rowSums(for_sums_inclusive),
         to_divide_by_7 = 7,
         to_divide_by_5 = 5) #create a variable to divide by for percentage of missingness

#create percentage based on sum/(either 5 or 7) -- 5 or 7 becuase that is the number of total variables
merged <- transform(merged, missing_percent_dis = total_dispersive/ to_divide_by_7)
merged <- transform(merged, missing_percent_con = total_constraining/ to_divide_by_7)
merged <- transform(merged, missing_percent_inc = total_inclusive/ to_divide_by_5)





##########################-----------maps-------------######################################

#load in map data
dat_map <- map_data("world")

#use same naming for countries so they can be merged
dat_map$ccode <- countrycode(dat_map$region, 
                             origin = "country.name",
                             destination = "wb")

merged$ccode <- countrycode(merged$country,
                            origin = "country.name",
                            destination = "wb")


#only necesarry years
merged <- subset(merged, year %in% c(1975,2000,2019))


#merge map and data set 
merged_for_map <- full_join(merged, dat_map, by = c("ccode"))



#create map using merged data------ dispersive map
dispersive_map <- ggplot(subset(merged_for_map, year == c(1975,2000, 2019)), 
                         aes(x = long, y = lat, group = group, fill = missing_percent_dis),
                         alpha = 1) +
  geom_polygon() +
  scale_fill_gradient(low = "#5656e5", high = "#b86334", name = "Avg Missingness") +
  labs(title = "Avg Missingness for Countries in 1975, 2000, & 2019") + #add a subtitle here with where data is from?
  theme_minimal() +
  facet_wrap(~year, ncol = 2)




#save dispersive map
ggsave("Fall 2020/Dis_missingness_AR_10292020.png", width = 12, height = 10, dpi = 400, dispersive_map )


#create map using merged data------ constraining map
constraining_map <- ggplot(subset(merged_for_map, year == c(1975,2000, 2019)), 
                           aes(x = long, y = lat, group = group, fill = missing_percent_con)) +
  geom_polygon() +
  scale_fill_gradient(low = "#5656e5", high = "#b86334", name = "Avg Missingness") +
  labs(title = "Avg Missingness for Countries in 1975, 2000, & 2019") + #add a subtitle here with where data is from?
  theme_minimal() +
  facet_wrap(~year, ncol = 2)


#save constraining map
ggsave("Fall 2020/Con_missingness_AR_10292020.png", width = 12, height = 10, dpi = 400, constraining_map )


#create map using merged data------ constraining map
inclusive_map <- ggplot(subset(merged_for_map, year == c(1975,2000, 2019)), 
                        aes(x = long, y = lat, group = group, fill = missing_percent_inc)) +
  geom_polygon() +
  scale_fill_gradient(low = "#5656e5", high = "#b86334", name = "Avg Missingness") +
  labs(title = "Avg Missingness for Countries in 1975, 2000, & 2019") + #add a subtitle here with where data is from?
  theme_minimal() +
  facet_wrap(~year, ncol = 2)


#save constraining map
ggsave("Fall 2020/Inc_missingness_AR_10292020.png", width = 12, height = 10, dpi = 400, inclusive_map )




# Sensitivity Anlysis: Simulations with different confidence thresholds and effect on occupancy tables
library(tidyverse)
library(fastDummies)
library(stringi)
library(lubridate)

setwd("C:/Users/apdwwolfson/Documents/Projects/Photo_Database/Machine_Learning/trained model output")
dat<-read_csv("Output/ceah_for_occ.csv")
# This dataset (imported on the line above contains all Machine Learning (ML) output from the CEAH images from FL and CA
# from the period of February 2015 - August 2017, which is about 5 million images.  The date, sequence number (aka burst #'s),
# and Event Number (aka unique # for each burst) have been extracted using the ExifTool application and 
# merged to the ML output. The dates were then converted to POSIXct and specific date categories were compiled (hour, day, week, etc).

#####################################################################################################################
setwd("C:/Users/apdwwolfson/Documents/Projects/Photo_Database/Machine_Learning/trained model output/Scripts")
source('helper_functions_for_creating_occupancy_tables.R')
# The preceding functions were made to help in the post-processing of ML output into occupancy tables ready for analysis.

# The first function 'occ_db' takes a dataframe (such as what you'd have as output from the ML model)
# that contains variables for 1) camera ID, 2) species to create occupancy table for (only works for single species for now),
# and 3) a column for time interval of interest (e.g. a week column listing the week number for each observation)
# At this point, the only time intervals that are accepted are day, week, and month (hour can eventually be added)

# The second function 'insert_NA' takes the output of the occ_db function and does several steps:
# 1) Makes a temporary dataframe which stores the start and end dates which each camera has data for,
# 2) Cycles through each cam and determines if the start dates for the camera are after the global start dates, 
# (global start date==the first date for any camera which will determine the first time interval for the occupancy table) 
# 3) If the camera starts later than the global start, then NA's are inserted into the columns for which there isn't data 
# for that specific camera and a message is printed saying that NA's are being inserted.
# 4) The same process is repeated for the end dates
# Btw, if the start and end dates are within one time interval from the global times, no NA's are inserted and a 
# message is printed that states as much.

########################################################################################################################
#' Simulations


#Two ways to simulate and filter confidence thresholds: 
# 1) by first retaining the max confidence value per burst (labled as Event Number in the exiftool output), or
# 2) just straight-up image by image
# I'll use the second, more simple, method first

# Parameters to adjust:
# 1) confidence interval at which to filter results,
# 2) time interval for which occupancy table is made (hyp: the coarser the scale, the less things matter)

# Metric of evaluation:
# For each confidence threshold value, determine the number of occupancy "intervals" that switch from 1 to 0
# It's not possible to switch from 0 to 1 because I'm only removing data, not adding it.
# I'll need to compare things to a baseline for how much the occupancy intervals are changing, so we had talked about
# using the 50% confidence level as the baseline for 'missing no pigs'.

# Visualization of Results plot:
#  For each category (facet?) of time interval:
# Figure with one axis as the confidence filter, the other as the # of occupancy events affected


###############################################################################################
###############################################################################################
# Simulation for the weekly time interval and wild pigs
confs<-seq(0.51, .99, 0.01)
week_res<-data.frame(thresholds=confs, occ_int_changed=NA, prop_changed=NA)

# 50% condidence to compare to
baseline50<-occ_db(df=dat, species = 22, thresh = 0.5, time_int = 'week')
baseline50<-insert_NA(occ_dataframe = baseline50, full_dataframe = dat, time_int = 'week')
b50<-sum(rowSums(baseline50[,-1], na.rm = T)) # this is pooling all the cameras together, which may not be the best way,
# but I'll start this way first, because it's the simplest


# all the other confidence values between 50 and 99%
for(i in 1:nrow(week_res)){
  tmp<-occ_db(df=dat, species = 22, thresh = confs[i], time_int = 'week') #22 is wild pig species label
  tmp<-insert_NA(occ_dataframe = tmp, full_dataframe = dat, time_int = 'week')
  week_res[i, 2]<-b50-sum(rowSums(tmp[,-1], na.rm = T))
  week_res[i, 3]<-week_res[i,2]/b50
  print(paste('Confidence value', confs[i], sep=' '))
}   # this is breaking at the confidence value of 0.8 becuase it thinks there aren't any records from FL-03


test<-occ_db(df=dat,species=22, thresh=0.8, time_int = 'week')
test1<-insert_NA(occ_dataframe = test, full_dataframe = dat, time_int = 'week')
str(test1)
class(rep(NA, ncol(tmp)-1))
str(tmp[seq(rowNums, nrow(tmp)),])

ggplot(week_res, aes(thresholds, prop_changed))+geom_point()+
  ggtitle("Simulations for Weekly Occupancy Intervals")+
  theme(plot.title=element_text(hjust=0.5))
###############################################################################################
# Simulation for the daily time interval and wild pigs
confs<-seq(0.51, .99, 0.01)
day_res<-data.frame(thresholds=confs, occ_int_changed=NA, prop_changed=NA)

# 50% condidence to compare to
baseline50<-occ_db(df=dat, species = 22, thresh = 0.5, time_int = 'day')
baseline50<-insert_NA(occ_dataframe = baseline50, full_dataframe = dat, time_int = 'day')
b50<-sum(rowSums(baseline50[,-1], na.rm = T)) # this is pooling all the cameras together, which may not be the best way,
# but I'll start this way first, because it's the simplest


# all the other confidence values between 50 and 99%
for(i in 1:nrow(day_res)){
  tmp<-occ_db(df=dat, species = 22, thresh = confs[i], time_int = 'day') #22 is wild pig species label
  tmp<-insert_NA(occ_dataframe = tmp, full_dataframe = dat, time_int = 'day')
  day_res[i, 2]<-b50-sum(rowSums(tmp[,-1], na.rm = T))
  day_res[i, 3]<-day_res[i,2]/b50
  print(paste('Confidence value', confs[i], sep=' '))
}   


ggplot(day_res, aes(thresholds, prop_changed))+geom_point()+
  ggtitle("Simulations for daily Occupancy Intervals")+
  theme(plot.title=element_text(hjust=0.5))

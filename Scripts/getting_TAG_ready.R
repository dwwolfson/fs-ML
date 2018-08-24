# Add hour functionality to the occ_db() function.....

library(tidyverse)
library(lubridate)
library(fastDummies)
library(here)


#Use the TAG data to test becuase it's pretty small and that's what I'll nedd to use it on anyway.
setwd('C:/Users/apdwwolfson/Documents/Projects/Photo_Database/Machine_Learning/CEAH_Exif_output')
tag_exif<-read_csv("All_TAG_output.csv")
cols_keep<-c('Directory','FileName', 'DateTimeOriginal')
tag_exif<-tag_exif[,cols_keep]
tag_exif<-transform(tag_exif, unique_chk=as.numeric(factor(Directory)))

tag_exif$camID<-sapply(tag_exif$FileName, function(x)
  strsplit(x, '_')[[1]][1])

#ML output
df<-read_csv('C:/Users/apdwwolfson/Documents/Projects/Photo_Database/Machine_Learning/trained model output/Data/species_model_predictions/L1_preds.csv')

#add study names to ml output
df<-df%>%
  mutate(study=ifelse(IMG_PATH%in%grep("CPW", IMG_PATH, value=T), "CPW", 
                      ifelse(IMG_PATH%in%grep("NWRC", IMG_PATH, value=T), "NWRC",
                             ifelse(IMG_PATH%in%grep('ACAM', IMG_PATH, value=T),'SREL', 
                                    ifelse(IMG_PATH%in%grep('FL', IMG_PATH, value=T), 'FL',
                                           ifelse(IMG_PATH%in%grep('CA-', IMG_PATH, value=T), 'CA', 
                                                  ifelse(IMG_PATH%in%grep('Florida', IMG_PATH, value=T), 'FL',
                                                         ifelse(IMG_PATH%in%grep('TAG', IMG_PATH, value=T), 'TAG', 
                                                                ifelse(IMG_PATH%in%grep('Tejon', IMG_PATH, value=T), 'CA', 'flag')))))))))
table(df$study)
tag_ml<-df[df$study=="TAG",]
apply(is.na(tag_ml), 2, sum) # no NA's

#Convert the image name to just the camera number and image number
tag_ml$FileName<-sapply(tag_ml$IMG_PATH, function(x)
  strsplit(x, '/')[[1]][4])

#Merge the two tag datasets
sum(duplicated(tag_exif$FileName))
tag_exif<-tag_exif[!duplicated(tag_exif$FileName),] #237,596 to 236,200

alltag<-left_join(tag_ml, tag_exif, by='FileName')
apply(is.na(alltag),2,sum)
alltag<-na.omit(alltag) #remove 311 rows with no date

#Deal with date/times
options(lubridate.verbose = TRUE)
summary(nchar(alltag$DateTimeOriginal))
alltag$DateTimeOriginal<-ymd_hms(alltag$DateTimeOriginal, tz='US/Pacific') #no errors parsing
alltag<-alltag %>% 
  mutate(year=lubridate::year(DateTimeOriginal),
         week=week(DateTimeOriginal),
         day=yday(DateTimeOriginal),
         hour=hour(DateTimeOriginal),
         month=month(DateTimeOriginal))

summary(alltag$DateTimeOriginal)
table(alltag$year)
alltag<-alltag[!alltag$year=='2011',] #there are some images that had metadata showing the year 2011, which should be removed

#write out
write_csv(alltag, "C:/Users/apdwwolfson/Documents/Projects/Photo_Database/Machine_Learning/trained model output/Output/TAG_cams/tagdf.csv")
##############################################################################################################################
##############################################################################################################################


# A function for taking the output of the Machine Learning model (MLWIC) and turn it into a table of 0/1 for occupancy analyses.

occ_db<-function(df,species, thresh, time_int){ 
  # df must be a dataframe with variables[camID=]
  # species must be the numeric label corresponding to the desired species ID from lookup table
  # thresh must be the cutoff for confidence values (between 0 and 1)
  # time_in must be either 'day', 'week', or 'month'
  if(!time_int%in%c("day", "week", "month")){
    stop("Error: Input 'time_int' needs to be either day, week, or month")
  }
  df<-df[df$GUESS1==species,]    #filter by species of interest
  df<-df[df$CONFIDENCE1>thresh,] #filter by confidence level threshold below which images aren't used
  years<-sort(unique(unlist(df[,"year"]))) # get vector of years present
  db<-as.data.frame(matrix(nrow=length(unique(df$camID)))) #create dataframe with correct dimensions to fill with info
  db[,1]<-sort(unique(df$camID))    #arrange cameras in numeric order
  for(i in 1:length(years)){
    tmp<-df[df$year==years[[i]],]   #grab subset by year
    tmp<-fastDummies::dummy_cols(tmp, select_columns = time_int)  #make dummy columns by preferred time interval
    tmp<-tmp[stringi::stri_order(sub(".*_", "", names(tmp)), numeric=TRUE)] #order the time interval columns
    tmp<-tmp%>%group_by(camID)%>%
      summarise_at(.vars=names(tmp)[grepl(paste0(time_int, '_'), names(tmp))], .funs = max) #collapse rows into 0/1
    # db$camID<-tmp$camID #set first column as the camera ID's
    
    if(!length(db[,1])==length(tmp$camID)){  #check for missing camera names and add them in with NA's
      missing<-setdiff(db[,1], tmp$camID)
      if(length(missing)==1){ # the structure of what to do is different if only one camera is missing vs multiple
        rowNum<-grep(pattern = missing, x = db[,1])   #find which cam is missing
        newRow<-c(missing, as.numeric(rep(NA, ncol(tmp)-1)))   # combine cam name with correct dim's of NA's
        tmp[seq(rowNum+1, nrow(tmp)+1),]<-tmp[seq(rowNum, nrow(tmp)),] #this creates an extra row so 
        # the dataframe has the proper dimensions and duplicates a row at the point where a missing 
        # one should be added
        tmp[rowNum,]<-newRow #then this adds the missing row at the right place, overwriting the copied row
        tmp[,-1]<-as.data.frame(sapply(tmp[,-1], as.numeric)) #need the columns to be numeric instead of character
      }else if(length(missing)>1){  #if there are multiple cameras that are missing from the yearly subselection...
        rowNums<-NA    #create blank object
        for(j in 1:length(missing)){
          rowNums[j]<-grep(pattern=missing[j], x = db[,1]) #collect the rows (cameras) that are missing
        }
        for(k in 1:length(rowNums)){
          tmprow<-c(missing[k], as.numeric(rep(NA, ncol(tmp)-1)))
          if(rowNums[k]<=nrow(tmp)){
            tmp[seq(rowNums[k]+1, nrow(tmp)+1),]<-tmp[seq(rowNums[k], nrow(tmp)),]
            tmp[rowNums[k],]<-tmprow
          } else if(rowNums[k]>nrow(tmp)){
            tmp[rowNums[k],]<-tmprow
          } 
          tmp[,-1]<-as.data.frame(sapply(tmp[,-1], as.numeric)) #prevent the summation from bonking because rows were characters
        }
      }
    }
    tmp<-tmp%>%select(-camID)%>%
      setNames(paste(years[[i]], names(tmp[-1]), sep='_'))  #add year prefix to the column names
    db<-cbind.data.frame(db,tmp)                            #add to dataframe
    j<-k<-NULL #reset counters
  }
  # db<-db[,-1] #cut off the first junk column which is empty
  return(db)
}
#####################################################################################################################
# A function for adding 0's when the camera is active but didn't have the focal sp present

#Now I need to add zeros for two different situations:
# 1) When there are additional active intervals (that don't have the focal sp) at the start and end of 
# a camera's "focal sp-containing" interval of pictures-- these aren't considered when you filter down to the focal sp as guess 1
# 2) Similar to above, but between the start and end times, if there are entire intervals where no cameras contain the focal sp,
# the interval won't be included (this is more of an issue as the time interval gets smaller)

# Whereas the previous function filled in the difference between the global (or first) cam date and each individual 
# camera date and fills in teh occupnacy table with NA's,
# this finds the difference in the active date for each camera and the first date the focal sp is detected, and then fills in 0's.

#Bring in reftable
reftable<-read_csv('Data/TAG_Cam_Dates.csv')
reftable$SetDate<-as.POSIXct(reftable$SetDate, format="%d-%b-%y")

#reduce to just tag cams
# reftable<-reftable[grep(x=reftable$LocationID, pattern = 'TAG'),] # if it wasn't already reduced down to just the tag cams

# Extract last date present for each camera
refs<-alltag%>%
  group_by(camID)%>%
  summarise(end.date=max(DateTimeOriginal))

# Extract set date for each camera
colnames(reftable)[1]<-"camID"
reftable$camID<-gsub(' ', '', reftable$camID) # take out spaces in the camera names
refs<-left_join(refs, reftable[,c("camID", "SetDate")])

# TAG-TC33 wasn't on the spreadsheet for set dates for some reason
refs[refs$camID=='TAG-TC33','SetDate']<-as.POSIXct('2015-08-18', format='%Y-%m-%d')

# Don't really want time for the end date
refs$end.date<-substr(refs$end.date, 1, 10)

# Join start and end dates onto dataframe
# alltag<-left_join(alltag, refs)

# Plan:

# First solution will deal with the entire dataframe as a whole:
# Based on the global start and end dates that will set the dimensions of the occupancy dataframe, I'll first
# 1) create a sequence from global start (from earliest set date) to global end date (as the last active date in the full unfiltered database,
# which would equate to the end of the cameras being active) with the time interval of choice,
# 2) make a vector made from the global start/end dates using the time interval and compare to the existing columns and 
# use setdiff to pull out the columns that are missing from existing dataframe
# 3) use the format: xx<-c('week_23', 'week_24', etc), df[xx]<-0 to create columns with proper names and 0's
# 4) then sort the columns so they are in the proper order
# This solves the problem of time intervals in 'the middle' of the deployments, but not cameras that were set and pulled at different times


add_zero_cols<-function(occ_df, cam_set_dates, camID , time_int, dates_full_df){
  # 'dates_full_df' need to be a vector of dates in POSIXct format for date/times of the full dataset (not filtered for focal sp)
  #  'cam_set_dates' need to be a vector of dates in POSIXct format for when cameras were set up
  # camID should be a vector of camera names that corresponds to the column for camera ID in the occupancy table being used
  if(!is.POSIXct(cam_set_dates)){
    stop("Error: Input 'cam_set_dates' needs to be in POSIXct format")}
  if(!is.POSIXct(dates_full_df)){
      stop("Error: Input 'dates_full_df' needs to be in POSIXct format")}
  if(!time_int%in%c("hour", "day", "week")){
    stop("Error: Input 'time_int' needs to be either hour, day, or week")
  }
  global_range<-c(min(cam_set_dates), max(dates_full_df))
  first_day<-yday(global_range[1])
  last_day<-yday(global_range[2])
  # AT THIS POINT WILL ONLY WORK FOR A SINGLE YEAR AT A TIME; THIS CAN BE CHANGED LATER#
  full_intervals<-paste(year(global_range[1]), time_int, seq(from=first_day, to=last_day, by=1), sep='_')
  cols_to_add<-setdiff(full_intervals, names(occ15[-1]))
  occ_df[cols_to_add]<-0  #these should be 0 because cam's were active, if there are periods at beginning and
  # end where cameras weren't active the next function (insert_NA) should correct it
  occ_df<-occ_df[stringi::stri_order(sub(".*_", "", names(occ_df)), numeric=TRUE)]
  occ_df<-occ_df[,c(ncol(occ_df), 1:(ncol(occ_df)-1))]  #move camera ID column back to the first position
  return(occ_df)
  }

##############################################################################################################################

##############################################################################################################################
# A function for 1) adding NA's into the occupancy table when the camera isn't active 
# (instead of 0's which would indicate the focal sp wasn't present)
# This is needed because the cameras don't all have the same set and pull dates
insert_NA<-function(occ_dataframe,full_dataframe, time_int){
  #time_int needs to be either week, day, or hour for this function, can't be month (although I could write in month to just be equal to 30 days...)
  if(!is.POSIXct(full_dataframe$DateTimeOriginal)){
    stop("Error: Input 'full_dataframe$DateTimeOriginal' needs to be in POSIXct format")
  }
  if(!time_int%in%c("hour", "day", "week")){
    stop("Error: Input 'time_int' needs to be either hour, day, or week")
  }
  # 1) First I'll probably want to pull out all the deployment info for all the cameras
  camDates<-full_dataframe%>%  #note this only reflects
    group_by(camID)%>%
    summarise(start_date=min(DateTimeOriginal), 
              start_yr=year(start_date),
              start_wk=week(start_date),
              start_day=yday(start_date),
              start_yr_wk=paste(start_yr, start_wk, sep='_'),
              end_date=max(DateTimeOriginal), 
              end_yr=year(end_date),
              end_wk=week(end_date),
              end_day=yday(end_date),
              end_yr_wk=paste(end_yr, end_wk, sep='_'))
  # 2) Now I'll need to compare the deployment dates of each camera to the overall range of dates 
  #    that the occupancy table is compiling information for.
  date_range<-c(min(full_dataframe$DateTimeOriginal), max(full_dataframe$DateTimeOriginal)) # except these are different time zones
  # First convert camDates to data.frame for it to work nicely with the ifelse statement
  camDates<-as.data.frame(camDates)
  #3) Now I need a way to determine if 1) the start date of the cam is after the global start, and by how much;
  # and 2) if the end data of the cam is before the global end date
  for(i in 1:nrow(camDates)){
    if(camDates[i,"start_yr"]!=lubridate::year(date_range[1])){
      print(paste("Camera", camDates[i, "camID"], "has a start date that isn't the same year as the global start date. Exclude and rerun."))
      break}
    s.diff<-difftime(camDates[i,'start_date'], date_range[1]) #diff b/w cam start and occ table global start
    if(time_int=="week"){s.diff.units<-as.numeric(s.diff, units="weeks")}
    if(time_int=="day"){s.diff.units<-as.numeric(s.diff, units="days")}
    if(time_int=="hour"){s.diff.units<-as.numeric(s.diff, units="hours")} #months don't work because they aren't of fixed duration
    if(abs(s.diff.units)<1){
      print(paste("Camera", camDates[i, "camID"], "starts within one time interval of global start."))
    } else if(abs(s.diff.units)>1){
      print(paste("Camera", camDates[i, "camID"], "starts more than one interval from global start. Adding NA's to occupancy dataframe."))
      start_cols<-round(s.diff.units)
      occ_dataframe[i,2:(2+start_cols)]<-NA} #this makes the first time interval that the camera has NA as well because it's a partial interval
    if(camDates[i, "end_yr"]!=lubridate::year(date_range[2])){
      print(paste("Camera", camDates[i, "camID"], "has an end date that isn't the same year as the global start date. Exclude and rerun."))
      break}
    e.diff<-difftime(camDates[i,'end_date'], date_range[2]) #diff b/w cam end and occ table global end
    if(time_int=="week"){e.diff.units<-as.numeric(e.diff, units="weeks")}
    if(time_int=="day"){e.diff.units<-as.numeric(e.diff, units="days")}
    if(time_int=="hour"){e.diff.units<-as.numeric(e.diff, units="hours")}
    if(abs(e.diff.units)<1){
      print(paste("Camera", camDates[i, "camID"], "ends within one time interval of global end."))
    } else if (abs(e.diff.units)>1){
      print(paste("Camera", camDates[i, "camID"], "ends more than one interval from global end. Adding NA's to occupancy dataframe."))
      num_underscores<-stringr::str_count(last(names(occ_dataframe)), '_')
      last_col<-as.numeric(strsplit(last(names(occ_dataframe)), '_')[[1]][num_underscores+1])+round(e.diff.units)
      occ_dataframe[i, grep(last_col, names(occ_dataframe)):ncol(occ_dataframe)]<-NA 
      #this makes the last time interval that the camera has NA as well because it's a partial interval
    }
  }
  return(occ_dataframe)
}

###############################################################################################################################
alltag<-read_csv("Output/TAG_cams/tagdf.csv")


#Simulations for day intervals

#Baseline with no filtering
no_filtdb<-occ_db(df = alltag, species = 22, thresh = 0, time_int = 'day')
bust<-insert_NA(occ_dataframe = no_filtdb, full_dataframe = alltag, time_int = 'day') #the year thing screws it up
tag15<-alltag[alltag$year==2015,]
tag16<-alltag[alltag$year==2016,]
occ15<-occ_db(df = tag15, species = 22, thresh = 0, time_int = 'day')
test<-add_zero_cols(occ_df = occ15, cam_set_dates = refs$SetDate, camID = refs$camID, time_int = 'day', dates_full_df = tag15$DateTimeOriginal)
test1<-insert_NA(occ_dataframe = test, full_dataframe = tag15, time_int = 'day')
occ16<-occ_db(df=tag16, species=22, thresh=0, time_int='day')
occ16<-add_zero_cols(occ_df = occ16, cam_set_dates = refs$SetDate, camID = refs$camID, time_int = 'day', dates_full_df = tag15$DateTimeOriginal)
occ16<-insert_NA(occ_dataframe = occ16, full_dataframe = tag16, time_int = 'day')


ref15<-refs[30:43,]

# from camtrapR
# ct15<-cameraOperation(CTtable = ref15, stationCol = 'camID', setupCol = 'SetDate', retrievalCol = 'end.date', dateFormat = "%Y-%m-%d")# works but only for daily intervals?
df15<-insert_NA(occ_dataframe = occ15, full_dataframe = tag15, time_int = 'day')


hist(as.numeric(alltag$DateTimeOriginal))
table(alltag$camID, alltag$year)

# Given that 14 cams are only in 2015, and 29 are only in 2016, prob makes sense to separate them

df = alltag 
species = 22
thresh = 0
time_int = 'day'





# Objectives of this script:
# Determine the sensitivities of filtering the machine learning output at different threshold values if
# 'considering' bursts by their highest species confidence values. Then looking at how many bursts
# you lose at different confidence thresholds, 

# and how many of those lost (and retained) actually had the target species. (this might not be possible)


library(tidyverse)
library(lubridate)
library(fastDummies)
# Bring together all the exiftool metadata output
setwd('C:/Users/apdwwolfson/Documents/Projects/Photo_Database/Machine_Learning/CEAH_Exif_output')
root_path<-'C:/Users/apdwwolfson/Documents/Projects/Photo_Database/Machine_Learning/CEAH_Exif_output'
ca_path<-paste(root_path, "CA_exiftool_output", sep='/')
fl_path<-paste(root_path, "FL_output", sep='/')

####################
# # TAG Images (in one level above CA and FL image folders)
# tag<-read_csv('All_Tag_output.csv') #doesn't look like there is information about the bursts
# # No Sequence or Event Number
# names(tag)
# tag_cols<-c('Directory','FileName', 'DateTimeOriginal')
# tag<-tag[,tag_cols]

##################
cols_keep<-c('Directory','FileName', 'DateTimeOriginal', 'Sequence', 'EventNumber')

#CA Images
setwd(ca_path)
ca_files<-list.files()

ca_db<-data.frame()

#combine in a loop (doesn't take too long)
for(i in 1:length(ca_files)){
  temp<-read_csv(ca_files[i])
  temp<-temp[,cols_keep]
  temp<-transform(temp, unique_chk=as.numeric(factor(Directory)))
  temp$chk_burst<-paste(temp$unique_chk, temp$EventNumber, sep='_')
  ca_db<-rbind(ca_db, temp)
  print(i)
}
ca_db$camID<-sapply(ca_db$FileName, function(x)
  strsplit(x, '_')[[1]][1])
ca_db$cam_chk_burst<-paste(ca_db$camID, ca_db$chk_burst, sep='_')
length(unique(ca_db$cam_chk_burst)) #319,883 separate bursts  (1,792,150 images)

###############################
#fl Images
setwd(fl_path)
fl_files<-list.files()

fl_db<-data.frame()

#combine in a loop 
for(i in 1:length(fl_files)){
  temp<-read_csv(fl_files[i])
  temp<-temp[,cols_keep]
  temp<-transform(temp, unique_chk=as.numeric(factor(Directory)))
  temp$chk_burst<-paste(temp$unique_chk, temp$EventNumber, sep='_')
  fl_db<-rbind(fl_db, temp)
  print(i)
} # this still takes a while to load all the data frames

fl_db$camID<-sapply(fl_db$FileName, function(x)
  strsplit(x, '_')[[1]][1])
fl_db$cam_chk_burst<-paste(fl_db$camID, fl_db$chk_burst, sep='_')
length(unique(fl_db$cam_chk_burst)) # 408,768 separate bursts (3,193,134 images)

##########################
# Combine all
# tag$study<-'tejon_TAG'
ca_db$study<-'tejon'
fl_db$study<-'florida'
dat<-rbind(ca_db, fl_db) #exclude tag cams becuase there aren't separate event numbers; 4,986,284 images
dat<-na.omit(dat)
apply(is.na(dat),2, sum)

#Looks like this is all the CEAH images that were run against the newer versin of the model, 
# though we only have label info from the APHIS query
df<-read_csv('C:/Users/apdwwolfson/Documents/Projects/Photo_Database/Machine_Learning/trained model output/Data/species_model_predictions/L1_preds.csv')
# put study names on so I can just use FL and CA images

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
#Exclude TAG for now becuase no event numbers for burst periods
df<-df[!df$study=='TAG',]
apply(is.na(df),2, sum)

#Convert the image name to just the camera number and image number
df$FileName<-sapply(df$IMG_PATH, function(x)
  strsplit(x, '/')[[1]][4])

#Merge the two datasets
alldat<-left_join(df, dat, by='FileName')

#trim columns
alldat<-alldat[, -which(names(alldat)%in%c('ROW_NUM', 'study.y', 'IMG_PATH'))]
names(alldat)
#probably not worth saving out this enormous csv
#####################################################################################################################
#Now group by burst and then filter by different confidence levels to see how many bursts are dropped

# First for one confidence level (0.95) and then for everything between 0.5 and 1
conf<-0.95

# I think I don't want to filter to images that are pig guesses because a burst could contain images with different guesses
#
# First find number of bursts that have pig in guess1, then find out how many we'd lose at different conf levels
pig_burstdb<-alldat%>%
  filter(GUESS1==22)                                   #289,703 rows
tot_bursts<-length(unique(pig_burstdb$cam_chk_burst)) # total number of bursts that contained a guess for pig 61k (regardless of conf thresh)
conf95<-pig_burstdb%>%
  group_by(cam_chk_burst)%>%
  mutate(b_max=max(CONFIDENCE1), b_min=min(CONFIDENCE1))
hist(conf95$b_max)
hist(conf95$b_min)
summary(conf95$b_max)

#now filter based on max burst
filt_conf95<-conf95%>%
  filter(b_max>0.9499)
length(unique(filt_conf95$cam_chk_burst))/tot_bursts #59% of bursts retained

hist(filt_conf95$b_min)

conf95<-arrange(conf95, camID, unique_chk, EventNumber)


###################################################################################################
# now to see the relationship with lots of different confidence thresholds

confs<-seq(0.5, 1, 0.01)
filt_db<-data.frame(thresholds=confs, prop_retained=NA)
for(i in 1:length(confs)){
  tmp<-pig_burstdb%>%
    group_by(cam_chk_burst)%>%
    mutate(b_max=max(CONFIDENCE1), b_min=min(CONFIDENCE1)) # calculate min and max conf values per burst
  tmp<-tmp%>%
    filter(b_max>confs[i])                                  #filter out data by the max burst confidence
  filt_db[i,2]<-length(unique(tmp$cam_chk_burst))/tot_bursts #compare number of burst remaining to original total
}
ggplot(filt_db, aes(thresholds, prop_retained))+geom_point()+geom_line()
#########################################################################################



# I also want to know how this would affect occupancy input data (and consider different length time intervals)
# I'll replicate the previous simulations, but carry it all the way through to making occupancy tables
# For each threshold, I'll compare it to the results of using a confidence cut-off at 50% to quantify how many 
# time intervals would have a different 0/1 value.

# I'll do this all the way through once, and then run all the confidence thresholds in a loop
# I'll compare everything to the 50% filter

# add all the date information first
summary(nchar(alldat$DateTimeOriginal)) 
apply(is.na(alldat),2, sum)
alldat<-na.omit(alldat)
colnames(alldat)[11]<-'study'
alldat$study<-as.factor(alldat$study)

#convert to posix.ct
unique(alldat$study)
fl<-alldat[alldat$study=='FL',]
ca<-alldat[alldat$study=='CA',]
options(lubridate.verbose=TRUE)
ca$DateTimeOriginal<-ymd_hms(ca$DateTimeOriginal, tz = "US/Pacific") # 40 failed to parse
fl$DateTimeOriginal<-ymd_hms(fl$DateTimeOriginal, tz = "US/Eastern") # 160 filed to parse
alldat<-rbind(ca, fl)
alldat$year<-year(alldat$DateTimeOriginal)
alldat$week<-week(alldat$DateTimeOriginal)
alldat$day<-day(alldat$DateTimeOriginal)
alldat$hour<-hour(alldat$DateTimeOriginal)
alldat$month<-month(alldat$DateTimeOriginal)
alldat<-alldat[!is.na(alldat$day),]            #filter on one of the new columns to remove rows where the dates didn't parse


#Check all the different time zones that are present in the dataset
# table(sapply(alldat$DateTimeOriginal, tz)) #it's saying that they're all "US/Pacific" ???

# 50% filter as a baseline to compare to
p50<-alldat%>%
  filter(GUESS1==22)                                   
p50<-p50%>%
  group_by(cam_chk_burst)%>%
  mutate(b_max=max(CONFIDENCE1), b_min=min(CONFIDENCE1)) 
p50<-p50%>%
  filter(b_max>0.4999)

#########
# now turn to occ table

#function for different species, thresholds and time intervals (saved as a separate file)

occ_db<-function(df,species, thresh, time_int){ 
  # df must be a dataframe with variables[camID=]
  # species must be the numeric label corresponding to the desired species ID from lookup table
  # thresh must be the cutoff for confidence values (between 0 and 1)
  # time_in must be either 'day', 'week', or 'month'
  df<-df[df$GUESS1==species,]
  df<-df[df$CONFIDENCE1>thresh,] #filter by confidence level
     years<-sort(unique(unlist(df[,"year"]))) # get vector of years present
     db<-as.data.frame(matrix(nrow=length(unique(df$camID)))) #create dataframe with correct dimensions to fill with info
     
     for(i in 1:length(years)){
       tmp<-df[df$year==years[[i]],]   #grab subset by year
       tmp<-dummy_cols(tmp, select_columns = time_int)  #make dummy columns by preferred time interval
       tmp<-tmp[stringi::stri_order(sub(".*_", "", names(tmp)), numeric=TRUE)] #order the time interval columns
       tmp<-tmp%>%group_by(camID)%>%
         summarise_at(.vars=names(tmp)[grepl(paste0(time_int, '_'), names(tmp))], .funs = max) #collapse rows into 0/1
       db$camID<-tmp$camID #set first column as the camera ID's
       tmp<-tmp%>%select(-camID)%>%
         setNames(paste(years[[i]], names(tmp[-1]), sep='_'))  #add year prefix to the column names
       db<-cbind.data.frame(db,tmp)                            #add to dataframe
     }
     db<-db[,-1] #cut off the first junk column which is empty
     return(db)
}

test<-occ_db(df=alldat,species=22, thresh=0.5, time_int = 'week')
test1<-occ_db(df=alldat,species=22, thresh=0.5, time_int = 'month')

# ***   A problem of the dummy_cols function is that it's giving something a 0 whether a cam was active or not, instead 
# of a 0 is cam active and no pig and a NA if cam not active. I'll need another function to deal with this.
##############################################################################################
##############################################################################################

insert_NA<-function(occ_dataframe,full_dataframe, time_int){
  #time_int needs to be either week, day, or hour for this function, can't be month (although I could write in month to just be equal to 30 days...)
  # 1) First I'll probably want to pull out all the deployment info for all the cameras
  camDates<-full_dataframe%>%
  group_by(camID)%>%
  summarise(start_date=min(DateTimeOriginal), 
            start_yr=year(start_date),
            start_wk=week(start_date),
            start_yr_wk=paste(start_yr, start_wk, sep='_'),
            end_date=max(DateTimeOriginal), 
            end_yr=year(end_date),
            end_wk=week(end_date),
            end_yr_wk=paste(end_yr, end_wk, sep='_'))
# 2) Now I'll need to compare the deployment dates of each camera to the overall range of dates 
#    that the occupancy table is compiling information for.
date_range<-c(min(df$DateTimeOriginal), max(df$DateTimeOriginal)) # except these are different time zones
# First convert camDates to data.frame for it to work nicely with the ifelse statement
camDates<-as.data.frame(camDates)
#3) Now I need a way to determine if 1) the start date of the cam is after the global start, and by how much;
# and 2) if the end data of the cam is before the global end date
for(i in 1:nrow(camDates)){
if(camDates[i,"start_yr"]!=year(date_range[1])){
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
if(camDates[i, "end_yr"]!=year(date_range[2])){
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
       end_cols<-round(e.diff.units) # this should be negative
       occ_dataframe[i, (ncol(occ_dataframe)+end_cols):ncol(occ_dataframe)]<-NA
       #this makes the last time interval that the camera has NA as well because it's a partial interval
     }
}
return(occ_dataframe)
}

###############################################################################################
################################################################################################
#test it out
super_test<-insert_NA(occ_dataframe=test,full_dataframe=alldat, time_int="week")
# Looks like it works!!


# ggplot(df, aes(camID, DateTimeOriginal))+geom_line()+coord_flip()  
ggplot(camDates, aes(camID, start_date))+geom_point()+coord_flip()+geom_vline(xintercept=date_range[1])
# don't know why vline isn't showing up, maybe the time zone?

ggplot(camDates, aes(camID, end_date))+geom_point()+coord_flip()+geom_vline(xintercept=date_range[2])

########################
# For 95% filter
conf95<-pig_burstdb%>%
  group_by(cam_chk_burst)%>%
  mutate(b_max=max(CONFIDENCE1), b_min=min(CONFIDENCE1)) 
hist(conf95$b_max)
hist(conf95$b_min)
summary(conf95$b_max)

#now filter based on max burst 
filt_conf95<-conf95%>%
  filter(b_max>0.9499)
length(unique(filt_conf95$cam_chk_burst))/tot_bursts






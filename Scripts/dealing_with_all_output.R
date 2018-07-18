library(tidyverse)
library(lubridate)
library(fastDummies)

############################
# Bring together all the exiftool metadata output
setwd('C:/Users/apdwwolfson/Documents/Projects/Photo_Database/Machine_Learning/CEAH_Exif_output')
root_path<-'C:/Users/apdwwolfson/Documents/Projects/Photo_Database/Machine_Learning/CEAH_Exif_output'
ca_path<-paste(root_path, "CA_exiftool_output", sep='/')
fl_path<-paste(root_path, "FL_output", sep='/')

####################
# TAG Images (in one level above CA and FL image folders)
tag<-read_csv('All_Tag_output.csv') #doesn't look like there is information about the bursts
names(tag)
cols_keep<-c('Directory','FileName', 'DateTimeOriginal' )
tag<-tag[,cols_keep]

##################
#CA Images
setwd(ca_path)
ca_files<-list.files()

ca_db<-data.frame()

#combine in a loop (doesn't take too long)
for(i in 1:length(ca_files)){
  temp<-read_csv(ca_files[i])
  temp<-temp[,cols_keep]
  ca_db<-rbind(ca_db, temp)
  print(i)
}

###############################
#fl Images
setwd(fl_path)
fl_files<-list.files()

fl_db<-data.frame()

#combine in a loop 
for(i in 1:length(fl_files)){
  temp<-temp[,cols_keep]
  fl_db<-rbind(fl_db, temp)
  print(i)
}
##########################
# Combine all
tag$study<-'tejon_TAG'
ca_db$study<-'tejon'
fl_db$study<-'florida'
df<-rbind(tag, ca_db, fl_db)

# remove records where the DateTime didn't get recorded
apply(is.na(df), 2, sum)
df<-df[!is.na(df$DateTimeOriginal),]

############################################################################
# Now merge this with the model output
preds<-read_csv('C:/Users/apdwwolfson/Documents/Projects/Photo_Database/Machine_Learning/output_from_all_unclassified/L1_preds.csv' )

# Now some name wrangling to be able to merge these two datasets together.
# It's probably easiest to merge on just the filename, since they should all be unique
# I want the Camera ID in the name as well becasuse I'm pretty sure each camera has it's 
# own unique ID number(concatenated with cam ID)

# I'll separate on the "/", at least for the preds dataframe
preds$FileName<-sapply(preds$IMG_PATH, function(x)
  strsplit(x, "/")[[1]][4])

# Now merge
alldat<-left_join(preds, df)   # 5,241,516 records
apply(is.na(alldat),2,sum)     # 303,469 records from the datetime df didn't match the model output

##
# *****  Maybe dig a little deeper to figure out why this merge made NA's   ****

# I'll drop them 
alldat<-alldat[!is.na(alldat$DateTimeOriginal),]
apply(is.na(alldat),2,sum) # No more NA's

##################################
# Add date and time specific columns

# can trim a few columns
alldat<-alldat[, c("FileName", "Directory", "study", "DateTimeOriginal", 
                   "GUESS1","GUESS2", "GUESS3","GUESS4","GUESS5","CONFIDENCE1","CONFIDENCE2",
                   "CONFIDENCE3", "CONFIDENCE4","CONFIDENCE5")]
summary(nchar(alldat$DateTimeOriginal)) #all 19 characters, that's good

alldat$study<-as.factor(alldat$study)

#convert to posix.ct
unique(alldat$study)
fl<-alldat[alldat$study=='florida',]
ca<-alldat[alldat$study=='tejon'|alldat$study=='tejon_TAG',]
ca$DateTimeOriginal<-ymd_hms(ca$DateTimeOriginal, tz = "US/Pacific") # 40 failed to parse
fl$DateTimeOriginal<-ymd_hms(fl$DateTimeOriginal, tz = "US/Eastern") # 160 filed to parse
alldat<-rbind(ca, fl)
alldat$year<-year(alldat$DateTimeOriginal)
alldat$week<-week(alldat$DateTimeOriginal)
alldat$day<-day(alldat$DateTimeOriginal)
alldat$hour<-hour(alldat$DateTimeOriginal)
alldat<-alldat[!is.na(alldat$day),] #filter on one of the new columns to remove rows where the dates didn't parse


#write out dataset in it's current state
setwd('C:/Users/apdwwolfson/Documents/Projects/Photo_Database/Machine_Learning/trained model output/Data/all_CEAH')
write.csv(alldat, "all_results_with_dates.csv")
##############################################
# now to get it into a format to be used for occupancy analyses
setwd('C:/Users/apdwwolfson/Documents/Projects/Photo_Database/Machine_Learning/trained model output/Data/all_CEAH')
alldat<-read_csv('all_results_with_dates.csv')
#still need to pull out the camera ID information though to have a separate row for each camera
alldat$FileName<-as.character(alldat$FileName)
alldat$camID<-sapply(alldat$FileName, function(x)
  strsplit(x, "_")[[1]][1])
#####################################################################################################################



# Now to make a dataframe with camID as rows and 'presence by week' as columns
# I'll deal with just one year at a time for now
pigs<-alldat[alldat$GUESS1==22, ]
table(pigs$year)
#########################################################################
#########################################################################
# 2015
p15<-pigs[pigs$year==2015,]
p15<-dummy_cols(p15, select_columns = 'week')

# Sort the new columns by the order of the weeks
p15<-p15[stringi::stri_order(sub(".*_", "", names(p15)), numeric=TRUE)]

# Now collapse rows on camera ID. I'll take the max of the columns, so if there is a 1 for that week it'll stay 1, otherwise 0
occ_pig15<-p15%>%
  group_by(camID)%>%
  summarise_at(.vars = names(p15)[grepl('week_', names(p15))], .funs = max)

#and add the year prefix
colnames(occ_pig15)<-paste('2015', colnames(occ_pig15), sep='_')
colnames(occ_pig15)[1]<-'camID'  #but not the first one
#########################################################################
# 2016
p16<-pigs[pigs$year==2016,]
p16<-dummy_cols(p16, select_columns = 'week')

# Sort the new columns by the order of the weeks
p16<-p16[stringi::stri_order(sub(".*_", "", names(p16)), numeric=TRUE)]

# Now collapse rows on camera ID. I'll take the max of the columns, so if there is a 1 for that week it'll stay 1, otherwise 0
occ_pig16<-p16%>%
  group_by(camID)%>%
  summarise_at(.vars = names(p16)[grepl('week_', names(p16))], .funs = max)

#and add the year prefix
colnames(occ_pig16)<-paste('2016', colnames(occ_pig16), sep='_')
colnames(occ_pig16)[1]<-'camID'  #but not the first one
#########################################################################
# 2017
p17<-pigs[pigs$year==2017,]
p17<-dummy_cols(p17, select_columns = 'week')

# Sort the new columns by the order of the weeks
p17<-p17[stringi::stri_order(sub(".*_", "", names(p17)), numeric=TRUE)]

# Now collapse rows on camera ID. I'll take the max of the columns, so if there is a 1 for that week it'll stay 1, otherwise 0
occ_pig17<-p17%>%
  group_by(camID)%>%
  summarise_at(.vars = names(p17)[grepl('week_', names(p17))], .funs = max)

#and add the year prefix
colnames(occ_pig17)<-paste('2017', colnames(occ_pig17), sep='_')
colnames(occ_pig17)[1]<-'camID'  #but not the first one
#########################################################################
#########################################################################
# Now to put it all together

# Because all three datasets have different numbers of cameras accounted for, and different weeks as well, 
# I'll make a dataframe with the full set of cameras and there will be NA's turned to 0's

pigdf<-data.frame(camID=sort(unique(alldat$camID)))
pigdf<-left_join(pigdf, occ_pig15)
pigdf<-left_join(pigdf, occ_pig16)
pigdf<-left_join(pigdf, occ_pig17)
#write this out just to save, then change NA's to 0's and write out again.
setwd('C:/Users/apdwwolfson/Documents/Projects/Photo_Database/Machine_Learning/trained model output/Output')
write.csv(pigdf, 'Occupancy_format/pigdf.csv')



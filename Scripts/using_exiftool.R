#Extract metadata from images
library(V8)
library(lubridate)
library(suncalc)
library(tidyverse)

# a simple way
test_path<-'C:/Users/apdwwolfson/Documents/test_images'

db<-as.data.frame(system('C:/Users/apdwwolfson/Documents/ExifTool/exiftool -n -csv C:/Users/apdwwolfson/Documents/test_images/FL-01_0084371.JPG', 
           intern=TRUE))
#this works, but it's a clunky way of storing the output


out<-system('C:/Users/apdwwolfson/Documents/ExifTool/exiftool -n -csv C:/Users/apdwwolfson/Documents/test_images', 
                         intern=TRUE)
out.df<-read.csv(textConnection(out), stringsAsFactors = FALSE, header=F)
names(out.df)
#################################################################################
# A better way to do it

#Set working directory to where the images are
setwd('C:/Users/apdwwolfson/Documents/test_images')

#define exif function          ###!!!  only works on my local path
get.exif<-function(filename){
  command<-paste('C:/Users/apdwwolfson/Documents/ExifTool/exiftool -n -csv',   #modify to where your exiftool exe is located
                 paste(shQuote(filename), collapse=" "))  #just in case there are spaces in the filenames, this will collapse them
  read.csv(textConnection(system(command, intern=TRUE)),
           stringsAsFactors = FALSE, skip=c(1,2))      
  }

#Define path where images are located (if within wd but not at top level)
test_path<-'C:/Users/apdwwolfson/Documents/test_images'






#load exif data from specified path
exifdata<-get.exif(list.files(path=test_path, full.names=T))
exifdata<-exifdata[,c('SourceFile', 'FileName', 'DateTimeOriginal')]
# need to worry about the timezone as well
###################################################################################################
# Now use the datetime info to create a column for night/day

getSunlightTimes(date = Sys.Date(), lat = 50.1, lon = 1.83, tz = "CET",
                 keep=c('sunriseEnd','sunsetStart')) #this is the general format

#  I'll need to write a function that does the following:
# 1) pulls off the date from each row of the exiftool output data.frame
# 2) enters it into the suncalc function along with the pre-saved lat/lon and timezone #will need identifiers for projects
# 3) an ifelse statement to say if date is more than sunriseEnd and less than sunsetStart, then DAY, otherwise NIGHT # maybe add a flag for ones that don't fit

############################################################################################
# First step by step, then into function

#test with 6 california images
exifdata$dt<-ymd_hms(exifdata$DateTimeOriginal, tz = 'US/Pacific')
exifdata$date<-as.Date(exifdata$dt)
c.lat<-34.9
c.lon<-(-118.7)
ca.sun<-getSunlightTimes(date=exifdata$date, lat=c.lat, lon=c.lon, tz='US/Pacific',
                         keep=c('sunriseEnd','sunsetStart'))
exifdata<-cbind(exifdata, ca.sun[,c('sunriseEnd', 'sunsetStart')])
exifdata$daynight<-ifelse(exifdata$dt>exifdata$sunriseEnd & exifdata$dt<exifdata$sunsetStart, 'day',
                          ifelse(exifdata$dt<exifdata$sunriseEnd | exifdata$dt>exifdata$sunsetStart, 'night', 'flag'))


get_daynight<-function(datetimes, tz, lat, lon){ #datetimes can be output of exiftool
  dt<-ymd_hms(datetimes, tz=tz)
  date<-as.Date(dt)
  sundb<-getSunlightTimes(date=date, lat=lat, lon=lon, tz=tz,
                          keep=c('sunriseEnd','sunsetStart'))
  sundb$daynight<-ifelse(dt>sundb$sunriseEnd & dt<sundb$sunsetStart, 'day',
                         ifelse(dt<sundb$sunriseEnd | dt>sundb$sunsetStart, 'night', 'flag'))
}

exifdata$daynight<-get_daynight(exifdata$DateTimeOriginal, tz='US/Pacific', lat=c.lat, lon=c.lon)
#looks like it works!


###########################################################################################################
#FL
#Set working directory to where the images are
setwd('C:/Users/apdwwolfson/Documents/Projects/Photo_Database/Machine_Learning/photos_for_exiftool/FL')

#Define path where images are located (if within wd but not at top level)
fl_path<-'C:/Users/apdwwolfson/Documents/Projects/Photo_Database/Machine_Learning/photos_for_exiftool/FL'

# exifdata<-get.exif(list.files(path=fl_path, full.names=T)) # r crashed when I ran this

command<-paste('C:/Users/apdwwolfson/Documents/ExifTool/exiftool -n -csv',   #modify to where your exiftool exe is located
               paste(shQuote(fl_path), collapse=" "))  #just in case there are spaces in the filenames, this will collapse them
exifdata_fl<-read.csv(textConnection(system(command, intern=TRUE)),
         stringsAsFactors = FALSE) 
exifdata_fl<-exifdata_fl[,c('SourceFile', 'FileName', 'DateTimeOriginal')]

exifdata_fl$daynight<-get_daynight(datetimes=exifdata_fl$DateTimeOriginal, lat=27.1, lon=(-81.1), tz='US/Eastern')
exifdata_fl$study<-"FL"

#write data
setwd('C:/Users/apdwwolfson/Documents/Projects/Photo_Database/Machine_Learning/trained model output/Output/csvs_with_daynight')
write.csv(exifdata_fl, 'fl_daynight.csv')
############################################################
#Combine all
ca<-read_csv('ca_daynight.csv')
ca<-ca[,-1]
cpw<-read_csv('cpw_daynight.csv')
cpw<-cpw[,-1]
srel<-read_csv('srel_daynight.csv')
srel<-srel[,-1]
full_df<-rbind(exifdata_fl, ca, cpw, srel)
apply(is.na(full_df),2,sum)
full_df<-na.omit(full_df)
write.csv(full_df, 'full_df.csv')
##############################################################
#Now add on to the test results table
setwd('N:/Wildlife-Livestock-Health-Team/NationalFeralSwineProgram/Projects/Automated Camera Trap Classification')
res<-read_csv("test_results.csv")

res<-res%>%
  mutate(study=ifelse(imageID%in%grep("CPW", imageID, value=T), "CPW", 
                      ifelse(imageID%in%grep("NWRC", imageID, value=T), "NWRC",
                             ifelse(imageID%in%grep('ACAM', imageID, value=T),'SREL', 
                                    ifelse(imageID%in%grep('FL', imageID, value=T), 'FL',
                                           ifelse(imageID%in%grep('CA-', imageID, value=T), 'CA', 
                                                  ifelse(imageID%in%grep('Florida', imageID, value=T), 'FL',
                                                         ifelse(imageID%in%grep('TAG', imageID, value=T), 'CA', 
                                                                ifelse(imageID%in%grep('Tejon', imageID, value=T), 'CA', 'flag')))))))))

# can ignore nwrc for now, no exif metadata

#srel
srel<-res[res$study=='SREL',]
srel[1,2]
# strsplit('/lscratch/datasets/usda/resized_disk4_empty/Pooled_Images/10/ACAM202_100RECNX_IMG_0041.JPG', '/')[[1]][8]
srel$trim<-sapply(srel$imageID, function(x)
  strsplit(x, '/')[[1]][8])

#cpw
cpw<-res[res$study=='CPW',]
cpw[1,2]
# strsplit('/lscratch/datasets/usda/resized_CPW/3/2014_Unit160_Ivan092_img0367.jpg', '/')[[1]][7]
cpw$trim<-sapply(cpw$imageID, function(x)
  strsplit(x, '/')[[1]][7])

#ca
ca<-res[res$study=="CA",]
ca[1,2]
# paste(strsplit('/lscratch/datasets/usda/resized_1218/63/CA-37_08_03_2015_CA-37_0017365.jpg','_')[[1]][6],
#       strsplit('/lscratch/datasets/usda/resized_1218/63/CA-37_08_03_2015_CA-37_0017365.jpg','_')[[1]][7], sep="_")
ca$trim<-sapply(ca$imageID, function(x)
  paste(strsplit(x, '_')[[1]][6], 
        strsplit(x, '_')[[1]][7], sep='_'))

#fl
fl<-res[res$study=="FL",]
fl[1,2]
# paste(strsplit('/lscratch/datasets/usda/resized_1918/1/FL-28_07_28_2015_FL-28_0018595.jpg', '_')[[1]][6],
#       strsplit('/lscratch/datasets/usda/resized_1918/1/FL-28_07_28_2015_FL-28_0018595.jpg', '_')[[1]][7], sep='_')
fl$trim<-sapply(fl$imageID, function(x)
  paste(strsplit(x, '_')[[1]][6], 
        strsplit(x, '_')[[1]][7], sep='_'))
########################

#Now merge from day/night table onto the result table
# re-combine result table
r1<-rbind(srel, cpw, ca, fl)
names(r1)
names(full_df)
r1test<-left_join(r1, full_df, by=c('trim'='FileName'))
apply(is.na(r1test), 2, sum)
names(r1test)
colnames(r1test)[14]<-'study'
r1test[,19]<-NULL
write.csv(r1test, "test_results_with_daynight.csv")

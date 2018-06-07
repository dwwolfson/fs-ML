# Attach study names to model output
# May 23, 2018
library(tidyverse)
df<-read_csv("Data/May_23_18_output/L1WEN_test.csv", col_names = c('imageID', 'X2'))

df<-df%>%
  mutate(study=ifelse(imageID%in%grep("CPW", imageID, value=T), "CPW", 
                      ifelse(imageID%in%grep("NWRC", imageID, value=T), "NWRC",
                            ifelse(imageID%in%grep('ACAM', imageID, value=T),'SREL', 
                                   ifelse(imageID%in%grep('FL', imageID, value=T), 'FL',
                                      ifelse(imageID%in%grep('CA-', imageID, value=T), 'CA', 
                                             ifelse(imageID%in%grep('Florida', imageID, value=T), 'FL',
                                                ifelse(imageID%in%grep('TAG', imageID, value=T), 'CA', 
                                                    ifelse(imageID%in%grep('Tejon', imageID, value=T), 'CA', 'flag')))))))))

table(df$study)

#these are the images that were in the test set we need to extract info for
required_list<-as.vector(df[,1])

#########################################################
# I think I need to hack the partial string paths off to just get the filename that I can match against the full directories
#NWRC
nwrc<-df[df$study=='NWRC',]
nwrc[1,1]

#example
strsplit('resized_NWRC/WA-Site8_02997.JPG', '/')[[1]][2]


# Cut off everything before the first backslash
nwrc$trim<-sapply(nwrc$imageID, function(x)
  strsplit(x, '/')[[1]][2]
  )

#SREL
srel<-df[df$study=='SREL',]
srel[1,1]

#Need to chop off everything after the third backslash?
strsplit('resized_disk4_empty/Pooled_Images/10/ACAM202_100RECNX_IMG_0041.JPG', '/')[[1]][4]

srel$trim<-sapply(srel$imageID, function(x)
  strsplit(x, '/')[[1]][4]
  )

#CPW
cpw<-df[df$study=='CPW',]
cpw[1,1]
strsplit('resized_CPW/23/2015_Unit112_Ivan069_img0437.jpg', '/')[[1]][3]

cpw$trim<-sapply(cpw$imageID, function(x)
  strsplit(x, '/')[[1]][3]
  )

#CEAH
ca<-df[df$study=='CA',]
ca[1,1]
paste(strsplit('resized_1218/63/CA-37_08_03_2015_CA-37_0006618.jpg', '_')[[1]][6],
      strsplit('resized_1218/63/CA-37_08_03_2015_CA-37_0006618.jpg', '_')[[1]][7],sep='_')

ca$trim<-sapply(ca$imageID, function(x)
  paste(strsplit(x, '_')[[1]][6],
        strsplit(x, '_')[[1]][7],sep='_'))

fl<-df[df$study=="FL",]
fl[1,1]

paste(strsplit('resized_1918/1/FL-28_07_28_2015_FL-28_0018595.jpg', '_')[[1]][6],
       strsplit('resized_1918/1/FL-28_07_28_2015_FL-28_0018595.jpg', '_')[[1]][7], sep='_')
fl$trim<-sapply(fl$imageID, function(x)
  paste(strsplit(x, '_')[[1]][6],
        strsplit(x, '_')[[1]][7],sep='_'))
################################# 
#Put back together into one big vector of names that I need to copy the files for
to_get<-c(ca$trim, fl$trim, nwrc$trim, srel$trim, cpw$trim)


# maybe list all the directories where the images could be?
ca_path<-"E:/Unclassified_Images/CA" #this will need to be run recursively
fl_path<-"E:/Unclassified_Images/FL" #same deal
srel_path<-"E:/SREL photos/SREL_Pooled_Images" #these are all in the same folder
cpw_path<-"E:/archive/CPW/full_size_images" #these are all in the same folder
nwrc_path<-"E:/archive/NWRC_first_round_no_counts/pooled_images_resized" #one folder

path_list<-c(ca_path, fl_path, srel_path, cpw_path, nwrc_path)
names_to_match<-list(ca$trim, fl$trim, srel$trim, cpw$trim, nwrc$trim)
dest<-"C:/Users/apdwwolfson/Documents/Projects/Photo Database/Machine Learning/photos_for_exiftool"

# for(i in 1:length(path_list)){
#   setwd(path_list[i])
#   files.tmp<-list.files(recursive = T)
#   to_copy_tmp<-files.tmp[match(names_to_match[[i]], files.tmp)] #have to have [[]] instead of [] to get vector instead of list
#   file.copy(to_copy_tmp, dest)
# }

#This seems to run forever without copying
######################################################

# I'll do it one at a time
#SREL
i=3

  setwd(path_list[i])
  files.tmp<-list.files(recursive = T)
  to_copy_tmp<-files.tmp[match(names_to_match[[i]], files.tmp)] #have to have [[]] instead of [] to get vector instead of list
  file.copy(to_copy_tmp, dest)

# running i=3, SREL 5/30 noonish; finished at ~12:30
  ######################################################
# running CPW
i=4
dest<-"C:/Users/apdwwolfson/Documents/Projects/Photo Database/Machine Learning/photos_for_exiftool/CPW"
setwd(path_list[i])
files.tmp<-list.files()                                       #ran almost instantly (once scan was ended)
to_copy_tmp<-files.tmp[match(names_to_match[[i]], files.tmp)] #ran almost instantly (once scan was ended)
file.copy(to_copy_tmp, dest)    #started 1:40; done at 2:08

######################################################
# NWRC photos don't still have metadata becuase they were resized
#####################################################################
#California images 

i=1
dest<-"C:/Users/apdwwolfson/Documents/Projects/Photo Database/Machine Learning/photos_for_exiftool/CA"
  setwd(path_list[i])
  files.tmp<-list.files(recursive = T)  #started 2:15; done by 2:30
  #have to chop off part of name 
  #strsplit("CA-01/02_11_2016/CA-01_0006500.jpg", '/')[[1]][3]
  
  #takes off path for name matching
  new.tmp<-sapply(files.tmp, function(x)
    strsplit(x, '/')[[1]][3])
  
  prefix<-sapply(files.tmp, function(x)
    paste(strsplit(x, '/')[[1]][1],
          strsplit(x, '/')[[1]][2], sep='/'))
  
  test.db<-cbind.data.frame(files.tmp, new.tmp, prefix)
  ind<-match(names_to_match[[i]], new.tmp)
  to_export<-as.character(test.db[ind,'files.tmp'])
  file.copy(to_export, dest) 
















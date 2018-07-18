#  Create Occupancy Table

##############################################
setwd('C:/Users/apdwwolfson/Documents/Projects/Photo_Database/Machine_Learning/trained model output/Data/all_CEAH')
alldat<-read_csv('all_results_with_dates_6_27_18_DW.csv')
#still need to pull out the camera ID information though to have a separate row for each camera
alldat$FileName<-as.character(alldat$FileName)
alldat$camID<-sapply(alldat$FileName, function(x)
  strsplit(x, "_")[[1]][1])



###
# my botched attempt at turning all of this into a function
# occ_db<-function(df, sp, thresh){
#    df<-df[df$GUESS1==sp,]  #filter by species
#    df<-df[df$CONFIDENCE1>thresh,] #filter by confidence level
#    years<-unique(df[,'year'])
#    db<-data.frame()
#    for(i in 1:length(years)){
#      tmp<-df[df$year==i,]
#      tmp<-dummy_cols(tmp, select_columns = 'week')
#      tmp<-tmp[stringi::stri_order(sub(".*_", "", names(tmp)), numeric=TRUE)]
#      tmp<-tmp%>%group_by(camID)%>%
#        summarise_at(.vars=names(tmp)[grepl('week_', names(tmp))], .funs = max)
#      colnames(tmp)<-paste('i', colnames(tmp), sep='_')
#      db<-cbind.data.frame(db, tmp)
#    }
#   }
# 
# test<-occ_db(alldat, 22, 0.95)

# The above doesn't work, not exactly sure why not.

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
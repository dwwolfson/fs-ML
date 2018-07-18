# Sensitivity Analysis of how much data is being dropped
library(tidyverse)
library(gridExtra)

setwd('C:/Users/apdwwolfson/Documents/Projects/Photo_Database/Machine_Learning/trained model output/Data/all_CEAH')
alldat<-read_csv('all_results_with_dates_6_27_18_DW.csv')
#still need to pull out the camera ID information though to have a separate row for each camera
alldat$FileName<-as.character(alldat$FileName)
alldat$camID<-sapply(alldat$FileName, function(x)
  strsplit(x, "_")[[1]][1])

# Define a vector of confidence thesholds to use
# Write a loop that filters at each of these and fills a dataframe, one point for each threshold

# First just for pigs
pigdb<-alldat[alldat$GUESS1==22,]
tot<-nrow(pigdb)


threshs<-seq(.2, 0.99, 0.01)
filtdb<-data.frame(threshold=threshs, prop_remaining=NA)
for(i in 1:length(threshs)){
  tmpdb<-pigdb[pigdb$CONFIDENCE1>threshs[i],]
  tmp<-nrow(tmpdb)/tot
  filtdb[i, 2]<-tmp
}

pigs_gg<-ggplot(filtdb, aes(threshold, prop_remaining))+
  geom_point()+
  ylim(0,1)+
  xlab("Confidence threshold below which we don't make predictions")+
  ylab('Percent of Data')+
  ggtitle("Wild Pig (286,124 total images)")+
  theme(plot.title=element_text(hjust=0.5))
################

# Now for everything, and maybe some other species
# maybe cattle, mule deer, mountain lion
threshs<-seq(.2, 0.99, 0.01)
filtdb_all<-data.frame(threshold=threshs, prop_remaining=NA)
tot_all<-nrow(alldat)
df<-alldat
for(i in 1:length(threshs)){
  tmpdb<-df[df$CONFIDENCE1>threshs[i],]
  tmp<-nrow(tmpdb)/tot_all
  filtdb_all[i, 2]<-tmp
}
all_spp_gg<-ggplot(filtdb_all, aes(threshold, prop_remaining))+
  geom_point()+
  ylim(0,1)+
  xlab("Confidence threshold below which we don't make predictions")+
  ylab('Percent of Data')+
  ggtitle("Entire FL/CA Dataset (4,937,847 total images)")+
  theme(plot.title=element_text(hjust=0.5))

#####
# Cows
cowdb<-alldat[alldat$GUESS1==1,]
tot_cow<-nrow(cowdb)
threshs<-seq(.2, 0.99, 0.01)
filtdb_cow<-data.frame(threshold=threshs, prop_remaining=NA)


for(i in 1:length(threshs)){
  tmpdb<-cowdb[cowdb$CONFIDENCE1>threshs[i],]
  tmp<-nrow(tmpdb)/tot_cow
  filtdb_cow[i, 2]<-tmp
}
cow_gg<-ggplot(filtdb_cow, aes(threshold, prop_remaining))+
  geom_point()+
  ylim(0,1)+
  xlab("Confidence threshold below which we don't make predictions")+
  ylab('Percent of Data')+
  ggtitle("Cattle (3,079,474 total images)")+
  theme(plot.title=element_text(hjust=0.5))

#####
# Mountain Lion
mtn_liondb<-alldat[alldat$GUESS1==20,]
tot_mtn_lion<-nrow(mtn_liondb)
threshs<-seq(.2, 0.99, 0.01)
filtdb_mtn_lion<-data.frame(threshold=threshs, prop_remaining=NA)


for(i in 1:length(threshs)){
  tmpdb<-mtn_liondb[mtn_liondb$CONFIDENCE1>threshs[i],]
  tmp<-nrow(tmpdb)/tot_mtn_lion
  filtdb_mtn_lion[i, 2]<-tmp
}
mtn_lion_gg<-ggplot(filtdb_mtn_lion, aes(threshold, prop_remaining))+
  geom_point()+
  ylim(0,1)+
  xlab("Confidence threshold below which we don't make predictions")+
  ylab('Percent of Data')+
  ggtitle("Mountain Lion (24,396 total images)")+
  theme(plot.title=element_text(hjust=0.5))

########
# Deer
deerdb<-alldat[alldat$GUESS1==15,]
tot_deer<-nrow(deerdb)
threshs<-seq(.2, 0.99, 0.01)
filtdb_deer<-data.frame(threshold=threshs, prop_remaining=NA)


for(i in 1:length(threshs)){
  tmpdb<-deerdb[deerdb$CONFIDENCE1>threshs[i],]
  tmp<-nrow(tmpdb)/tot_deer
  filtdb_deer[i, 2]<-tmp
}
deer_gg<-ggplot(filtdb_deer, aes(threshold, prop_remaining))+
  geom_point()+
  ylim(0,1)+
  xlab("Confidence threshold below which we don't make predictions")+
  ylab('Percent of Data')+
  ggtitle("Deer (168,227 total images)")+
  theme(plot.title=element_text(hjust=0.5))

########
# Bobcat
bobcatdb<-alldat[alldat$GUESS1==13,]
tot_bobcat<-nrow(bobcatdb)
threshs<-seq(.2, 0.99, 0.01)
filtdb_bobcat<-data.frame(threshold=threshs, prop_remaining=NA)


for(i in 1:length(threshs)){
  tmpdb<-bobcatdb[bobcatdb$CONFIDENCE1>threshs[i],]
  tmp<-nrow(tmpdb)/tot_bobcat
  filtdb_bobcat[i, 2]<-tmp
}
bobcat_gg<-ggplot(filtdb_bobcat, aes(threshold, prop_remaining))+
  geom_point()+
  ylim(0,1)+
  xlab("Confidence threshold below which we don't make predictions")+
  ylab('Percent of Data')+
  ggtitle("Bobcat (53,568 total images)")+
  theme(plot.title=element_text(hjust=0.5))


grid.arrange(all_spp_gg, cow_gg, deer_gg, pigs_gg, mtn_lion_gg, bobcat_gg)

setwd("C:/Users/apdwwolfson/Documents/Projects/Photo_Database/Machine_Learning/trained model output/Output")
pdf("Threshold_Sensitivity.pdf", width=8.5, height=11 )
grid.arrange(all_spp_gg, cow_gg, deer_gg, pigs_gg, mtn_lion_gg, bobcat_gg)
dev.off()

summary(pigdb$CONFIDENCE1)
summary(df$CONFIDENCE1)

names(alldat)
unique(alldat$study)
fl<-alldat[alldat$study=='florida',]
summary(fl$DateTimeOriginal)

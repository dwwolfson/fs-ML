#  Need to produce a table that contains:
library(tidyverse)

# 1) Filename used for ML model
# 2) all the columns of ML output
# 3) UnkID that was produced with the APHIS query
# 4) SpeciesID column that matches the values in the APHIS Photo DB

# Import output of PhotoDB query
setwd('C:/Users/apdwwolfson/Documents/Projects/Photo_Database/Machine_Learning/trained model output/Data/output_of_sql_query')
qry<-read_csv('qryAPHIS.csv', col_names = FALSE)
colnames(qry)<-c('filename', 'UnkID', 'junk1', 'speciesID', 'always1', 'mark_status', 'db_path')
# I'll need to match these filenames, UnkID, and the species labels
qry<-qry[,c('filename', 'UnkID', 'speciesID')]

# Import ML model output
# This contains more than just the initial photodb output, it's 
setwd('C:/Users/apdwwolfson/Documents/Projects/Photo_Database/Machine_Learning/trained model output/Data')
ml<-read_csv('all_CEAH/all_results_with_dates_6_27_18_DW.csv')
colnames(ml)[2]<-'filename'

head(qry[1])
head(ml[2])

#adjust filenames from query
paste(strsplit('FL-01_06_01_2015_FL-01_0000001.jpg', '_')[[1]][5],
      strsplit('FL-01_06_01_2015_FL-01_0000001.jpg', '_')[[1]][6], sep='_')
colnames(qry)[1]<-'old_filename'
qry$filename<-sapply(qry$old_filename, function(x)
                paste(strsplit(x, '_')[[1]][5],
                      strsplit(x, '_')[[1]][6], sep='_'))

mergedb<-left_join(qry, ml)
apply(is.na(mergedb), 2, sum)
df<-na.omit(mergedb)
head(df)
df<-df[,-5]
df<-df[,-5]

# Now just need to make sure the species labels can go back into the photo db
table(df$GUESS1)
table(df$speciesID)

# write out
setwd('C:/Users/apdwwolfson/Documents/Projects/Photo_Database/Machine_Learning/trained model output')
# write.csv(df, 'Output/output_for_Newkirk_7_3_18/df.csv')


# Species labels that match well
# ML_code - ML_name  -  PhotoDB name 
# 1       -  Cattle  -  Bos Taurus
# 2       -  Quail   -  Callipepla californica
# 4       -  Elk     -  Cervus canadensis
# 6       -  Corvid  -  Corvus spp
# 7       -  Armadillo - Daypus novemcinctus
# 8       -  Turkey  -   Meleagris gallopavo
# 9       -  Opossum -  Possum
# 11      -  Human   -  Homo sapiens  #but this includes 'Ranch_person_on_horse_foot'
# 13      -  Bobcat  -  Lynx rufus
# 14      -  Str_Skunk - Mephitis mephitis
# 15      -  Deer spp -  Odocoileus spp
# 19      -  Raccoon - Procyon lotor
# 20      - Mtn Lion - Puma concolor
# 22      - Wild Pig - Sus scrofa
# 24      - Black bear - Ursus americanus
# 25      - Vehicle  - Vehicle truck
# 26      - Bird     - Aves


# Doesn't work so well
# 1) "Horse" stuff
# PhotoID 20 (Ranch Person on Foot_Horse) was coded as 'Homo sapiens', 
# but PhotoID 58 (Horse), didn't have latin, so those pictures weren't used,
# and PhotoID 54 (Donkey) was coded as Equus africanus, and used in ML as #10, Equus spp.

# 2) Moose (ML#0), not in PhotoID
# 3) Canidae (ML#3), grouped spp
# 4) Mustelidae (ML#5), grouped spp
# 5) Rabbits (ML#12), grouped spp
# 6) Rodent (ML#16), grouped spp
# 7) Mule Deer (ML#17), not in PhotoDB
# 8) White-tailed Deer (ML#18), not in PhotoDB
# 9) Squirrel (ML#21), grouped spp

# Sorta works?
# -Fox (ML#23) had grey and red fox, but PhotoDB only had grey fox, probably doesn't matter??





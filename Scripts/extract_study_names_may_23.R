# Attach study names to model output
# May 23, 2018
library(tidyverse)
df<-read_csv("Data/May_23_18_output/L1WEN_train.csv", col_names = c('imageID', 'X2'))

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

write.csv(df, "Output/May_23_2018/L1WEN_train.csv")


fl<-df%>%
  filter(study=='FL')
table(fl$X2)


cats<-read_csv('Data/categories.csv')




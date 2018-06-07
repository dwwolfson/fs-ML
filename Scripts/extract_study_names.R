# Attach study names to model output
library(tidyverse)
df<-read_csv("Data/test_results.csv")

table(df$answer)

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

result_tallies<-df%>%
  group_by(study)%>%
  count(answer)

cats<-read_csv('Data/categories.csv')
cats$className<-recode(cats$className, "Red Deer"="Elk")


result_tallies<-left_join(result_tallies, cats,by=c('answer'='Class_ID'))

test1<-result_tallies%>%
  arrange(study, desc(n))%>%
  mutate(order=row_number())



ggplot(result_tallies, aes(className,n))+
  geom_bar(stat='identity', fill='blue')+
  coord_flip()+facet_wrap(~study, scales='free')+theme_bw()

write.csv(df, "Output/test_output_with_names.csv")
write.csv(test1, "Output/test_sp_tallies.csv")

#Doesnt work (below)

# test<-result_tallies%>%
#   arrange(study, desc(n))%>%
#   mutate(order=row_number())
# 
# 
# ggplot(test, aes(order,n))+
#   geom_bar(stat='identity', fill='blue')+
#   coord_flip()+facet_wrap(~study, scales='free')+
#   theme_bw()+
#   scale_x_continuous(
#     breaks=test$order,
#     labels=test$className,
#     expand=c(0,0)
#     )















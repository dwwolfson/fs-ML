# A function for taking the output of the Machine Learning model (MLWIC) and turn it into a table of 0/1 for occupancy analyses.

occ_db<-function(df,species, thresh, time_int){ 
  # df must be a dataframe with variable, camID
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

##############################################################################################################################
# A function for adding NA's into the occupancy table when the camera isn't active (instead of 0's which would indicate the focal sp wasn't present)

insert_NA<-function(occ_dataframe,full_dataframe, time_int){
  #time_int needs to be either week, day, or hour for this function, can't be month (although I could write in month to just be equal to 30 days...)
  # 1) First I'll probably want to pull out all the deployment info for all the cameras
  camDates<-full_dataframe%>%
    group_by(camID)%>%
    summarise(start_date=min(DateTimeOriginal), 
              start_yr=year(start_date),
              end_date=max(DateTimeOriginal), 
              end_yr=year(end_date))
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
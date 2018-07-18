# ML output to Occupancy table helper function

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
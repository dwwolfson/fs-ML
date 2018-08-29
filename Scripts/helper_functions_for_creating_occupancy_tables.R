# A function for taking the output of the Machine Learning model (MLWIC) and turn it into a table of 0/1 for occupancy analyses.

occ_db<-function(df,species, thresh, time_int){ 
  # df must be a dataframe with variables[camID=]
  # species must be the numeric label corresponding to the desired species ID from lookup table
  # thresh must be the cutoff for confidence values (between 0 and 1)
  # time_in must be either 'day', 'week', or 'month'
  if(!time_int%in%c("day", "week", "month")){
    stop("Error: Input 'time_int' needs to be either day, week, or month")
  }
  allcams<-unique(df$camID)
  df<-df[df$GUESS1==species,]    #filter by species of interest
  df<-df[df$CONFIDENCE1>thresh,] #filter by confidence level threshold below which images aren't used
  years<-sort(unique(unlist(df[,"year"]))) # get vector of years present
  db<-as.data.frame(matrix(nrow=length(allcams))) #create dataframe with correct dimensions to fill with info
  db[,1]<-sort(unique(allcams))    #arrange cameras in numeric order
  for(i in 1:length(years)){
    tmp<-df[df$year==years[[i]],]   #grab subset by year
    tmp<-fastDummies::dummy_cols(tmp, select_columns = time_int)  #make dummy columns by preferred time interval
    tmp<-tmp[stringi::stri_order(sub(".*_", "", names(tmp)), numeric=TRUE)] #order the time interval columns
    tmp<-tmp%>%group_by(camID)%>%
      summarise_at(.vars=names(tmp)[grepl(paste0(time_int, '_'), names(tmp))], .funs = max) #collapse rows into 0/1
    # db$camID<-tmp$camID #set first column as the camera ID's
    
    if(!length(db[,1])==length(tmp$camID)){  #check for missing camera names and add them in with NA's
      missing<-setdiff(db[,1], tmp$camID)
      if(length(missing)==1){ # the structure of what to do is different if only one camera is missing vs multiple
        rowNum<-grep(pattern = missing, x = db[,1])   #find which cam is missing
        newRow<-c(missing, as.numeric(rep(NA, ncol(tmp)-1)))   # combine cam name with correct dim's of NA's
        tmp[seq(rowNum+1, nrow(tmp)+1),]<-tmp[seq(rowNum, nrow(tmp)),] #this creates an extra row so 
        # the dataframe has the proper dimensions and duplicates a row at the point where a missing 
        # one should be added
        tmp[rowNum,]<-newRow #then this adds the missing row at the right place, overwriting the copied row
        tmp[,-1]<-as.data.frame(sapply(tmp[,-1], as.numeric)) #need the columns to be numeric instead of character
      }else if(length(missing)>1){  #if there are multiple cameras that are missing from the yearly subselection...
        rowNums<-NA    #create blank object
        for(j in 1:length(missing)){
          rowNums[j]<-grep(pattern=missing[j], x = db[,1]) #collect the rows (cameras) that are missing
        }
        for(k in 1:length(rowNums)){
          tmprow<-c(missing[k], as.numeric(rep(NA, ncol(tmp)-1)))
          if(rowNums[k]<=nrow(tmp)){
            tmp[seq(rowNums[k]+1, nrow(tmp)+1),]<-tmp[seq(rowNums[k], nrow(tmp)),]
            tmp[rowNums[k],]<-tmprow
          } else if(rowNums[k]>nrow(tmp)){
            tmp[rowNums[k],]<-tmprow
          } 
          tmp[,-1]<-as.data.frame(sapply(tmp[,-1], as.numeric)) #prevent the summation from bonking because rows were characters
        }
      }
    }
    tmp<-tmp%>%select(-camID)%>%
      setNames(paste(years[[i]], names(tmp[-1]), sep='_'))  #add year prefix to the column names
    db<-cbind.data.frame(db,tmp)                            #add to dataframe
    j<-k<-NULL #reset counters
  }
  # db<-db[,-1] #cut off the first junk column which is empty
  return(db)
}

##############################################################################################################################
add_zero_cols<-function(occ_df, cam_set_dates, camID , time_int, dates_full_df){
  # 'occ_df' is an occupancy table (of 0/1 format), as produced by the occ_db function
  # 'dates_full_df' need to be a vector of dates in POSIXct format for date/times of the full dataset (not filtered for focal sp)
  #  'cam_set_dates' need to be a vector of dates in POSIXct format for when cameras were set up
  # camID should be a vector of camera names that corresponds to the column for camera ID in the occupancy table being used
  if(!is.POSIXct(cam_set_dates)){
    stop("Error: Input 'cam_set_dates' needs to be in POSIXct format")}
  if(!is.POSIXct(dates_full_df)){
    stop("Error: Input 'dates_full_df' needs to be in POSIXct format")}
  if(!time_int%in%c("hour", "day", "week")){
    stop("Error: Input 'time_int' needs to be either hour, day, or week")
  }
  global_range<-c(min(cam_set_dates), max(dates_full_df))
  first_day<-yday(global_range[1])
  last_day<-yday(global_range[2])
  years<-sort(unique(year(dates_full_df)))
  db<-as.data.frame(matrix(nrow=length(unique(camID)))) #create dataframe with correct dimensions to fill with info
  # AT THIS POINT WILL ONLY WORK FOR A SINGLE YEAR AT A TIME; THIS CAN BE CHANGED LATER#
  for(i in 1:length(years)){
    tmp<-occ_df[, grep(pattern = years[i], x = names(occ_df))]
    # if this year subset is the first year of active dates, then first day should be the minimum camera set date,
    # if this year subset isn't the first year of activate dates, then first day should be 1
    # if this year subset is the last year of active dates, then last day should be the last date in the full dataset (max(dates_full_df))
    # if this year subset isn't the last year of active dates, then last day should be 365 (?)
    if(years[i]==year(global_range[1])){
      yr_day_start<-first_day
    }else{ yr_day_start<-1}
    if(years[i]==year(global_range[2])){
      yr_day_end<-last_day
    }else{yr_day_end<-365}
    intervals<-paste(years[i], time_int, seq(from=yr_day_start, to=yr_day_end, by=1), sep='_')
    cols_to_add<-setdiff(intervals,names(tmp))
    tmp[cols_to_add]<-0 #these should be 0 because cam's were active, if there are periods at beginning and
    # end where cameras weren't active the next function (insert_NA) should correct it
    tmp<-tmp[stringi::stri_order(sub(".*_", "", names(tmp)), numeric=TRUE)]
    db<-cbind.data.frame(db, tmp)
  }
  return(db)
}

##############################################################################################################################
# A function for adding NA's into the occupancy table when the camera isn't active (instead of 0's which would indicate the focal sp wasn't present)

insert_NA<-function(occ_dataframe,full_dataframe, time_int){
  #time_int needs to be either week, day, or hour for this function, can't be month (although I could write in month to just be equal to 30 days...)
  if(!is.POSIXct(full_dataframe$DateTimeOriginal)){
    stop("Error: Input 'full_dataframe$DateTimeOriginal' needs to be in POSIXct format")
  }
  if(!time_int%in%c("hour", "day", "week")){
    stop("Error: Input 'time_int' needs to be either hour, day, or week")
  }
  # 1) First I'll probably want to pull out all the deployment info for all the cameras
  camDates<-full_dataframe%>%  #note this only reflects
    group_by(camID)%>%
    summarise(start_date=min(DateTimeOriginal), 
              start_yr=year(start_date),
              start_wk=week(start_date),
              start_day=yday(start_date),
              start_yr_wk=paste(start_yr, start_wk, sep='_'),
              end_date=max(DateTimeOriginal), 
              end_yr=year(end_date),
              end_wk=week(end_date),
              end_day=yday(end_date),
              end_yr_wk=paste(end_yr, end_wk, sep='_'))
  # 2) Now I'll need to compare the deployment dates of each camera to the overall range of dates 
  #    that the occupancy table is compiling information for.
  date_range<-c(min(full_dataframe$DateTimeOriginal), max(full_dataframe$DateTimeOriginal)) # except these are different time zones
  # First convert camDates to data.frame for it to work nicely with the ifelse statement
  camDates<-as.data.frame(camDates)
  #3) Now I need a way to determine if 1) the start date of the cam is after the global start, and by how much;
  # and 2) if the end data of the cam is before the global end date
  for(i in 1:nrow(camDates)){
    if(camDates[i,"start_yr"]!=lubridate::year(date_range[1])){
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
    if(camDates[i, "end_yr"]!=lubridate::year(date_range[2])){
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
      num_underscores<-stringr::str_count(last(names(occ_dataframe)), '_')
      last_col<-as.numeric(strsplit(last(names(occ_dataframe)), '_')[[1]][num_underscores+1])+round(e.diff.units)
      occ_dataframe[i, grep(last_col, names(occ_dataframe)):ncol(occ_dataframe)]<-NA 
      #this makes the last time interval that the camera has NA as well because it's a partial interval
    }
  }
  return(occ_dataframe)
}
# Want to run classify on all cameras in a check in one batch job
library(tidyverse)


convert<-function(ml_out, cpw){
  ml_out$FileName<-sapply(ml_out$fileName, function(x)
    sub(".*/", "", x), USE.NAMES = FALSE)
  ml_out$FileName<-gsub('.{1}$', '', ml_out$FileName)
  ml_out[,c('X','X1', 'fileName', 'answer')]<-NULL # X or X1 depending on read.csv vs read_csv
  names(ml_out)<-str_to_title(names(ml_out))
  colnames(ml_out)[11]<-'FileName'
  cpw$FileName<-as.character(cpw$FileName) #needed if imported by read.csv instead of read_csv
  
  # now names match, but it's easier to do the join if the columns aren't in the cpw dataframe
  cpw<-cpw[,c('ImageID', 'FileName')]
  cpw<-left_join(cpw, ml_out, by="FileName")
  return(cpw)
}



# IMPORTANT: Make sure that in the 'root_to_image_dirs' folder there are only folders for each camera, nothing else
# The folders should be named FL01, FL02, etc (with study site included)
#
# check_num should be an integer
#
# model_dir should be the folder where the L1 folder resides (aka a level above L1)
# 
# python_path should be the folder above where the python exe file resides (aka a level above)
# 


batch_classify<-function(root_to_image_dirs, check_num, model_dir, python_loc){
  cam_names<-list.files(root_to_image_dirs)
  for(i in 1:length(cam_names)){
    classify(data_info = paste(root_to_image_dirs,cam_names[i], "Image_Labels.csv", sep="/"), 
           model_dir = model_dir,
           python_loc = python_loc,
           save_predictions = paste(root_to_image_dirs, cam_names[i], "raw_out.txt", sep="/"),
           path_prefix = paste(root_to_image_dirs, cam_names[i], "Resized", sep="/"))
    
    make_output(output_location = paste(root_to_image_dirs, cam_names[i], sep="/"), 
                model_dir=model_dir,
                output_name = paste(cam_names[i], paste0("check", check_num), "clean_out.csv", sep="_"),
                saved_predictions = paste(root_to_image_dirs, cam_names[i], "raw_out.txt", sep="/"))
              
    ml<-read_csv(paste(root_to_image_dirs, cam_names[i],paste(cam_names[i], paste0("check", check_num),
                        "clean_out.csv", sep="_"), sep="/"))
    
    cpw<-read_csv(paste(root_to_image_dirs, cam_names[i], "Import.csv", sep="/"))
    
    to_import<-convert(ml, cpw)
    
    write_csv(to_import, paste(root_to_image_dirs, cam_names[i], 
                               paste(cam_names[i], paste0("check", check_num), 
                                     "to_import.csv", sep="_"), sep="/"))
  }
}






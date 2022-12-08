## Intro to Biocomputing Final R Project: Supporting Functions
## Clayton Glasgow, Sydney Harris, Jaden Bailey
## 14 December 2022

# set working directory


# function to compile data from all csv files in a directory into a single csv file
# compile_csv(dir, type, place, start = 8, stop = 10, na_rm): 
# "dir" = path to directory of interest
# "type" = file type to include in compilation ("csv", "txt", etc)
# "place" = place to include in added "country" column ("X" or "Y")
# "start" = start point to extract information from file name to include as a new column (day of year of screening)
# "stop" = end point to "start"; the defaults (start = 8, stop = 10) extract the day of screening from each file name
# "na_rm" = manage NA values within resulting dataframe
#           na_rm = "remove": rows with NA values are removed
#           na_rm = "warn": if NA values are present, a warning message appears
#           na_rm = "include": NA values are included without a warning
compileFiles <- function(dir, type, place, start = 8, stop = 10, na_rm){
  # create a list of files from the directory of interest
  file_list <- list.files(dir, type)
  # read in first file in file_list; this will be the dataframe to which all
  # subsequent csv's will be appended
  all <- read.csv(file_list[1], header = TRUE)
  # add country column
  all$country = place
  # add dayofYear column (day of year of screening)
  all$dayofYear = substr(file_list[1], start = start, stop = stop)
  # loop through the rest of file_list, appending each csv to the bottom of "all"
  for(file in 2:length(file_list)){
      # read in next csv file in file_list
      screen <- read.csv(file_list[file], header = TRUE)
      # add country column
      screen$country <- place
      # add dayofYear column (day of year of screening)
      screen$dayofYear <- substr(file_list[file], start = start, stop = stop)
      # append ith csv file to all csv dataframe
      all <- rbind(all, screen)
      # remove the unnecessary objects to save memory
      rm(screen)
  }
  # include optional NA warning/removal
  # if na_rm = "remove", rows with NA values are deleted from dataframe and the message
  # "NA values removed" is printer
  if(na_rm == "remove"){
    all <- na.omit(all)
    print("NA values removed")
  } # if na_rm = "warn", a warning appears if NA values are present in dataframe
    else if(na_rm == "warn"){
    if(any(is.na(all)) == TRUE){
      print("Warning: data contain NA values")
      # if no NA values are present, the message "No NA values" is printed
    } else{print("No NA values")}
  } # if na_rm = "include", NA values are included without a warning message
    else if(na_rm == "include"){
    all <- all
    }
  write.csv(all, file = paste(place, "_allData.csv", sep = ""), row.names = FALSE) 
}







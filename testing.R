# this function takes in a directory, and compiles all csv files into a single
# file
csv_join <- function(dir, naflag){
  # create blank matrix with the headers
  headers<-c("gender", "age", "marker01", "marker02","marker03","marker04","marker05","marker06","marker07","marker08","marker09","marker10", "country", "dayofYear")
  allData<-matrix(headers, ncol=14)
  
  # get all the files we are looping through
  files<-list.files(dir, pattern ="*.csv", full.names = TRUE, recursive = FALSE)
  
  # get country name
  country_name<-gsub("country", "", dir)
  
  # loop through each file, add to all data
  for (file in files){
    # create a temp var to read in file
    read<-read.csv(file, header = TRUE)
    
    # get the dayofYear
    dayofYear<-as.numeric(gsub(".*?([0-9]+).*", "\\1", file))
    
    # create a matrix that will be appended to our overall
    num_rows<-nrow(read)
    temp_mat<-matrix(nrow=num_rows, ncol=14)
    
    # keep the index
    index<-1
    
    # go through each row
    for (row in 1:nrow(read)){
      # check if the row has NA, we need a boolean for this
      hasNA<-FALSE
      na<-is.na(read[row,])
      for (item in na){
        # this means we found a NA value
        if (item == "TRUE"){
          # this flag is if you want to remove the row
          if (naflag == 1){
            hasNA<-TRUE
            break
          }
          # this flag is if you want to a warning
          else if (naflag == 2){
            cat("Warning: NA value on row ", row, " in file ", file, "\n")
            break
          }
          # we do not need to check for the other flag since they do not want
          # any warnings and want the row included
        }
      }
      # hasNA flag is set if we want to remove row, so we just skip this loop
      if (hasNA == "TRUE"){
        next
      }
      
      # get all the data from row
      data<-c(read[row,1], read[row,2], read[row,3], read[row,4], read[row,5], read[row,6], read[row,7], read[row,8], read[row,9], read[row,10], read[row,11], read[row,12])
      # append the data along with country and day to temp matrix
      temp_mat[index,]<-c(data, country_name, dayofYear)
      # increase index
      index<-index + 1
    }
    # need to check if we need to drop rows with NA again, since the way we did it
    # would leave rows full of NA
    if (hasNA == "TRUE"){
      temp_mat<-na.omit(temp_mat)
    }
    
    # combine the matrices
    allData<-rbind(allData, temp_mat)
  }
  return(allData)
  
}

allData<-csv_join("test", 1)
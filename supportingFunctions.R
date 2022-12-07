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
    # for the num_rows, we want to get rid of rows that have unrealistic age
    # which will be skipped later
    for (i in 1:nrow(read)){
      if (read[i,2] > 125){
        num_rows<-num_rows - 1
      }
    }
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
      # check the person's age, if not a real age, skip it
      if (read[row,2] > 125){
        next
      }
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
  # create the name for the file
  fileName<-paste("combinedData_", dir, ".csv", sep="")
  
  # write the file
  write.table(allData, file = fileName, sep=",", row.names=FALSE, col.names=FALSE)
  
}

# This function is used to combine csv files that are created using the csv_join
# function to make one big file that has multiple countries in it
# it takes in a directory and combines csv files that follow the name format 
# from the csv_join
combine_big_csv <- function(dir){
  # create blank matrix with the headers
  headers<-c("gender", "age", "marker01", "marker02","marker03","marker04","marker05","marker06","marker07","marker08","marker09","marker10", "country", "dayofYear")
  allData<-matrix(headers, ncol=14)
  
  # get all the files we are looping through
  files<-list.files(dir, pattern ="combinedData_.*.csv", full.names = TRUE, recursive = FALSE)
  
  # loop through all the files
  for (file in files){
    # create a temp var to read in file
    read<-read.csv(file, header = TRUE)
    
    # create a matrix that will be appended to our overall
    num_rows<-nrow(read)
    temp_mat<-matrix(nrow=num_rows, ncol=14)
    
    # keep the index
    index<-1
    
    # go through each row
    for (row in 1:nrow(read)){
      # get all the data from row
      data<-c(read[row,1], read[row,2], read[row,3], read[row,4], read[row,5], read[row,6], read[row,7], read[row,8], read[row,9], read[row,10], read[row,11], read[row,12], read[row,13], read[row,14])
      # append the data along with country and day to temp matrix
      temp_mat[index,]<-data
      # increase index
      index<-index + 1
    }
    allData<-rbind(allData, temp_mat)
  }
  
  # write the file
  write.table(allData, file = "allData_test.csv", sep=",", row.names=FALSE, col.names=FALSE)
  
}

# this function takes in a directory and converts the .txt files to .csv
txt_to_csv <- function(dir){
  # get all the files we are looping through
  files<-list.files(dir, pattern ="*.txt", full.names = TRUE, recursive = FALSE)
  
  # loop through the files
  for (file in files){
    read<-read.table(file, sep = "", header = FALSE)
    filename<-gsub(".txt", ".csv", file)
    write.table(read, file = filename, sep=",", row.names=FALSE, col.names=FALSE)
  }
}

# this function is used to get the summary info from the csv file with all the data
csv_summarize <- function(filepath){
  # read in the file
  my_data <- read.csv(file=filepath, header=TRUE, sep=',')
  # get the number of rows
  num_screens = nrow(my_data)
  
  # find the total number of screens which will be the number of rows
  total_num_screens = paste("Number of screens: ", num_screens, sep="")
  print(total_num_screens)
  
  # find the total infected count by going through each row and see if a marker
  # has a 1
  infected_count = 0
  for (row in 1:nrow(my_data)){
    if (my_data$marker01[row] == 1 || my_data$marker02[row] == 1 || my_data$marker03[row] == 1 || my_data$marker04[row] == 1 || my_data$marker05[row] == 1 || my_data$marker06[row] == 1 || my_data$marker07[row] == 1 || my_data$marker08[row] == 1){
      infected_count = infected_count + 1
    }
  }
  
  # get the infected percent
  infect_percent = paste("Percent of patients that were infected: ", infected_count/num_screens, sep="")
  print(infect_percent)
  
  
  # loop through the rows again and find how many people are male, female
  # and then the infected number for each
  num_males = 0
  num_females = 0
  num_infected_males = 0
  num_infected_females = 0
  for (row in 1:nrow(my_data)){
    if (my_data$gender[row] == 'female'){
      num_females = num_females + 1
      if (my_data$marker01[row] == 1 || my_data$marker02[row] == 1 || my_data$marker03[row] == 1 || my_data$marker04[row] == 1 || my_data$marker05[row] == 1 || my_data$marker06[row] == 1 || my_data$marker07[row] == 1 || my_data$marker08[row] == 1){
        num_infected_females = num_infected_females + 1  
      }  
    }
    else if (my_data$gender[row] == 'male'){
      num_males = num_males + 1
      if (my_data$marker01[row] == 1 || my_data$marker02[row] == 1 || my_data$marker03[row] == 1 || my_data$marker04[row] == 1 || my_data$marker05[row] == 1 || my_data$marker06[row] == 1 || my_data$marker07[row] == 1 || my_data$marker08[row] == 1){
        num_infected_males = num_infected_males + 1  
      }
    }
  }
  
  # creating the strings then printing out the results
  infect_females = paste("Percent of female patients that were infected: ", num_infected_females/num_females, sep="")
  ratio_females = paste("Ratio of female patients that were infected: ", num_infected_females, "/", num_females, sep="")
  infect_males = paste("Percent of male patients that were infected: ", num_infected_males/num_males, sep="")
  ratio_males = paste("Ratio of male patients that were infected: ", num_infected_males, "/", num_males, sep="")
  print(infect_females)
  print(ratio_females)
  print(infect_males)
  print(ratio_males)
  
  # cut off the x age values at 125, as there are a few outliers that make the graph difficult to read
  ggplot(my_data, aes(x=age)) + 
    geom_histogram(binwidth=1) 
    #geom_histogram(binwidth=1) + 
    #xlim(0, 125)
  
}

library(ggplot2)




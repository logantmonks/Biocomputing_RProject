#Authors: Catherine Andreadis, Shehani Fernando, Zhuoran Yu

#Country X and Y have different traditions for the delimiter in their data files. Write a function that
#converts all files in a directory with space- or tab-delimited data (.txt) into comma-separated value
#files.

#usage:convert_files_in_directory("path of directory")

convert_files_in_directory <- function(dir) {
  # Make a list of all .txt files in the specified directory
  files <- list.files(dir, pattern="*.txt", full.names=TRUE)
  
  # read in each file in the list through a loop
  for (file in files) {
    # Read the file into a data frame, using the specified delimiter
    df <- read.table(file, header=TRUE, sep=" ", stringsAsFactors=FALSE)
    
    # Write the data frame to csv with a same name
    write.csv(df, file=gsub(".txt$", ".csv", file),row.names = FALSE)
  }
}

#Write a function to compile data from all .csv files in a directory into a single .csv file. The compiled
#data should have the original twelve columns from daily data sheets, but also country and dayofYear
#columns. The user should be able to choose whether they want to remove rows with NA’s in any
#columns, include NAs in the compiled data but be warned of their presence, or include NAs in the
#compiled data without a warning

#usage: compile_all_csv("path of directory for first country","path of directory for second country",
#"country name for the first directory","country name for the second directory","choose from remove(remove all the rows with NA
#or warn (print out warnings that there are NAs in the file)))

compile_all_csv <- function(dir1, dir2, country_name_1,country_name_2,na_option) {
  # Get list of all .csv files in the specified directory
  country1 <- list.files(dir1, pattern = "*.csv", full.names = TRUE)
  country2 <- list.files(dir2, pattern = "*.csv", full.names = TRUE)
  
  # Create empty data frame to store compiled data
  compiledData <- data.frame()
  
  # Loop through all files in the directory and compile data
  for (file in country1) {
    
    # Read data from current file
    data1 <- read.csv(file)
    
    # Add country and dayOfYear columns to data
    data1$country <- country_name_1
    data1$dayofYear <- gsub(".*_|\\.csv$","",file)
    
    # Handle NAs according to specified option
    if (na_option == "remove") {
      # Remove rows with NA values
      data1 <- na.omit(data1)
    } else if (na_option == "warn") {
      # Check for NAs and print warning if present
      if (any(is.na(data1))) {
        print("WARNING: There are NAs in the data")
      }
    }
    
    # Append current data to compiled data frame
    compiledData <- rbind(compiledData,data1 )
  }
  
  # Loop through all files in the directory and compile data
  for (file in country2) {
    
    # Read data from current file
    data2 <- read.csv(file)
    
    # Add country and dayOfYear columns to data
    data2$country <- country_name_2
    data2$dayofYear <- gsub(".*_|\\.csv$","",file)
    
    # Handle NAs according to specified option
    if (na_option == "remove") {
      # Remove rows with NA values
      data2 <- na.omit(data2)
    } else if (na_option == "warn") {
      # Check for NAs and print warning if present
      if (any(is.na(data2))) {
        print("WARNING: There are NAs in the data")
      }
    }
    
    # Append current data to compiled data frame
    compiledData <- rbind(compiledData,data2 )
  }
  # Return compiled data frame
  return(compiledData)
}

#Write a function to summarize the compiled data set in terms of number of screens run, percent of
#patients screened that were infected, male vs. female patients, and the age distribution of patients.
#Note that we provide a file with the data compiled (allData.csv), so that this task is not dependent
#on completion of the other tasks.

#usage: summary_stats(target file with path)
summary_stats <- function(file){
  #read in csv for analysis
  data<-read.csv(file)
  #sum all the rows to find out if the patient is infected, 
  #if all_marker is greater than 0, then this patient is infected
  data$all_marker = rowSums(data[,3:12])
  
  #make a new column to show if the patient is infected
  #non infected screens are 0 and infected patients are 1, for easy calculation
  data$infection_status = NA
  
  for (i in 1:nrow(data)) {
    if (data$all_marker[i] == 0)
      data$infection_status[i] = 0
    else 
      data$infection_status[i] = 1
  }
  
  #filter out rows with age that are unlikely to be real for plotting
  data$age[data$age>110] <- NA
  
  #make a data frame with patient only to plot
  patients = subset(data, data$infection_status == 1)
  
  #count all the screens for both countries
  total_screens = as.numeric(nrow(data))
  #use infection_status to count all the patients
  total_infected = as.numeric(nrow(patients))
  #calculate infection rate
  infected_percentage = (total_infected/total_screens)
  #calculate male patients
  male_patient_count= sum(patients$infection_status[patients$gender=="male"])
  #calculate female patients
  female_patient_count= sum(patients$infection_status[patients$gender=="female"])
  
  
  library(ggplot2)
  age_hist <- ggplot(patients,aes(x = age)) +
    geom_histogram()+
    theme_bw() +
    xlab("Age") +
    ylab("Count")+ggtitle("Age distribution")
  
  print(c("Total screen count is",total_screens))
  print(c("Percent of patients screened that were infected is",infected_percentage))
  print(c("Count of male patient is",male_patient_count))
  print(c("Count of female patient is",female_patient_count))
  
  return(age_hist)
}

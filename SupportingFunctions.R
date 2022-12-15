
# FUNCTION 1: names function "txt2csv" that converts .txt files to .csv 

# "wd" argument provides a working directory for files to be converted in.
# Leaving "wd" blank will use the current working directory.

# "deltxt" argument, if set to TRUE, will delete .txt files after conversion.
# If left blank, or set to FALSE, will not delete files.

txt2csv <- function(wd = NULL, deltxt = NULL) {
# Sets given working directory
   if (!is.null(wd)) {
  setwd(wd)
   }
# Finds all .txt files in directory, writes list of filenames
  list_txt <- list.files(pattern = ".+\\.txt")
# Steps through list of file names, writing an object "file_name" that replaces
#   the .txt extension with .csv
  for (i in 1:length(list_txt)) {
    file_name <- gsub(pattern = "\\.txt", replacement = ".csv", x = list_txt[i])
# Tables from .txt files read to overwriting "data" object, also includes col names
    data <- read.table(file = list_txt[i], header = TRUE)
# Writes csv from "data" object, using "file_name" constructed above and
#   excludes row/col names
    write.csv(x = data, file = file_name, row.names = FALSE)
# If deltxt = TRUE, deletes .txt files from directory
      if (isTRUE(deltxt)) {
        unlink(list_txt[i])
      }
    else{}
  }
}

# FUNCTION 2: compiles all .csv in a directory into one file, also adds new columns
#   "country" and "dayofYear" based on the working directory and filenames, respectively.

# Because no data inside the .csvs have info denoting them as from a certain country,
#   country identification is dependent on file location within named directories.
#   Thus, the following function should be used in both directories of each
#   country's data. Afterwards, rbind can be used to combine the two resulting
#   .csv files:
#       allData.csv <- rbind([filepath]/Country_Y.csv, [filepath]/Country_X.csv)

# argument countryname should describe what country the data originates from
#   and is best used with no spaces.

# argument omit.NA will remove rows with NA values from combined .csv result
#   when omit.NA = TRUE, and will not when FALSE. if left NULL, NAs will be
#   included in data with a warning message

compile.csv <- function(countryname, omit.NA = NULL, wd = NULL) {
# Sets given working directory
  if (!is.null(wd)) {
    setwd(wd)
  }
# Finds all .csv files in directory, writes list of filenames
  list_csv <- list.files(pattern = ".+\\.csv")
# initialize objects
  tempdata <- data.frame(matrix(ncol = 0, nrow = 0))
  doYlist <- data.frame(matrix(ncol = 0, nrow = 0))
# steps through all csv files  
  for (i in 1:length(list_csv)) {
# regmatches extracts regex-matched substrings, pulling the doY out of filenames here
    dayofYear <- regmatches(list_csv[i], regexpr("[0-9]+", list_csv[i]))
# A list of dayofYear, extracted from the csv filename, is written with a length
#   of nrow(.csv file)
    doYlist_i <- as.data.frame(rep(dayofYear, nrow(read.csv(list_csv[i]))))
# dayofYear list is "appended" as for loop iterates
    doYlist <- rbind(doYlist, doYlist_i)
# original data is "appended" or combined as for loop iterates
    tempdata <- rbind(tempdata, read.csv(list_csv[i]))
  }
# "countryname" argument is used to make single column dataframe, repeating the 
#   given countryname for length nrow(tempdata), or all combined dir data
  countrylist <- data.frame(rep(countryname, nrow(tempdata)))
# dataframes of new doY and country columns are combined into one
  newcols <- cbind(doYlist, countrylist)
# rename column headings
  colnames(newcols) <- c("dayofYear", "country")
# combine new columns to combined data
  alldata <- cbind(tempdata, newcols)
  
if (isTRUE(omit.NA)) {
  omitdata <- na.omit(alldata)
# write csv of all data!
  write.csv(omitdata, file = paste0(countryname,".csv"), row.names = FALSE)
}
else if (isFALSE(omit.NA)) {
# write csv of all data!
  write.csv(alldata, file = paste0(countryname,".csv"), row.names = FALSE)
}
else if (is.null(omit.NA)) {
# write csv of all data!
  write.csv(alldata, file = paste0(countryname,".csv"), row.names = FALSE)
  print("WARNING! Output file contains NAs.")
}
}

# FUNCTION 3
overview <- function(filename) {
  all_data <- read.csv(filename)
  total_screens <- nrow(all_data)
  N_females_screened <- nrow(all_data[all_data$gender == "female",])
  N_males_screened <- nrow(all_data[all_data$gender == "male",])
  
  infected_data <- data.frame(matrix(ncol = 0, nrow = 0))
  healthy_data <- data.frame(matrix(ncol = 0, nrow = 0))
  total_infected <- 0
  N_marker01 <- 0
  N_marker02 <- 0
  N_marker03 <- 0
  N_marker04 <- 0
  N_marker05 <- 0
  N_marker06 <- 0
  N_marker07 <- 0
  N_marker08 <- 0
  N_marker09 <- 0
  N_marker10 <- 0
  
  for (i in 1:nrow(all_data)) {
    
    if (all_data$marker01[i] > 0 || all_data$marker02[i] > 0 || all_data$marker03[i] > 0 || all_data$marker04[i] > 0 || all_data$marker05[i] > 0 || all_data$marker06[i] > 0 || all_data$marker07[i] > 0 || all_data$marker08[i] > 0|| all_data$marker09[i] > 0|| all_data$marker10[i] > 0) {
      infected_data <- rbind(infected_data, all_data[i,])
    }
    else {
      healthy_data <- rbind(healthy_data, all_data[i,])
    }
    
    if (all_data$marker01[i] > 0){
      N_marker01 <- N_marker01 + 1
    }
    if (all_data$marker02[i] > 0){
      N_marker02 <- N_marker02 + 1
    }
    if (all_data$marker03[i] > 0){
      N_marker03 <- N_marker03 + 1
    }
    if (all_data$marker04[i] > 0){
      N_marker04 <- N_marker04 + 1
    }
    if (all_data$marker05[i] > 0){
      N_marker05 <- N_marker05 + 1
    }
    if (all_data$marker06[i] > 0){
      N_marker06 <- N_marker06 + 1
    }
    if (all_data$marker07[i] > 0){
      N_marker07 <- N_marker07 + 1
    }
    if (all_data$marker08[i] > 0){
      N_marker08 <- N_marker08 + 1
    }
    if (all_data$marker09[i] > 0){
      N_marker09 <- N_marker09 + 1
    }
    if (all_data$marker10[i] > 0){
      N_marker10 <- N_marker10 + 1
    }
    
  }
  
  total_infected <- nrow(infected_data)
  percent_infected <- total_infected / total_screens * 100
  N_females_infected <- nrow(infected_data[infected_data$gender == "female",])
  N_males_infected <- nrow(infected_data[infected_data$gender == "male",])
  percent_females_infected <- N_females_infected / N_females_screened * 100
  percent_males_infected <- N_males_infected / N_males_screened * 100
  
  print(paste0("Total infected: ", total_infected))
  print(paste0("Total screened:", total_screens))
  print(paste0("Total infected: ", total_infected))
  
  
  # Age distribution histogram
  # extreme outliers in age, such as some individuals living to +400 y.o.a, exist
  #   and make plotting difficult. Thus, ages up to 123 are plotted, though all data
  #   will be written to output files.
  
  healthy_data_trim <- healthy_data[healthy_data$age < 123,]
  infected_data_trim <- infected_data[infected_data$age < 123,]
  
  
  
  library(ggplot2)
 ggplot() +
    geom_histogram(data=infected_data_trim, binwidth = 1, aes(x=age), alpha = 0.6, fill = "red") +
    geom_histogram(data=healthy_data_trim, binwidth = 1, aes(x=age), alpha = 0.6, fill = "green") +
    scale_color_manual(name = "Status", breaks = c("infected", "healthy"), values = c("infected" = "red", "healthy" = "green")) +
    ggtitle("Infected (red) and healthy (green) patient age distribution")
}





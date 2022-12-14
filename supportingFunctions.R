# supportingFunctions.R : a script containing serveral functions to manage the data for RProject
setwd("~/Desktop/Biocomputing/Rnovice/TestRProject/")
# Create a function to convert all Tab and Space delimited .txt files into comma delimited .csv files
convert_files <- function(directory) {
  file_list <- list.files(directory, pattern = .txt)
  for (file in file_list){
    data <- read.table(file, sep = c(" ", "\t"), header = TRUE)
    write.csv(data, file = paste0(file, ".csv"))
  }
}
#go into each directory and convert the files to .csv files
setwd("~/Desktop/Biocomputing/Rnovice/TestRProject/countryX")
convert_files("~/Desktop/Biocomputing/Rnovice/TestRProject/countryX")
setwd("..")
setwd("~/Desktop/Biocomputing/Rnovice/TestRProject/countryY")
convert_files("~/Desktop/Biocomputing/Rnovice/TestRProject/countryY")
setwd("..")

#a function to combine all .csv files in a dir into a single .csv file
compiled_data <- function(directory, naOption){
  #get all .csv files in the dir
  files <- list.files(directory, pattern = .csv)
  #create empty data frame to put all data into
  compiledfiles <- data.frame()
  #create for loop to go through each file in dir
  for (file in files){
    data <- read.csv(file)
    # create if else statement to accomidate user's NA choice
    if (naOption == "remove") {
      data <- data[complete.cases(data),]
    } else if(naOption == "warn"){
      if (any(is.na(data))) {
        print("NA values present in data")
    }
    }
    else if(naOption == "include"){
      data <- data
    }
    compiledfiles <- rbind(compiledfiles, data)
  }
  write.csv(compiledfiles, file = "compiledfiles.csv")
}

compiled_data("countyX")
compiled_data("countyY")
# Biocomputing R Project
# hjeon
# supportingFunctions.R

# set workspace
# Note: I have unzipped the Rproject2022.zip and I just set the working directory to the unzipped
setwd("/Users/hyesooclarejeon/Desktop/Biocomputing_RProject/Rproject2022")

# import libraries
library(MASS)

################## Task 1: convert all files ending in txt into comma-separated value files ################

# define the function parseFiles
parseFiles <- function() {
  # add all full file names that are txt formatted into list
  txtFiles <- list.files(path = "countryY", pattern = "*.txt", full.names = TRUE)
  
  for (fileName in txtFiles) {
    # use read.delim because it is suited specifically for
    # tab-delimited/space-delimited data
    screenData <- read.delim(fileName, header = TRUE, sep = " ")
    
    # extract filename without the extension using regex
    fileName <- sub('\\..[^\\.]*$', '', fileName)
    
    # .csv extension concat
    fileName <- paste0(fileName, ".csv")
    
    # write to new file with csv extension comma-separated
    write.table(screenData, fileName, sep = ",", row.names=FALSE)
  }
}

################## Task 2: convert all files ending in txt into comma-separated value files ################

# define helper function for reading and wrangling csv data from both countries 
# to avoid redundancy in code
compileHelper <- function(countryFiles, combineChoice, combinedFileName, countryName) {
  for (fileName in countryFiles) {
    # use read.csv to read each fileName
    screenData <- read.csv(fileName, header = TRUE, na.strings = c("", " ", NA, "NA"))
    
    if (combineChoice == 1) {
      screenData <- na.exclude(screenData)
    }
    # combine two new columns - country and dayofYear - to existing 12 columns
    # check if NA for all 10 markers - if so, continue to next iteration
    for (i in 1:nrow(screenData)) {
      # save data for markers1 - 10 for this patient
      patient <- screenData[i, ]

      # if 1's are found in patient -> this is NOT an NA instance, so we would append to new csv file
      # extract dayOfYear using regex
      dayOfYear <- regmatches(fileName, regexpr("[0-9]+", fileName))
      
      # append 2 new columns to patient
      patient <- append(patient, list(countryName, dayOfYear))
      
      # append this patient + 2 new columns in new csv
      write.table(patient, file = combinedFileName, sep = ",", append = TRUE, quote = FALSE, col.names = FALSE, row.names = FALSE)
    }
  }
}

# define the function combineCsv
# the parameter combineChoice is either integers 1, 2, or 3 depending on the choice of compiling data:
# 1: remove rows with NA’s in any columns
# 2: include NAs in the compiled data but be warned of their presence
# 3: include NAs in the compiled data without a warning
combineCsv <- function(combineChoice) {
  
  # warn user of of NA prsence if choice was 2
  if (combineChoice == 2) {
    cat("\nWARNING: DATA WITH DATA MODE NA WILL BE ADDED TO COMPILED DATA allData_myversion.csv ...")
  }
  
  # collect all csv files in countryX directory
  countryXFiles <- list.files(path = "countryX", pattern = "*.csv", full.names = TRUE)
  
  # collect all csv files in countryY directory
  countryYFiles <- list.files(path = "countryY", pattern = "*.csv", full.names = TRUE)
  
  # define new comprehensive csv filename
  combinedFileName <- "allData_myversion.csv"
  
  # define header/column names
  header <- data.frame("gender", "age", "marker01",	"marker02",	
  "marker03",	"marker04",	"marker05",	"marker06",	"marker07",	
  "marker08",	"marker09",	"marker10",	"country",	"dayofYear")
  
  # add header to combinedFileName
  write.table(header, file = combinedFileName, sep = ",", append = TRUE, quote = FALSE, col.names = FALSE, row.names = FALSE)
  
  # compile from countryX files
  compileHelper(countryXFiles, combineChoice, combinedFileName, "X")
  
  # compile from countryY files
  compileHelper(countryYFiles, combineChoice, combinedFileName, "Y")
  
  # again warn user of of NA prsence if choice was 2
  if (combineChoice == 2) {
    cat("\nCOMPILATION COMPLETED")
    cat("\nWARNING: DATA WITH DATA MODE NA HAS BEEN ADDED TO COMPILED DATA allData_myversion.csv")
  }
}

################## Task 3: use compiled dataset to generate summary ################

# define function named summarizeData
summarizeData <- function() {
  # print header
  cat("Summary for allData.csv ...\n")
  
  # read given allData.csv
  compiledData <- read.csv("allData.csv", header = TRUE, sep= ",")
  
  # save number of rows in compiledData
  numScreens <- nrow(compiledData)
  
  # print numScreens
  cat("Number of screen tests run: ", numScreens, "\n")
  
  # initialize counter for tallying number of infected
  numInfected = 0
  
  # initialize counter for tallying number of infected who are male
  numInfectedMale = 0
  
  # initialize counter for tallying number of infected who are female
  numInfectedFemale = 0
  
  # initialize list for adding age of infected
  age_infected <- c()
  
  for (i in 1:numScreens) {
    # save data for markers1 - 10 for this patient
    patient <- compiledData[i, seq(3,12)]
    # if at least one marker is marked 1, then increment counter
    if (1 %in% patient) {
      numInfected <- numInfected + 1
      gender <- compiledData[i, 1]
      age <- compiledData[i, 2]
      if (gender == "male") {
        # if male, increment counter for male infected
        numInfectedMale <- numInfectedMale + 1
      } else {
        # if male, increment counter for female infected
        numInfectedFemale <- numInfectedFemale + 1
      }
      age_infected <- append(age_infected, age)
    }
  }
  
  # print number of infected
  cat("Number of infected: ", numInfected, "\n")
  # print percentage of infected
  cat("Percentage of infected: ", round(numInfected * 100 / numScreens, 2), " %\n")
  
  # print number of infected male
  cat("Number of infected (male): ", numInfectedMale, "\n")
  # print percentage of infected male
  cat("Percentage of infected (male): ", round(numInfectedMale * 100 / numScreens, 2), " %\n")
  
  # print number of infected female
  cat("Number of infected (female): ", numInfectedFemale, "\n")
  # print percentage of infected female
  cat("Percentage of infected (female): ", round(numInfectedFemale * 100 / numScreens, 2), " %\n")
  
  # print ratio of male to female number infected
  cat("Male vs. Female fraction: ")
  print(fractions(numInfectedMale / numInfectedFemale))
  
  # print age distribution
  cat("Age distribution ...\n")
  age_dist <- as.data.frame(table(age_infected))
  print(age_dist, row.names = FALSE)
}

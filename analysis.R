# Biocomputing R Project
# hjeon
# analysis.R

# load functions from supportingFunctions.R
source("supportingFunctions.R")

# choose a number (has to be an integer not a string) as 1 of 3 choices of combining data
# 1: remove rows with NA’s in any columns
# 2: include NAs in the compiled data but be warned of their presence
# 3: include NAs in the compiled data without a warning
# I just chose 1 for now so that it is easier to generate graphical evidence later on, but by changing the following assignment to 2 or 3,
# compilation choice can be customized
combineChoice <- 1

# process data compilation using imported function from supportingFunctions.R
# this will take some time as it has to process through all relevant files in countryX and countryY directories
# at the end, in this working directory, we will find a combined allData_myversion.csv
# I just named it allData_myversion.csv to differentiate from the already provided allData.csv
combineCsv(combineChoice)

# save compiled data
compiledData <- read.csv("allData_myversion.csv", header = TRUE)

####################################################################################################
########### Question 1: In which country (X or Y) did the disease outbreak likely begin? ###########
####################################################################################################

# initialize list for adding screen date when infection diagnosed for each country
screen_date_infectedX <- c()
screen_date_infectedY <- c()

# save number of screen tests performed in compiled data
numScreens <- nrow(compiledData)

# go through all screens to collect date for infected patients
for (i in 1:numScreens) {
  # save data for markers1 - 10 for this patient
  patient <- compiledData[i, seq(3,12)]
  # if at least one marker is marked 1, then it means patient infected
  if (1 %in% patient) {
    country <- compiledData[i, 13]
    date <- compiledData[i, 14]
    if (country == "X") {
      # if from countryX, append date to vector of countryX
      screen_date_infectedX <- append(screen_date_infectedX, date)
    } else {
      # for countryY
      screen_date_infectedY <- append(screen_date_infectedY, date)
    }
  }
}





# Biocomputing R project -- analysis
# Isabella Gimon, Carol de Souza Moreira, Gretchen Andreasen
# Dec 14 2022

# Begin by setting working directory ----
setwd("~/Documents")

# Source the code containing the necessary countries
source("supportingFunctions.R")
# Function 1: Converting_txt_to_csv
# Function 2: compiler
# Function 3: summarizer

# Convert any .txt files in the country folders to .csv files
# DO THIS ONCE THE FIRST FUNCTION IS FIXED

# Compile the country data into one file, "compiledData.csv", with udf compiler
compiler(path_country1 = "countryX", 
	name_country1 = "X", 
	path_country2 = "countryY", 
	name_country2 = "Y")

# Load the compiled data into R
compiledData <- read.csv(file = "compiledData.csv", header = TRUE)

# Summarize it with udf summarizer
summarizer(compiledData)

# Question 1 ----
# In which country (X or Y) did the disease outbreak likely begin?

# Let's see if we can graph the presence of infected individuals with days 
# on the x-axis.
# To start, I think it'd be easiest to have a simple "infected: Y/N" column.
# Let's copy the raw data to a new object
new_data <- compiledData
# Make a column with how many markers a patient has
new_data$marker_sum <- rowSums(new_data[3:12])
# Now let's run a for loop to determine if that patient is infected or not
for(i in 1:nrow(new_data)){ # each row is a patient/screening
	# If that column is > 0, the patient is infected
    if(new_data$marker_sum == 0){
      new_data$infected <- 0
    } else{
    	new_data$infected <- 1
    }
}

# Nevermind let's use marker sum as a proxy for infections

# Now let's get to graphing.
library(ggplot2)
ggplot(new_data, aes(x = dayofYear, y = marker_sum, group = country, 
	color = country)) +
	geom_point()






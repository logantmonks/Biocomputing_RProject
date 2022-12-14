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
# To start, I think it'd be easiest to use the # of markers as a proxy for
# infections (because, at the very least, 0 markers = 0 infected).
# Let's copy the raw data to a new object
new_data <- compiledData
# Make a column with how many markers a patient has
new_data$marker_sum <- rowSums(new_data[3:12])
# Now let's make a column with the cumulative sum of markers by country.
new_data$cum_marker <- ave(new_data$marker_sum, new_data$country, FUN = cumsum)

# Now let's get to graphing.
library(ggplot2)
ggplot(new_data, aes(x = dayofYear, y = cum_marker, group = country, 
	color = country)) +
	geom_line() +
	xlab("Day of year") +
	ylab("Cumulative marker count") +
	theme_minimal()

# To answer question 1: We believe that the disease first broke out in 
# Country X, given how much sooner Country X has markers (infected) present 
# in their population before Country Y, who doesn't have any markers until
# about day 140, a whole 20 days after Country X.


# Question 2 ----
# If Country Y develops a vaccine for the disease, is it likely to work for
# the citixens of Country X?

# Let's sum the markers 1-10 based on country and create a new dataframe.
for (column in new_data[3:12]){ # We have to use the full column from the data,
	# otherwise aggregate will be unhappy because it will only read a variable 
	# and not a full column.
	if(!exists("new_df")){ # This is just to reiterate onto a dataframe
		new_df <- aggregate(column ~ country, new_data, sum) # Summing column by country
	} else if (exists("new_df")){ # Appending onto previously created df
		temp_df <- aggregate(column ~ country, new_data, sum)
		new_df <- rbind(new_df, temp_df)
		rm(temp_df)
	}
}

# Great! That worked. But we're missing marker numbers for the data. 
# Let's add that posthumously, because we're working with columns in the 
# for loop.
for (row in nrow(new_df)){
	for (number in 1:10){
		new_df$marker <- number
	}
}

new_df



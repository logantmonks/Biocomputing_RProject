# Biocomputing R project -- analysis
# Isabella Gimon, Carol de Souza Moreira, Gretchen Andreasen
# Dec 14 2022

# Begin by setting working directory ----
setwd("~/Documents/Courses/Biocomputing_BIOS_60318/Tutorials/Biocomputing_RProject")

# Source the code containing the necessary countries
source("supportingFunctions.R")
# Function 1: Converting_txt_to_csv
# Function 2: compiler
# Function 3: summarizer

# Convert any .txt files in the country folders to .csv files
Converting_txt_to_csv() # Works!

# Compile the country data into one file, "compiledData.csv", with udf compiler
compiler(path_country1 = "Rproject2022/countryX", 
	name_country1 = "X", 
	path_country2 = "Rproject2022/fixed_countryY", 
	name_country2 = "Y")

# Summarize the compiled data using udf summarizer
summarizer(path_to_data = "compiledData.csv")

# Load the compiled data into R for analysis
compiledData <- read.csv(file = "compiledData.csv", header = TRUE)

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
# the citizens of Country X?

# Let's sum the markers 1-10 based on country and create a new dataframe.
for (column in new_data[3:12]){ 
	# We have to use the full column from the data, otherwise aggregate will be 
	# unhappy because it will only read a variable and not a full column.
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
count <- -1 # Create a count to add to for "double row numbers".
number <- 1 # Create number for first marker, starting at 1.
# Iterate over the rows to add a number and signify the marker.
for (row in 1:nrow(new_df)){
	count <- count + 1
	if (count >= 2){ # This will take care of the second row problem
		count <- 0
		number <- number + 1
	}
	new_df$marker[row] <- number
}

# Yay! Now we get to plot the data.
ggplot(new_df, aes(x = marker, y = column, color = country, fill = country)) +
	geom_bar(position = "dodge", stat = "identity") +
	theme_minimal() +
	ylab("Cumulative markers in population") +
	xlab("Marker") +
	scale_x_continuous(breaks = seq(0, 10, 1))

# To answer question 2: I think the vaccine will not work for Country X. Only 
# because the markers found in Country Y are less prevalent than those found in 
# country X. So a different vaccine would have to produced in order for citizens 
# to acquire the proper immunity. It’s comparable to what happens to the Dengue 
# vaccine. There’s 4 serotypes and it’s a major challenge to make it work for 
# all 4. With different markers being prevalent in different countries, they’ll 
# need to develop a vaccine that targets both groups or one vaccine for each 
# country.


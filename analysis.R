# Introduction to Biocomputing R Project
# Group Members: Brandon Barnacle, Austin Chang, Patrick Bahk

# For this file, and the way these functions are meant to be used, is for your
# working directory to be in the Rproject2022 folder. For other cases, this means
# that you should be in a director that contains sub directories that have the country's
# name as the directory name following the format of "countryCountryName"(e.x. countryX)
# and inside of those directors are the .txt or .csv files containing the screening data

# need to import ggplot and import our custom functions
library(ggplot2)
source("supportingFunctions.R")

# the first step is to create the output file
create_output_file(".")

# next we want to make sure all the data is in .csv format, in our case we know
# we need to do it for countryX and countryY, but can apply to any directory
txt_to_csv("countryX")
txt_to_csv("countryY")

# now we want to combine all the csv data into a single file for each country
csv_join("countryX")
csv_join("countryY")

# now we can combine these into one big csv file, we give the current directory
# because that is where the combined csv files will be, and that is where the
# allData file will be
combine_big_csv(".")

# now we can summarize the data we found
csv_summarize("allData.csv")

# to get a better understanding of the infection and to have more data to answer
# the two questions, we want to get the count of infected in each country and
# which markers are most present in both, and a timeline of the infections
country_infected_count(".")
country_markers("countryX")
country_markers("countryY")
infection_timeline("countryX")
infection_timeline("countryY")

# Answer to question 1
# We would say that the outbreak likely began in X because it has a greater total
# number of infections than country Y by more than double. We believe this shows
# that the outbreak had more time to spread in countryX than in countryY. Also,
# we have a function that shows a timeline of the infections, and we can see
# that for the first several screenings that countryY has no infections while
# countryX does. All of this information infers that the outbreak started
# in countryX


# Answer to question 2
# If countryY were to develop a vaccine, we think that it is unlikely that it will be
# effective in countryX. Our graphs for the marker count back up our data. Looking at
# which markers were most present in countryX, we can see that it is almost completely 
# made up of makers 1-5. However, when we look at the marker count for countryY, we
# see that the majority of the infections are with markers 6-10. In this way, if a 
# vaccine were developed in countryY, it would probably help build immunity for the 
# markers that are most present in countryY, which would mostly ignore the markers
# that are most present in countryX. For this reason, we believe a vaccine from countryY
# would not be effective in countryX. 

# Reusability
# The code written for this project can be used even if more screening data is added.
# If it stays with just countryX and countryY, as long as the screening data is put into
# the proper directories in .csv or .txt files, then rerunning the script will work and
# automatically update the data. If more countries are added, the functions will still
# work as long as you give the country a directory to hold its screening data. What you
# then need to do for the analysis is add another function call for the added country
# where you see the function calls for countryX and countryY. All the functions work
# no matter what the country is.
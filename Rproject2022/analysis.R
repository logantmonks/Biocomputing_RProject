# analysis.R
# Alex Brandt, Olivia Maracina, Hugh Smith
# 12/14/2022
source("~/biocomputing/Biocomputing_RProject/Rproject2022/supportingFunctions.R")

countries <- c('X', 'Y')
# pass a list of the countries desired for analysis to the functions
txt_to_csv(countries)
fulldata = compile_files(countries)
new_fulldata = summary(countries, fulldata)

# get only infected data
rate_data = subset(new_fulldata, infected==TRUE)

#display the infection rates of the two countries
ggplot(rate_data, aes(x=dayofYear, color = country)) +
  geom_density() +
  ggtitle("Infection Rates") +
  theme_classic()
# QUESTION 1
# As can be seen by the graph produced, it appears that the infection began
# in Country X- there are positive cases starting at day 120, where as cases
# don't start to appear in Country Y until around day 140. This is pretty 
# straightforward- cases should be higher/start from the country of origin.

# creating each variable to be added to dataframe for markers
markers <- c(rep("01" , 2), rep("02" , 2), rep("03" , 2), rep("04" , 2), rep("05" , 2), rep("06" , 2), rep("07" , 2), rep("08" , 2), rep("09" , 2), rep("10" , 2))
country <- rep(c("X" , "Y"), 10)
counts <- list()

# get count of markers by country
for(j in 3:12) {
  x_val = 0
  y_val = 0
  for(i in 1:nrow(fulldata)) {    
    if (fulldata[i,13]=='X') {
      x_val = x_val + fulldata[i,j]
    }
    else if (fulldata[i,13]=='Y') {
      y_val = y_val + fulldata[i,j]
    }
  }
  counts = append(counts, x_val)
  counts = append(counts, y_val)
}

# create table of data for bar graph
bar_data <- data.frame(markers=markers,country=country,counts=0)
for(i in 1:nrow(bar_data)) {
  bar_data[i,3] = counts[i]
}

# plot marker data
ggplot(bar_data, aes(fill=country, y=counts, x=markers)) + 
  geom_bar(position="dodge", stat="identity") +
  ggtitle("Marker Counts Comparison") +
  theme_classic()
# QUESTION 2
# It appears unlikely that a vaccine developed in Country Y would work in
# Country X because of the variation of the markers in each country.
# Country X is almost entirely based on markers 1-5, while Y is 6-10.
# The disease causing bacteria has likely evolved




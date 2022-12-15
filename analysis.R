# Biocomputing R Project
# hjeon
# analysis.R

# import libraries
library(ggplot2)

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

# go through all screens
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

# tally up dates at which infected
screen_date_infected_distX <- as.data.frame(table(screen_date_infectedX))
screen_date_infected_distY <- as.data.frame(table(screen_date_infectedY))
print(screen_date_infected_distX[,1, drop=FALSE], row.names = FALSE)

# use ggplot functionalities to produce smooth curves for both dataframes in single plot
ggplot() +
  geom_line(data = screen_date_infected_distX, mapping = aes(x = screen_date_infectedX, y = Freq, group = 1), color = "red") +
  geom_line(data = screen_date_infected_distY, mapping = aes(x = screen_date_infectedY, y = Freq, group = 1), color = "blue") +
  scale_x_discrete(breaks = function(x){x[c(TRUE, FALSE, FALSE, FALSE, FALSE)]}) +
  xlab('Screen Test Dates') +
  ylab('Number Infected') +
  ggtitle('Question 1: Where did the outbreak begin?') +
  annotate('text', x=20, y=400, label = 'countryX', col = "red")+
  annotate('text', x=40, y=100, label = 'countryY', col = "blue")

# answer for Question 1
"My answer for Question 1 is that it must have begun in Country X. 
As seen in the ggplot graph of two lines, the first screen tests that were positive
were recorded in countryX. In addition, the numbers of infected at common dates are much higher
for country X than for country Y generally, indicating it may have had the outbreak going on for 
a longer time.
"

####################################################################################################################################
########### Question 2: If Country Y develops a vaccine for the disease, is it likely to work for citizens of Country X? ###########
####################################################################################################################################

# add in any of the 10 markers that were present in patients of each country
markers_X <- c()
markers_Y <- c()

# go through all columns that are for markers
xMarkerData <- compiledData[compiledData$country == "X", seq(3,12)]
yMarkerData <-compiledData[compiledData$country == "Y", seq(3,12)]
for (i in 1:10) {
  # save this marker column for x and y
  marker_i_x <- xMarkerData[, i]
  marker_i_y <- yMarkerData[, i]
  # if x's marker column has 1
  if (is.element(1, marker_i_x)) {
    markers_X <- append(markers_X, i)
  } else {
    markers_X <- append(markers_X, 0)
  }
  # if y's marker column has 1
  if (is.element(1, marker_i_y)) {
    markers_Y <- append(markers_Y, i)
  } else {
    markers_Y <- append(markers_Y, 0)
  }
}
# create structure for marker 1 to 10
markers <- paste0("marker ", seq(1,10))

# create structure for [countryX, marker #, and present (marker # or 0)] factor
country <- "countryX"
markers_present <- markers_X
df_X <- cbind(country, markers, markers_present)

# create structure for [countryY, marker #, and present (marker # or 0)] factor
country <- "countryY"
markers_present <- markers_Y
df_Y <- cbind(country, markers, markers_present)

# combine df_X and df_Y into df
df <- data.frame(rbind(df_X, df_Y))

# # construct very simple tilemap to show presence of any of the 10 markers for country X and Y
ggplot(df, aes(country, markers, fill = markers_present)) + 
  geom_tile(colour = "grey50") +
  ggtitle('Question 2: Would vaccine in Y work for X?')

"My answer for Question 2 is yes vaccine developed in country Y would work for X,
because as we see in the simple tilemap, we see all markers present in X also in Y
infected patients. If the colors are different side by side for a marker, then
that means it is not a common marker between X and Y. However, we do see same colors
side by side for all markers in this graph. Given that the vaccine is developed based
on the country Y's markers 1 through 10 presence factor, yes the vaccine would also
work for those in X who have the same set of markers.
"


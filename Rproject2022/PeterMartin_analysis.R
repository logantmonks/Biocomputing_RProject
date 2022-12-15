# PeterMartin_analysis.R
# Analysis code that specifies arguments for the functions described in the script PeterMartin_supportingFunctions.R

# Set the working directory and load all necessary packages, as well as the contents of PeterMartin_supportingFunctions.R
setwd('~/Desktop/Rfiles/Biocomputing/Biocomputing_RProject/Rproject2022')
library(ggplot2)
library(wesanderson)
library(multcompView)
library(plotrix)
library(ggsignif)
source("PeterMartin_supportingFunctions.R")

# Define paths to data (makes writing the arguments for txt.to.csv() and compile.csv() easier)
countryX_path<-"~/Desktop/Rfiles/Biocomputing/Biocomputing_RProject/Rproject2022/countryX"
countryY_path<-"~/Desktop/Rfiles/Biocomputing/Biocomputing_RProject/Rproject2022/countryY"

# Converts the .txt files in the countryY folder to .csv format
txt.to.csv(file=list.files(path = countryY_path, pattern = "txt"),path_to_file = countryY_path)

# Compiles all .csv files in the two directories (countryX and countryY) into a single .csv file (with some additional
# data)
length(list.files(countryX_path, pattern = "csv"))
length(list.files(countryY_path, pattern = "csv"))
# Informs argument of nFiles

compile.csv(path_to_file = c(countryX_path,countryY_path),na.rm = "warn",nFiles = c(56,56))

# Conducts an analysis (various summary statistics and graphs) of the compiled data, with comparisons between countries
screenStats(file_name = "compileData.csv")

########## QUESTION 1 ###########
# In which country (X or Y) did the disease outbreak likely begin?
# ANSWER:
# Over the first 23 days of the outbreak (i.e., nFiles = c(23,23)), 55.18% of those screened in country X 
# tested positive, whereas only 2.07% of those screened in country Y tested positive. Both countries averaged
# 1 microsatellite detected per positive test. Over the entire time frame (56 days), 78.22% of people screened
# in country X teseted positive, while 34.65% of people screened in country Y tested positive. Country X, on
# average, had two markers detected per positive test, whereas country Y remained at one marker per positive
# test. These findings show that the disease likely began in country X. At the beginning of the period in which
# data was recorded, country X already had a high positivity rate, whereas country Y had a negligible rate.
# We see two countries whose positivity rates are increasing, but which are at different points along
# this curve. Country X, as the origin point, has a higher viral density than country Y. This hypothesis is
# supported by the microsatellite #/positive screen data: over the course of the data collection period, we
# observe stronger immune responses in country X (higher number of markers detected). This indicates immune
# systems which are less naive than country Y and are more sensitive to the disease, activating more secondary 
# immune responses upon infection.

########## QUESTION 2 ###########
# If Country Y develops a vaccine for the disease, is it likely to work for citizens of Country X?
# ANSWER:
# If Country Y develops a vaccine for the disease, it is not likely to work for citizens of Country X. This
# is because country Y has a different strain of the disease. If we examine the frequencies of given markers
# appearing in the screenings, we can see that country X is dominated by markers 1-5, whereas country Y has
# undergone a shift. As the disease has moved from country X to country Y and has traveled through
# evolutionary time, the genetic code of its main protein has mutated: markers 1-3 are
# detected much less frequently, as well as markers 4-5, and markers 6-10 are expressed at much higher 
# frequencies. Thus, the protein has fundamentally changed as the bacteria has evolved and encountered a new
# environment, leading to a new strain of the disease. A vaccine developed in country Y against its own major 
# strain is likely to be less effective, possibly not at all effective, in country X, since country X has
# a different major strain.



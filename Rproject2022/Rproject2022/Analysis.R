# R Project -- Completed Final Project analysis.R

# Laurel Lown
# Matthew Hawkins
# Andrew Lupinski

##All paths are specific to personal working directory during development

setwd("C:/Users/matha/Desktop/RProject") # set working directory to Rproject folder

## First, use source() to import functions from supportingFunctions.R script
source("SupportingFunctions.R")
## Function 1: convert .txt to .csv
### See additional notes on function in supportingFunctions.R file
setwd("C:/Users/matha/Desktop/RProject/countryY")
countryYfileConvert=list.files(pattern=".txt") 
# Create list for file(s) of interest, chooses .txt files ONLY within the Country Y working directory
fileConvert(countryYfileConvert)

## Function 2: Merges all .csv files of interest into a single .csv
### See additional notes on function in supportingFunctions.R file
merge("Rproject", na_handling="warn of NA presence")

## Function 3: Summary of Data - uses 'allData.csv'
### See additional notes on function in supportingFunctions.R file
setwd("C:/Users/matha/Desktop/RProject")
data_summary("allData.csv")

## Completes marker analysis comparing number of markers per country (X or Y)
### See additional notes on function in supportingFunctions.R file
markers("allData.csv")

# Question 1: In which country (X or Y) did the disease outbreak likely begin?

### We believe that the outbreak began in country X according to what our line graph outputted
### by function 3. This is based on the fact that number of infected cases was low and stagnant 
### in Country Y for nearly the first 20 days. This is in comparison to Country X, which had 
### gradual increase in number of individuals infected over those same ~20 days.


# Question 2: If Country Y develops a vaccine for the disease, is it likely to work for citizens of Country X?

### We do not believe that a vaccine developed for country Y would be effective in country X. 
### This is due to little to no alignment in terms of marker prevalence between the two countries. 
### Markers 1-5 are highly contained to country X, while markers 6-10 are contained to country Y. 
### Evidence for this was outputted from function 4.



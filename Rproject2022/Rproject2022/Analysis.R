setwd("C:/Users/matha/Desktop/RProject")
source("SupportingFunctions.R")
setwd("C:/Users/matha/Desktop/RProject/countryY")
countryYfileConvert=list.files(pattern=".txt") # create list for file(s) of interest, chooses .txt files ONLY within the Country Y working directory
fileConvert(countryYfileConvert)
merge("Rproject", na_handling="warn of NA presence")
setwd("C:/Users/matha/Desktop/RProject")
data_summary("allData.csv")
markers("allData.csv")

###Question 1
#We believe that the outbreak began in country X, this is based on the fact that number of infected cases was low and stagnant in Country Y for nearly the first 20 days. 
#This is in comparison to Country X, which had gradual increase in number of individuals infected over those same ~20 days.


###Question 2
#We do not believe that a vaccine developed for country Y would be effective in country X. 
#This is due to little to no alignment in terms of marker prevalence between the two countries. 
#Markers 1-5 are highly contained to country X, while markers 6-10 are contained to country Y. 
#Biocomputing R Project Analysis - Sara Bennett & Jonathan Gilman

#source for function definitions
source("supportingFunctions.R")

#Convert TXT to CSV (only need to do for Country Y)
makeCSV("countryY")

#Make one master CSV with all country data
masterData("countryX","countryY")

#Summarize Data (this is slow but it works just wait a few seconds)
sumData("allData.csv")

#In which country did the outbreak begin?
  # According to the data, we believe that the outbreak began in Country X. Looking at the graph, on the first days of screening
  # there are infections present in Country X, but not Country Y. We do not see the first infection in Country Y until screening on day 139.
  # Due to this later onset of infections in Country Y, we believe that the outbreak began in Country X. 

#Graphical Evidence - Total infected X vs Y over time
outbreak("allData.csv")

#Will a country Y vaccine work for country X?
  #We do not believe that a Country Y vaccine will work for Country X. While evaluating the biomarkers primarily
  #present in each country, you can see that Country X is much more significantly affected by biomarkers 1-5 in their
  #infections. On the other hand Country Y is moreso affected by biomarkers 6-10. Due to this prevalence, a Country Y vaccine
  #would likely target biomarkers 6-10. Country X has barely any occurrences of biomarkers 6-10, so this Country Y vaccine would not prevent
  #much infection at all in Country X. In addition, biomarkers 1-5 are much more rampant in spreading infection in Country X, and the 
  #Country Y vaccine focused on 6-10 would not work to prevent those at all, so the spread of infection due to biomarkers 1-5
  #in Country X would still continue, even with the Country Y vaccine. A new Country X vaccine focused on biomarkers 1-5 would be most effective for country X.


#Graphical Evidence - Total Biomarker Counts X vs Y
markerPrevalence("allData.csv")




# analysis.R
# A script to analyse the data and use the functions already made in the other script
source(("~/Desktop/Biocomputing/Rnovice/Biocomputing_RProject/supportingFunctions.R"))
#I moved all files from RProject2022 dir into RProject, so the path may need to be edited

#Name variable for countries analyized
countries <- c("X", "Y")
convert_files(countries)
#plug variables into supporting fuctions
#naOption allows user to choice "remove" to remove NA values, "warn" to warn of them, and "include" to include without warning 
compiledfiles <- compiled_data(counties, naOption)
finaldata <- summarize("allData.csv")

data <- read.csv("allData.csv")
#using the infected vector from supporting functions to only use the infected indivuals out of the data
Newinfecteddata <- data[infected,]

#Q:1 Which country did it start in?
#Make a graph that shows chronology of infections, whichever happened first is where infection likely started
Xdata <- Newinfecteddata[newinfecteddata$country == "X",]
chrono_plotX <- ggplot(Xdata, aes(x = dayofYear)) + geom_density() + theme_classic() + ggtitle("Timing of Infections")
print(chrono_plotX)
Ydata <- newinfecteddata[newinfecteddata$country == "Y",]
chrono_plotY <- ggplot(Ydata, aes(x = dayofYear)) + geom_density() + theme_classic() + ggtitle("Timing of Infections")
print(chrono_plotY)
#The plots show that infections in country X began around day 120 while infections in country Y began just before 140. This suggests that the infections began in X and spread to Y
# However, this is not definitive because if infected individals missed the screening they would not appear in the data

#Q:2 Would a vaccine from country Y be effective in country X
#Need to look at markers from each country to see if there is overlap

#Find prevelence of each marker in country X
XMarkers <- colSums(Xdata[,3:12])
XMarkerSum = sum(XMarkers)
XMarkerPrevelence <- (XMarkers/XMarkerSum)

#Find prevelence of each marker in country Y
YMarkers <- colSums(Ydata[,3:12])
YMarkerSum = sum(YMarkers)
YMarkerPrevelence <- (YMarkers/YMarkerSum)

print("Prevelence of Each Marker in Infected Patients in Country X")
XMarkerPrevelence
print("Prevelence of Each Marker in Infected Patients in Country Y")
YMarkerPrevelence

print("Most Common Markers in X:") 
order(XMarkerPrevelence)
print("Most Common Markers in Y:")
order(YMarkerPrevelence)
# A vaccine developed in country Y would likely not have high efficacy in country X because of the diffences in markers seen in each country
# The top 5 markers in one country are the bottom five markers in the other, because of this, independant vaccines will likely need to be developed

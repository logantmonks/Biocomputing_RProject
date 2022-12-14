#Authors: Catherine Andreadis, Shehani Fernando, Zhuoran Yu

source("/Users/zhuoranyu/Biocomputing_RProject/supportingFunctions.R")

convert_files_in_directory("/Users/zhuoranyu/Biocomputing_RProject/Rproject2022/countryY")

alldata <- compile_all_csv("/Users/zhuoranyu/Biocomputing_RProject/Rproject2022/countryX",
                "/Users/zhuoranyu/Biocomputing_RProject/Rproject2022/countryY",
                "X","Y","remove")

#set direction to put the csv with all data
setwd("/Users/zhuoranyu/Biocomputing_RProject/Rproject2022")
write.csv(alldata,"allData_from_funtion_2.csv",row.names = FALSE)

#Use output data from function 2 for function 3
summary_stats("/Users/zhuoranyu/Biocomputing_RProject/Rproject2022/allData_from_funtion_2.csv")

#1. In which country (X or Y) did the disease outbreak likely begin?

#read in the data again if we don't run the functions first
alldata <- read.csv("/Users/zhuoranyu/Biocomputing_RProject/Rproject2022/allData_from_funtion_2.csv")

#sum all the rows to find out if the patient is infected, 
#if all_marker is greater than 0, then this patient is infected

alldata$all_marker = rowSums(alldata[,3:12])

#make a new column to show if the patient is infected
#non infected screens are 0 and infected patients are 1, for easy calculation
alldata$infection_status = NA

for (i in 1:nrow(alldata)) {
  if (alldata$all_marker[i] == 0)
    alldata$infection_status[i] = 0
  else 
    alldata$infection_status[i] = 1
}

#make a data frame with only patients in it
patients = subset(alldata, alldata$infection_status == 1)

#remove the rows with patients with unrealistic age, so the result could be more accurate
patients$age[patients$age>110] <-NA
patients <- na.omit(patients)

#make a data frame to store the sum of patients for each screen day for both country
outbreak <- data.frame(matrix(ncol = 3, nrow = length(unique(alldata$dayofYear))))
x <- c("screen_day","country_X_patient_count","country_Y_patient_count")
colnames(outbreak) <- x

#input all screen days to the data frame
outbreak$screen_day = unique(alldata$dayofYear)

for (i in outbreak$screen_day){
    x_pat= patients[which(patients$dayofYear==i&patients$country=="X"),]
    outbreak[i-119,2]=nrow(x_pat)
    y_pat= patients[which(patients$dayofYear==i&patients$country=="Y"),]
    outbreak[i-119,3]=nrow(y_pat)}
 
#make cumulative sum for patients in both countries to plot
outbreak$X_cumsum <- cumsum(outbreak$country_X_patient_count)
outbreak$Y_cumsum <- cumsum(outbreak$country_Y_patient_count)

library(ggplot2)

ggplot(outbreak, aes(x=screen_day , y = "")) + 
  geom_line(aes(y = X_cumsum, col = "Country X")) + 
  geom_line(aes(y = Y_cumsum, col = "Country Y"))+
  theme_bw() +
  xlab("Day of Year") +
  ylab("Cumulative patient count")+
  theme(legend.title=element_blank())+ggtitle("Outbreak Comparison between Country X and Y")

##Answer: From the data and graphical evidence, it is suggested that the disease
##outbreak began in Country X. As visualized in the graph, we can see that at 
##Day 140, Country X reported roughly 4000 infected patients, while Country Y was
##still reporting 0 cases. Since Country Y only begins to report positive cases 
##after Country X, we can assume the disease outbreak began in Country X and 
##spread to Country Y.

#2. If Country Y develops a vaccine for the disease, is it likely to work for citizens of Country X?

#Subset the dataframe so only markers and country are left 
X_markers <- subset(alldata,country=="X", select = marker01:marker10)

Y_markers <- subset(alldata, country == "Y", select = marker01:marker10)

#Run colsum for each of the markers in each dataset to get sum of markers presented for each kind of marker

#For Country X
Xmarker_sums <- colSums(X_markers, na.rm=FALSE)

#For Country Y 
Ymarker_sums <- colSums(Y_markers, na.rm = FALSE)

#make a data frame to store marker info
marker<- data.frame(matrix(ncol = 3, nrow = 10))
y <- c("Marker_name","X_sum","Y_sum")
colnames(marker) <- y

marker$Marker_name <-1:10
marker$X_sum<-Xmarker_sums
marker$Y_sum<-Ymarker_sums


ggplot(marker, aes(x=Marker_name , y = "")) + 
  geom_point(aes(y = X_sum, col = "Country X")) + 
  geom_point(aes(y = Y_sum, col = "Country Y"))+
  theme_bw() +
  xlab("Marker Number") +
  ylab("Marker Counts")+
  theme(legend.title=element_blank())+ggtitle("Marker count comparison between Country X and Y")

##The disease in question can be identified by the presence of different markers.
##Since different markers may elicit different immune responses among patients, 
##this suggests that variants of the disease that are less similar to one another 
##may require different vaccines to effectively protect patients. Given the data
##and the subsequent plot, we can see that patients in Country X are more commonly 
##displaying variants of the disease with markers 1-5, and patients in Country Y 
##are more frequently displaying variants 6:10. Given this difference, a vaccine 
##for patients in Country X is less likely to work for patients in Country Y. 

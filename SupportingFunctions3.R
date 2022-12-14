#Biocomputing R Project Notes
#1. In which country (X or Y) did the disease outbreak likely begin?
#2. If Country Y develops a vaccine for the disease, is it likely to work for citizens of Country X?


#Custom Function 3

DataSet<-read.table("Rproject2022/allData.csv",header=TRUE, sep=",", stringsAsFactors=FALSE)

#Number of screens run can be determined by the number of rows(patients) in the DataSet
NumberOfScreens <- nrow(DataSet)


#Goal: Percent of patients screened that were infected
#Patients with at least one marker are infected patients

#create variables to hold the number of infected and notinfected patients
Infected<-0
NotInfected<-0

#Add up the total number of infected and notinfected patients
for(i in 1:nrow(DataSet)){
  SumRow<-sum(DataSet$marker01[i], DataSet$marker02[i],DataSet$marker03[i],DataSet$marker04[i], DataSet$marker05[i],DataSet$marker06[i],DataSet$marker07[i], DataSet$marker08[i],DataSet$marker09[i],DataSet$marker10[i])
  
  if(SumRow>0){
    
    Infected<-Infected+1
    
  }else{
    
    if(SumRow==0)
      NotInfected<-NotInfected+1
  }
} 

#Percent of population infected can be determined by Infected Patients/Total number of patients screened
PercentInfected<-Infected/NumberOfScreens

#Goal: Compарее Мale vs. Female infected patient percentages

#Infected Male Patients
MaleInfected<-0
MaleNotInfected<-0

#Add up the total number of Male infected and notinfected patients
for(i in 1:nrow(DataSet)){
  if(DataSet$gender[i]=="male"){
    MSumRow<-sum(DataSet$marker01[i], DataSet$marker02[i],DataSet$marker03[i],DataSet$marker04[i], DataSet$marker05[i],DataSet$marker06[i],DataSet$marker07[i], DataSet$marker08[i],DataSet$marker09[i],DataSet$marker10[i])
    
    if(MSumRow>0){
      
      MaleInfected<-MaleInfected+1
      
    }else{
      
      if(MSumRow==0)
        MaleNotInfected<-MaleNotInfected+1
    }
  } 
  
}
MaleTotal=(MaleInfected+MaleNotInfected)

#Percent of Male infected can be determined by Infected Patients/Total number of patients screened
PercentMaleInfected<-MaleInfected/MaleTotal

#Infected Female Patients
FemaleInfected<-0
FemaleNotInfected<-0

#Add up the total number of Male infected and notinfected patients
for(i in 1:nrow(DataSet)){
  if(DataSet$gender[i]=="female"){
    FSumRow<-sum(DataSet$marker01[i], DataSet$marker02[i],DataSet$marker03[i],DataSet$marker04[i], DataSet$marker05[i],DataSet$marker06[i],DataSet$marker07[i], DataSet$marker08[i],DataSet$marker09[i],DataSet$marker10[i])
    
    if(FSumRow>0){
      
      FemaleInfected<-FemaleInfected+1
      
    }else{
      
      if(FSumRow==0)
        FemaleNotInfected<-FemaleNotInfected+1
    }
  } 
  
}
FemaleTotal=(FemaleInfected+FemaleNotInfected)

#Percent of Male infected can be determined by Infected Patients/Total number of patients screened
PercentFemaleInfected<-FemaleInfected/FemaleTotal


#New data frame with infected patient data
InfectedPlot=data.frame()

for(i in 1:nrow(DataSet)){
  SumRow<-sum(DataSet$marker01[i], DataSet$marker02[i],DataSet$marker03[i],DataSet$marker04[i], DataSet$marker05[i],DataSet$marker06[i],DataSet$marker07[i], DataSet$marker08[i],DataSet$marker09[i],DataSet$marker10[i])
  
  if(SumRow>0){
    
    InfectedPlot= rbind(InfectedPlot, DataSet[i,])
    
  }
}


#Age density plot of all infected patients plotted
ggplot(data=InfectedPlot,aes(x=age))+
  geom_density()+
  theme_classic()+
  xlim(0,100)


mean(DataSet$age)
mean(InfectedPlot$age)

#Question 1
#Create dataset with infected patients in each country
for(i in 1:nrow(DataSet)){
  SumRow<-sum(DataSet$marker01[i], DataSet$marker02[i],DataSet$marker03[i],DataSet$marker04[i], DataSet$marker05[i],DataSet$marker06[i],DataSet$marker07[i], DataSet$marker08[i],DataSet$marker09[i],DataSet$marker10[i])
  
  if(SumRow>0){
    
    InfectedPlot= rbind(InfectedPlot, DataSet[i,])
    
  }
}

#Density plot of all infected patients plotted for each country
ggplot(data=InfectedPlot,aes(x=dayOfYear))+
  geom_density()+
  theme_classic()+
  xlim(100,185)

#After running the whole script for each country, we get two plots for the number of infected patients by day of year for each country
#Clearly Country X's infection begins earlier than Country Y thus the disease outbreak likely began in Country X 



#Question2
#Use infected patient data sets to analyze markers found on last day for infections in each country

for(i in 1:nrow(InfectedPlot)){
  
  if(InfectedPlot$dayOfYear[i]==175){
    
    LastDayInfected= rbind(LastDayInfected, InfectedPlot[i,])
    
  }
}

sum(LastDayInfected$marker01)
sum(LastDayInfected$marker02)
sum(LastDayInfected$marker03)
sum(LastDayInfected$marker04)
sum(LastDayInfected$marker05)
sum(LastDayInfected$marker06)
sum(LastDayInfected$marker07)
sum(LastDayInfected$marker08)
sum(LastDayInfected$marker09)
sum(LastDayInfected$marker010)

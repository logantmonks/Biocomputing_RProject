## Intro to Biocomputing Final R Project: Supporting Functions
## Clayton Glasgow, Sydney Harris, Jaden Bailey
## 14 December 2022


#supporting function #3
#Write a function to summarize the compiled data set in terms of
  #number of screens run
  #percent of patients screened that were infected
  #percent of female patients infected and percent of male patients infected with graphs to see if one gender is more susceptible
  #the age distribution graphs of all patients screened and of all patients infected
  #graph of percentage of infected patients for 10 year age group ranges to see if one age group range is more susceptible

#load data
allData <- read.csv ("/users/sydneyharris/desktop/allData.csv", header = T)

#create custom function called summarize_data()
summarize_data <- function(data){
  #find number of screens run
  total <- nrow(data)
  print("number of screens run:")
  print(total)
  #percent total patients infected
  #create column in data set that will hold whether the patient was infected (value=1) or not infected (value=0)
  data$infected <- numeric(length(1:nrow(data)))
  #create variable that will be the total number of patients infected
  num_infect <-0
  for (i in 1:nrow(data)){ #for each row in the data set, sum the values of all of the marker columns
    if (sum(data[i, 3:12]) >=1){ #if the sum is greater than 1 (ie. the patient has at least one marker)
      data$infected[i] <- 1 #make the value in the column "infected" equal to 1, meaning that patient is infected
    }else if (sum(data[i, 3:12])<1){ #if the sum is less than 1 (ie. the patient has no markers)
      data$infected[i] <- 0 #make the value in the column "infected" equal to 0, meaning that patient is not infected
    }
  }
  #sum values in the "infected" column which will give the total number infected
  num_infect <- sum(data[,"infected"])
  #calculate the percent of patients infected by dividing the total number infected by the total number of screens run
  percent_infected <- (num_infect/total)*100
  print("percent of all patients infected:")
  print(percent_infected)
  #number of male vs female infected
  #create variables for:
  #number of female patients infected
  num_female_inf <- 0
  #total number of female patients
  total_female <-0
  #number of male patients infected
  num_male_inf <- 0
  #total number of male patients
  total_male <- 0
  #for each row in the data set: evaluate whether the patient is female or male and whether they are infected or not
  for (i in 1:nrow(data)) { 
    if (data$gender[i] == "female"){ #if the patient (row i) is female
      total_female <- total_female+1 #add 1 to the number of total female patients
      if (data$infected[i] == 1){ # if the patient is female and infected (has value=1 in "infected" column)
        num_female_inf <- num_female_inf+1 #add 1 to number of infected females
      }else if (data$infected[i] == 0){ # if the patient is female and not infected (has value=0 in "infected" column)
        num_female_inf <- num_female_inf+0 #add 0 to number of infected females
      }
    } else if (data$gender[i] == "male"){ #if the patient (row i) is male
      total_male <- total_male+1 #add 1 to the number of total male patients
      if (data$infected[i] == 1){ # if the patient is male and infected (has value=1 in "infected" column)
        num_male_inf <- num_male_inf+1 #add 1 to number of infected males
      }else if (data$infected[i] == 0){ # if the patient is male and not infected (has value=0 in "infected" column)
        num_male_inf <- num_male_inf+0 #add 0 to number of infected males
      }
    }
  }
  #calculate percentages of each gender that is infected by dividing the number infected patients of that gender by the total number of patients screened of that gender
  female_inf_per <- (num_female_inf/total_female)*100
  male_inf_per <- (num_male_inf/total_male)*100
  print("percent of female patients infected:")
  print (female_inf_per)
  print("percent of male patients infected:")
  print(male_inf_per)
  #age distribution and infection percentage graphs
  #load ggplot
  library(ggplot2)
  #make a histogram to visualize age distribution of all patients screened
  allData_graph <-ggplot (data, aes (x=age))+
    #make bin width equal to 10 years, ranging from 0-10 to 90-100
    #exclude data for patients older than 100, which was likely an error in data collection or entry
    geom_histogram(binwidth=10, breaks = c(10,20,30,40,50,60,70,80,90,100))+ 
    xlab("Age")+
    ylab("Number of Patients Screened")+
    theme_classic()+
    labs(title="Age Distribution of Patients Screened")
  print(allData_graph)
  #create data frame with only infected patients
  infected_data <-  data[data$infected == "1",]
  #make a histogram to visualize age distribution of all infected patients
  infected_graph <-ggplot (infected_data, aes (x=age))+
    #make bin width equal to 10 years, ranging from 0-10 to 90-100
    #exclude data for patients older than 100, which was likely an error in data collection or entry
    geom_histogram(binwidth=10, breaks = c(10,20,30,40,50,60,70,80,90,100))+
    xlab("Age")+
    ylab("Number of Infected Patients")+
    theme_classic()+
    labs(title="Age Distribution of Patients Infected")
  print(infected_graph)
  #create graph to show percent of patients infected for each 10 year age range
  #create variables to store information for total number of patients screened and number of patients infected within an age group
  infected_10 <- 0
  total_10 <-0
  infected_20 <- 0
  total_20 <-0
  infected_30 <- 0
  total_30 <-0
  infected_40 <- 0
  total_40 <-0
  infected_50 <- 0
  total_50 <-0
  infected_60 <- 0
  total_60 <-0
  infected_70 <- 0
  total_70 <-0
  infected_80 <- 0
  total_80 <-0
  infected_90 <- 0
  total_90 <-0
  infected_100 <- 0
  total_100 <-0
  #for each row in the data set: determine if age is within a 10 year range and whether the patient is infected
  for (i in 1:nrow(data)) { 
    #create variable for the age of the patient
    age <- data[i,"age"]
    #if the patient is 0 to 10 years old:
    if (0<age && age<=10){ 
      #add 1 to the total number of 0-10 year old patients screened
      total_10 <- total_10+1 
      if (data$infected[i] == 1){ 
        #add 1 to the total number of 0-10 year old patients infected
        infected_10 <- infected_10+1 
      }else if (data$infected[i] == 0){ 
        #add 0 to the total number of 0-10 year old patients infected
        infected_10 <- infected_10+0
      }
    #if the patient is 11 to 20 years old:
    } else if (10<age && age<=20){ 
      #add 1 to the total number of 11-20 year old patients screened
      total_20 <- total_20+1 
      if (data$infected[i] == 1){ 
        #add 1 to the total number of 11-20 year old patients infected
        infected_20 <- infected_20+1 
      }else if (data$infected[i] == 0){ 
        #add 0 to the total number of 11-20 year old patients infected
        infected_20 <- infected_20+0
      }
    #if the patient is 21 to 30 years old:
    }else if (20<age && age<=30){ 
      #add 1 to the total number of 21-30 year old patients screened
      total_30 <- total_30+1 
      if (data$infected[i] == 1){ 
        #add 1 to the total number of 21-30 year old patients infected
        infected_30 <- infected_30+1 
      }else if (data$infected[i] == 0){ 
        #add 0 to the total number of 21-30 year old patients infected
        infected_30 <- infected_30+0
      }
    #if the patient is 31 to 40 years old:
    }else if (30<age && age<=40){ 
      #add 1 to the total number of 31-40 year old patients screened
      total_40 <- total_40+1 
      if (data$infected[i] == 1){ 
        #add 1 to the total number of 31-40 year old patients screened
        infected_40 <- infected_40+1 
      }else if (data$infected[i] == 0){ 
        #add 0 to the total number of 31-40 year old patients infected
        infected_40 <- infected_40+0
      }
    #if the patient is 41 to 50 years old:
    }else if (40<age && age<=50){ 
     #add 1 to the total number of 41-50 year old patients screened
      total_50 <- total_50+1 
      if (data$infected[i] == 1){
        #add 1 to the total number of 41-50 year old patients infected
        infected_50 <- infected_50+1 
      }else if (data$infected[i] == 0){ 
        #add 0 to the total number of 41-50 year old patients infected
        infected_50 <- infected_50+0
      }
    #if the patient is 51 to 60 years old:
    }else if (50<age && age<=60){ 
      #add 1 to the total number of 51-60 year old patients screened
      total_60 <- total_60+1 
      if (data$infected[i] == 1){ 
        #add 1 to the total number of 51-60 year old patients infected
        infected_60 <- infected_60+1 
      }else if (data$infected[i] == 0){ 
        #add 0 to the total number of 51-60 year old patients infected
        infected_60 <- infected_60+0
      }
    #if the patient is 61 to 70 years old:
    }else if (60<age && age<=70){ 
      #add 1 to the total number of 61-70 year old patients screened
      total_70 <- total_70+1 
      if (data$infected[i] == 1){ 
        #add 1 to the total number of 61-70 year old patients infected
        infected_70 <- infected_70+1 
      }else if (data$infected[i] == 0){ 
        #add 0 to the total number of 61-70 year old patients infected
        infected_70 <- infected_70+0
      }
    #if the patient is 71 to 80 years old:
    }else if (70<age && age<=80){ 
      #add 1 to the total number of 71-80 year old patients screened
      total_80 <- total_80+1 
      if (data$infected[i] == 1){ 
        #add 1 to the total number of 71-80 year old patients infected
        infected_80 <- infected_80+1 
      }else if (data$infected[i] == 0){ 
        #add 0 to the total number of 71-80 year old patients infected
        infected_80 <- infected_80+0
      }
    #if the patient is 81 to 90 years old:
    }else if (80<age && age<=90){ 
      #add 1 to the total number of 81-90 year old patients screened
      total_90 <- total_90+1 
      if (data$infected[i] == 1){ 
        #add 1 to the total number of 81-90 year old patients infected
        infected_90 <- infected_90+1 
      }else if (data$infected[i] == 0){ 
        #add 0 to the total number of 81-90 year old patients infected
        infected_90 <- infected_90+0
      }
    #if the patient is 91 to 100 years old:
    }else if (90<age && age<=110){ 
      #add 1 to the total number of 81-90 year old patients screened
      total_100 <- total_100+1 
      if (data$infected[i] == 1){ 
        #add 1 to the total number of 91-100 year old patients infected
        infected_100 <- infected_100+1 
      }else if (data$infected[i] == 0){ 
        #add 0 to the total number of 91-100 year old patients infected
        infected_100 <- infected_100+0
      }
    }
  }
  #calculate percentages of infected patients for each age group 
  percent_10 <- (infected_10/total_10)*100
  percent_20 <- (infected_20/total_20)*100
  percent_30 <- (infected_30/total_30)*100
  percent_40 <- (infected_40/total_40)*100
  percent_50 <- (infected_50/total_50)*100
  percent_60 <- (infected_60/total_60)*100
  percent_70 <- (infected_70/total_70)*100
  percent_80 <- (infected_80/total_80)*100
  percent_90 <- (infected_90/total_90)*100
  percent_100 <- (infected_100/total_100)*100
  #create vector with all age ranges
  age_ranges <- c("0-10", "11-20","21-30","31-40","41-50","51-60","61-70","71-80","81-90","91-100")
  #create vector with all infected patient percentages for each age group
  age_percents <- c(percent_10, percent_20, percent_30, percent_40, percent_50, percent_60, percent_70, percent_80, percent_90, percent_100)
  #create data frame for each age group and the percent of patients infected
  age_infected_data <- data.frame(age_ranges, age_percents)
  #graph data to display percent of patients infected for each each group to see if one age group is more susceptible
  infected_by_age_graph <- ggplot(age_infected_data, aes(x=age_ranges, y=age_percents))+
    geom_col()+
    xlab("Age Ranges")+
    ylab("Percent of Patients Infected")+
    ylim(min=0, max=100)+
    theme_classic()
  print(infected_by_age_graph)
  print(age_infected_data)
}


# PeterMartin_supportingFunctions.R
# Code for functions necessary to format and analyze the CDC data

txt.to.csv<-function(file_name,path_to_file=getwd()){
  # This function converts one or multiple .txt files into .csv file format
  
  # Necessary input is the file name(s), as well as the path to the file(s) (default is current working
  # directory)
  path_to_file<-paste(path_to_file,"/",sep = "")
  # Edits the description of the file path into a string that can be concatenated with the file name
  
  # Read in each .txt file, substitute .csv for .txt in the file name, and save the table as a .csv file
  for(i in file_name){
    table<-read.table(file = paste(path_to_file,i,sep = ""),header = TRUE,sep = " ")
    i<-gsub(".txt",".csv",i)
    write.table(table,file=i,sep = ",",row.names = FALSE)
  }
}

compile.csv<-function(path_to_file=getwd(),na.rm="ignore",nFiles){
  # This function compiles data from all .csv files in a directory into a single .csv file (compileData.csv). 
  # The compiled data has the original twelve columns of each file, but also country and dayofYear columns. 
  # The user is able to choose whether they want to remove rows that contain NA values, include NAs in the 
  # compiled data but be warned of their presence, or include NAs in the compiled data without a warning
  
  # Necessary input is the path(s) to the file(s) (default is current working
  # directory), the specification of how to treat NA values in the data ("warn": include NAs in the compiled data frame
  # but deliver a warning for each file that contains NA values; "yes": remove all rows that contain NAs; "ignore" (default):
  # ignore NA values and include them in the compiled data frame without a warning), and the number
  # of files to include in each path (can thereby make temporal comparisons) (must be a vector with 
  # length equal to the length of path_to_file)
  
  Ncountries<-length(path_to_file)
  for(i in 1:Ncountries){
    # Make each file path able to be concatenated with the file name, and load all .csv files within each specified
    # file path
    path_to_file[i]<-paste(path_to_file[i],"/",sep = "")
    file_name = list.files(path_to_file[i], pattern = "csv")
    file_name<-file_name[1:nFiles[i]]
    
    # Proceed with code on a file by file basis
    for(j in 1:length(file_name)){
      # Read in each file and add two columns to the table: a column containing the name of the country in which
      # the screenings were done, and the day of the year (1-365) on which the screenings were performed
      # (Info for both columns are pulled from the file name)
      table<-read.csv(paste(path_to_file[i],file_name[j],sep = ""),header = TRUE,stringsAsFactors = TRUE)
      table$country<-substr(path_to_file[i],nchar(path_to_file[i])-1,nchar(path_to_file[i])-1)
      table$dayofYear<-as.numeric(substr(file_name[j],nchar(file_name[j])-6,nchar(file_name[j])-4))
      
      # First check if there are any NA values in the table
      if(anyNA(table[,])==TRUE){
        # Code executed for the different possible arguments of na.rm
        if(na.rm == "warn"){
          print(paste0("Warning: there are one or more missing values in ",file_name[j]))
        }else if(na.rm == "yes"){
          for(n in 1:nrow(table)){
            if(anyNA(table[n,3:12])==TRUE){
              table$anyNA[n]<-"Y"
            }else{
              table$anyNA[n]<-"N"
            }
          }
          table<-table[table$anyNA=="N",]
        }
      }
      
      # Compiles tables generated from all files located within one path, using rbind()
      if(j==1){
        compileDF<-table
      }else{
        compileDF<-rbind(compileDF,table)
      }
    }
    
    # If statement to bind the compiled data frames from each path together (if there is more than one path)
    if(i==1){
      compileData<-compileDF
    }else{
      compileData<-rbind(compileData,compileDF)
    }
  }
  
  # Creates another column with the total number of microsatellites detected in a given individual
  for(m in 1:nrow(compileData)){
    compileData$microsat_num[m]<-sum(compileData[m,3:12])
  }
  
  # Writes a file called "compileData.csv" from the data frame containing all the screening information
  write.table(compileData,file="compileData.csv",sep = ",",row.names = FALSE)
}

ageFilter<-function(x,age_cutoff=113){
  # A function which removes all screened individuals from a data frame that are beyond a certain age cutoff
  
  # Necessary input is a data frame (x), as well as the age cutoff (default of 113 years)
  x<-x[x$age<age_cutoff,]
}

microsatFreq<-function(x,multiCountry=TRUE){
  # This function is used to compare microsatellite detection within and across countries, using
  # ANOVAs and Tukey's test to check for significant differences. In other words, this function
  # can help detect the presence of multiple strains of the disease.
  
  # One must specify a data frame (x), as well as whether there are multiple countries being analyzed (default
  # is TRUE)
  
  # Reshape the data frame so that an ANOVA can be run on the data (specifically on microsatellite data)
  data_reshape <- data.frame(gender = x$gender, age = x$age, country = x$country, dayofYear = x$dayofYear,                        
                             markerPresence = c(x$marker01, x$marker02, x$marker03, x$marker04, x$marker05, x$marker06, 
                                             x$marker07, x$marker08, x$marker09, x$marker10),
                             markerID = c(rep("marker01", nrow(x)),
                                              rep("marker02", nrow(x)),
                                              rep("marker03", nrow(x)),
                                              rep("marker04", nrow(x)),
                                              rep("marker05", nrow(x)),
                                              rep("marker06", nrow(x)),
                                              rep("marker07", nrow(x)),
                                              rep("marker08", nrow(x)),
                                              rep("marker09", nrow(x)),
                                              rep("marker10", nrow(x))))
  
  
  # Type of ANOVA to be performed: if more than one country, perform a two-way ANOVA with interaction effect
  # Otherwise, perform a one-way ANOVA comparing marker presence among different microsatellites
  if(multiCountry==TRUE){
    res.aov <- aov(markerPresence ~ country * markerID, data = data_reshape)
    print("Results of a two-way ANOVA analyzing the effect of country and markerID on the frequency of microsatellite detection")
  }else{
    res.aov<-aov(markerPresence ~ markerID,data = data_reshape)
    print("Results of a one-way ANOVA analyzing the effect of markerID on the frequency of microsatellite detection")
  }
  print(summary(res.aov))
  print("-")
  print("-")
  
  # Tukey's test (post hoc test on results of ANOVA)
  tukey <- TukeyHSD(res.aov)
  
  # Creates a "compact letter display" (i.e., assigns significance letters to different groups based on post hoc test
  # results) and converts this display to a data frame (selecting only the test of interest)
  tukey.cld <- multcompLetters4(res.aov, tukey)
  if(multiCountry==TRUE){
    cld <- as.data.frame.list(tukey.cld$`country:markerID`)
  }else{
    cld <- as.data.frame.list(tukey.cld$`markerID`)
  }
  
  # Creates a summary table of the reshaped data frame with mean presence/absence and standard error 
  # of each microsatellite ID (makes the following comparisons much faster in execution)
  summaryTable<-aggregate(data_reshape$markerPresence, list(markerID=data_reshape$markerID,country=data_reshape$country),FUN=mean)
  standard_error<-aggregate(data_reshape$markerPresence, list(markerID=data_reshape$markerID,country=data_reshape$country),FUN=std.error)
  summaryTable$se<-standard_error$x
  
  # Creates two search strings: markerN (the marker ID) and country (the country with which data of a given row is
  # associated). Each variable is the same length as cld and is ordered according to the order in which the
  # post hoc test made its comparisons. The following for loop looks at each row of the summary table and, using the 
  # search strings, assigns it its corresponding significance letter from the information stored in cld.
  # The sumamry table stores these letters in a 5th column: sigLet
  rN<-rownames(cld)
  if(multiCountry==FALSE){
    markerN<-rN
    country<-as.character(summaryTable$country)
  }else{
    markerN<-substr(rN,nchar(rN)-7,nchar(rN))
    country<-sub(":.*","",rN)
  }
  for(p in 1:nrow(summaryTable)){
    for(q in 1:nrow(cld)){
      if(markerN[q]==summaryTable$markerID[p] && country[q]==summaryTable$country[p]){
        summaryTable$sigLet[p]<-cld$Letters[q]
      }
    }
  }
  
  # nColor determines the number of colors desired for the wes_palette function within ggplot
  nColor<-length(unique(summaryTable$country))
  
  # Plots summary table data in barplot form (with error bars and letters of significance)
  microsat<-ggplot(summaryTable, aes(x = markerID, y = x, fill=country)) +
    geom_bar(position = "dodge",stat="identity",width = 0.85) +
    geom_errorbar(aes(ymin=x-se, ymax=x+se),
                  width=0.2,                    # Width of the error bars
                  position=position_dodge(.85)) +
    geom_text(aes(label=sigLet, y = x+0.015), size = 3, position = position_dodge(width=0.85)) +
    xlab("Microsatellite ID") +
    ylab("Proportion of individuals who tested positive for a marker") +
    theme_bw() +
    theme(axis.text.x = element_text(angle=65, vjust=0.6)) +
    # If else statements that control fill pattern of boxes
    if(nColor>5){
      scale_fill_manual('Country', values=wes_palette(name="GrandBudapest1",n=nColor,type = "continuous"))
    }else{
      scale_fill_manual('Country', values=wes_palette(name="GrandBudapest1",n=nColor))
    }
  print(microsat)
}

microsatNum<-function(x,multiCountry=TRUE){
  # This function is used to compare the average number of microsatellites detected in a positive test across countries, 
  # using an ANOVA and Tukey's test to check for significant differences
  
  # One must specify a data frame (x), as well as whether there are multiple countries being analyzed (default
  # is TRUE)
  if(multiCountry==FALSE){
    # Prints a boxplot displaying the average number of microsatellites detected in patients for the single country
    microsat<-ggplot(positive_compileData,aes(x=country,y=microsat_num,fill=country)) +
      geom_boxplot() +
      xlab("Country") +
      ylab("# of Microsatellites Detected") +
      ylim(0,7) +
      theme_bw() +
      scale_fill_manual('Country', values=wes_palette(name="GrandBudapest1",n=1))
    print(microsat)
  }else{
    # Performs a one-way ANOVA comparing the number of markers detected among different countries
    res.aov<-aov(microsat_num ~ country,data = x)
    print("Results of a one-way ANOVA analyzing the effect of country on the number of microsatellites detected")
    print(summary(res.aov))
    
    # nColor determines the number of colors desired for the wes_palette function within ggplot
    nColor<-length(unique(x$country))
    if(nColor>2){
      print("-")
      print("-")
      # Tukey's test (post hoc test on results of ANOVA) (only if there are more than two countries)
      tukey <- TukeyHSD(res.aov)
      print("Results of Tukey's test")
      print(summary(tukey))
    }
    
    # Specifies the countries being compared for geom_signif()
    country<-as.character(unique(x$country))
    
    # Prints a boxplot comparing the average number of microsatellites detected in patients across the different countries
    # Also uses geom_signif() to depict significant differences between countries
    microsat<-ggplot(x, aes(x = country, y = microsat_num, fill=country)) +
      geom_boxplot() +
      geom_signif(comparisons = list(country), map_signif_level = TRUE, tip_length = 0) +
      xlab("Country") +
      ylab("# of Microsatellites Detected") +
      ylim(0,7) +
      theme_bw() +
      # If else statements that control fill pattern of boxes
      if(nColor>5){
        scale_fill_manual('Country', values=wes_palette(name="GrandBudapest1",n=nColor,type = "continuous"))
      }else{
        scale_fill_manual('Country', values=wes_palette(name="GrandBudapest1",n=nColor))
      }
    print(microsat)
  }
}

screenStats<-function(file_name,path_to_file=getwd()){
  # This function summarizes the compiled data set in terms of number of screens run, 
  # percent of patients screened that were infected, male vs. female patients, and the age 
  # distribution of patients. It also compares the average number of microsatellites detected between countries.
  
  # Necessary input is a file name (assumed to be some sort of summary .csv file of compiled data), 
  # as well as the path to the file (default is the current working directory)
  
  # Loads .csv file and applies the ageFilter() function described above
  compileData<-read.csv(file = paste0(path_to_file,"/",file_name),header = TRUE,stringsAsFactors = TRUE)
  compileData<-ageFilter(x=compileData)
  
  # Creates a new column in the data frame that contains the number of microsatellites detected for each patient
  for(j in 1:nrow(compileData)){
    compileData$microsat_num[j]<-sum(compileData[j,3:12])
  }
  
  # Computes and prints total number of positive tests in the entire data frame, across all countries
  positive_compileData<-compileData[compileData$microsat_num>0,]
  print(paste0("Across all countries included in this study, ",nrow(positive_compileData)," people have tested positive"))
  print("—")
  
  # Creates a vecor of unique countries contained within the data set, and then uses a for loop to perform
  # a set of actions on the subset of the data frame that refers to each unique country 
  n_country<-unique(compileData$country)
  for(i in 1:length(n_country)){
    # Loads the subset of the data, counts the number of screens performed in that country, creates another 
    # data subset that only includes the screenings which tested positive (i.e., one or more microsatellites detected),
    # and counts the number of rows in that subset of a subset 
    country<-compileData[compileData$country==n_country[i],]
    n_screens<-nrow(country)
    positiveScreens<-country[country$microsat_num>0,]
    positive_count<-nrow(positiveScreens)
    
    # Calculate the percentage of positive tests from all the screenings performed in that country
    rawPercent<-(positive_count/n_screens)*100
    Percent<-round(rawPercent,digits=2)
    
    # Counts the number of each sex screened, as well as the number of each sex who tested positive
    male<-nrow(country[country$gender=="male",])
    female<-nrow(country[country$gender=="female",])
    positive_male<-nrow(positiveScreens[positiveScreens$gender=="male",])
    positive_female<-nrow(positiveScreens[positiveScreens$gender=="female",])
    
    # Calculation of summary statistics for the age data of those screened, as well as a histogram plot that shows
    # the frequency distribution of different age categories (binwidth=5) in the screening data
    dist<-summary(country$age)
    dist<-cbind(dist,sd(country$age))
    age_dist<-ggplot(country,aes(age)) +
      geom_histogram(binwidth = 5,fill=wes_palette(name="GrandBudapest1",n=23,type="continuous")) +
      ggtitle(paste0("Age distribution of those screened in country ",n_country[i])) +
      xlab("Age") +
      theme_bw()
    print(age_dist)
    
    # Code that gives an output to the console summarizing the important screening statistics 
    print(paste0("There have been a total of ",n_screens," screenings in country ",n_country[i]))
    print(paste0(positive_count," people have tested positive"))
    print(paste0(female," females have been screened, of which ",positive_female," have tested positive"))
    print(paste0(male," males have been screened, of which ",positive_male," have tested positive"))
    print(paste0("The average age screened was ",round(dist[4],digits = 2),", the minimum age was ",dist[1],
                 ", and the maximum was ",dist[6]))
    print(paste0("The standard deviation in age screened was ",dist[7]))
    print("—")
    print(paste0(Percent,"% of people screened in country ",n_country[i]," have tested positive"))
    print("—")
    print("-")
  }
  
  # Applies the microsatFreq() and microsatNum() functions to the subset of the compiled data frame 
  # that includes all positive tests
  microsatFreq(positive_compileData)
  microsatNum(positive_compileData)
}


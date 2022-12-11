# PeterMartin_supportingFunctions.R

txt.to.csv<-function(file_name,path_to_file=getwd()){
  # This function converts one or multiple .txt files into .csv file format
  
  # Necessary input is the file name(s), as well as the path to the file(s) (default is current working
  # directory)
  path_to_file<-paste(path_to_file,"/",sep = "")
  # Edits the description of the path to file into a string that can be concatenated with the file name
  
  # Read in each .txt file, substitute .csv for .txt in the file name, and save the table as a .csv file
  for(i in file_name){
    table<-read.table(file = paste(path_to_file,i,sep = ""),header = TRUE,sep = " ")
    i<-gsub(".txt",".csv",i)
    write.table(table,file=i,sep = ",",row.names = FALSE)
  }
}

compile.csv<-function(file_name,path_to_file=getwd(),nPaths=1,na.rm="warn"){
  # This function compiles data from all .csv files in a directory into a single .csv file (compileData.csv). 
  # The compiled data has the original twelve columns from daily data sheets, but also country and dayofYear columns. 
  # The user is able to choose whether they want to remove rows with NA’s in any columns, include NAs in the 
  # compiled data but be warned of their presence, or include NAs in the compiled data without a warning
  
  # Necessary input is the file name(s), the path to the file(s) (default is current working
  # directory), the number of paths to files that need to be compiled (i.e. the number of countries, with a 
  # default of 1), and the option as to how to treat NAs in the data ("warn": includes NAs in the compiled data 
  # but delivers a warning for each file that contains NA values; "yes": removes all rows that contain NAs; "ignore":
  # ignores NA values and includes them in the compiled data frame without a warning)
  
  for(i in 1:nPaths){
    # Make each file path able to be concatenated with the file name
    path_to_file[i]<-paste(path_to_file[i],"/",sep = "")
    
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
      
      # Compiles tables generated from all files located within one path
      if(j==1){
        compileDF<-table
      }else{
        compileDF<-rbind(compileDF,table)
      }
    }
    
    # If statements to check if there are multiple paths, and if so to bind the compiled data frames from each path 
    # together 
    if(i==1){
      compileData<-compileDF
    }else{
      compileData<-rbind(compileData,compileDF)
    }
  }
  
  # Creates another column with the total number of microsatellites for the disease detected in a given individual
  for(m in 1:nrow(compileData)){
    compileData$microsat_num[m]<-sum(compileData[m,3:12])
  }
  
  # Writes a file called "compileData.csv" from the data frame containing all the data
  write.table(compileData,file="compileData.csv",sep = ",",row.names = FALSE)
}

ageFilter<-function(x,age_cutoff=113){
  # A function which removes all screened individuals from a data frame that are beyond a preset age cutoff (default
  # of 113)
  
  # Necessary input is a data frame (x), as well as the age cutoff (default of 113)
  x<-x[x$age<age_cutoff,]
}

microsatMean<-function(x,multiCountry=TRUE){
  # This function summarizes the compiled data set in terms of number of screens run, 
  # percent of patients screened that were infected, male vs. female patients, and the age 
  # distribution of patients. It also compares microsatellite detection within and across countries, using
  # ANOVAs to test for significance
  
  # Necessary input is a data frame (x), as well as whether there are more than one countries being analyzed (default
  # is TRUE)
  
  # Reshape the data frame so that an ANOVA can be run on the data (specifically on microsatellite data)
  data_reshape <- data.frame(gender = x$gender, age = x$age, country = x$country, dayofYear = x$dayofYear,                         # Reshape data frame
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
  }else{
    res.aov<-aov(markerPresence ~ markerID,data = data_reshape)
  }
  print(summary(res.aov))
  
  # Tukey's test (post hoc test on results of ANOVA)
  tukey <- TukeyHSD(res.aov)
  
  # Creates a "compact letter display" (i.e., assigns significance letters to different groups based on post hoc test
  # results) and converting this display to a data frame (selecting only the test of interest)
  tukey.cld <- multcompLetters4(res.aov, tukey)
  if(multiCountry==TRUE){
    cld <- as.data.frame.list(tukey.cld$`country:markerID`)
  }else{
    cld <- as.data.frame.list(tukey.cld$`markerID`)
  }
  
  # Creates a summary table of reshaped data frame with mean presence/absence and standard error 
  # of each microsatellite ID (makes following comparisons much faster in execution)
  summaryTable<-aggregate(data_reshape$markerPresence, list(markerID=data_reshape$markerID,country=data_reshape$country),FUN=mean)
  standard_error<-aggregate(data_reshape$markerPresence, list(markerID=data_reshape$markerID,country=data_reshape$country),FUN=std.error)
  summaryTable$se<-standard_error$x
  
  # Creates two search strings: rN (the marker ID) and country (the country with which data of a given row is
  # associated). Each variable is the same length as cld and is ordered according to the order in which the
  # post hoc test made its comparisons. The following for loop looks at each row of the summary table and, using the 
  # search strings, assigns it its corresponding significance letter from the information stored in cld
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
  
  # Plots summary table data in barplot form (with error bars)
  microsat<-ggplot(summaryTable, aes(x = markerID, y = x, fill=country)) +
    geom_bar(position = "dodge",stat="identity",width = 0.85) +
    geom_errorbar(aes(ymin=x-se, ymax=x+se),
                  width=0.2,                    # Width of the error bars
                  position=position_dodge(.85)) +
    geom_text(aes(label=sigLet, y = x+0.015), size = 3, position = position_dodge(width=0.85)) +
    xlab("Microsatellite #") +
    ylab("Frequency of marker detection in positive scan") +
    theme_bw() +
    theme(axis.text.x = element_text(angle=65, vjust=0.6)) +
    scale_fill_manual('Country', values=wes_palette(name="GrandBudapest1",n=nColor))
  print(microsat)
}

screenStats<-function(file_name,path_to_file=getwd()){
  compileData<-read.csv(file = paste0(path_to_file,"/",file_name),header = TRUE,stringsAsFactors = TRUE)
  compileData<-ageFilter(x=compileData)
  n_country<-unique(compileData$country)
  run_sum<-0
  for(j in 1:nrow(compileData)){
    compileData$microsat_num[j]<-sum(compileData[j,3:12])
    #if(sum(compileData[j,3:12]>0)){
      #run_sum<-run_sum+1
      #if(run_sum==1){
        #positive_test<-paste0("Patient ",j," tested on day ",compileData$dayofYear[j]," in country ",
                              #compileData$country[j]," has tested positive with ",sum(compileData[j,3:12]),
                              #" microsatellite(s) detected")
      #}else{
        #positive_test<-rbind(positive_test,paste0("Patient ",j," tested on day ",compileData$dayofYear[j],
                                                  #" in country ",compileData$country[j]," has tested positive with ",
                                                  #sum(compileData[j,3:12])," microsatellite(s) detected"))
      #}
    #}
  }
  #write.table(positive_test,file = "positive_test_info.txt",sep = " ",col.names = FALSE,row.names = FALSE)
  for(i in 1:length(n_country)){
    n_screens<-nrow(compileData[compileData$country==n_country[i],])
    country<-compileData[compileData$country==n_country[i],]
    positive_country<-country[country$microsat_num>0,]
    country_count<-nrow(country[country$microsat_num>0,])
    decimal<-(country_count/n_screens)*100
    percent<-round(decimal,digits=2)
    male<-nrow(country[country$gender=="male",])
    female<-nrow(country[country$gender=="female",])
    positive_male<-nrow(positive_country[positive_country$gender=="male",])
    positive_female<-nrow(positive_country[positive_country$gender=="female",])
    
    dist<-summary(country$age)
    dist<-cbind(dist,sd(country$age))
    
    age_dist<-ggplot(country,aes(age)) +
      geom_histogram(binwidth = 5,fill=wes_palette(name="GrandBudapest1",n=23,type="continuous")) +
      ggtitle(paste0("Age distribution of those screened in country ",n_country[i])) +
      theme_bw()
    print(age_dist)
    
    print(paste0("There have been a total of ",n_screens," screenings in country ",n_country[i]))
    print(paste0(country_count," people have tested positive"))
    print(paste0(female," females have been screened, of which ",positive_female," have tested positive"))
    print(paste0(male," males have been screened, of which ",positive_male," have tested positive"))
    print(paste0("The average age screened was ",round(dist[4],digits = 2),", the minimum age was ",dist[1],
                 ", and the maximum was ",dist[6]))
    print(paste0("The standard deviation in age screened was ",dist[7]))
    print("—")
    print(paste0(percent,"% of people screened in country ",n_country[i]," have tested positive"))
    print("—")
  }
  
  positive_compileData<-compileData[compileData$microsat_num>0,]
  
  print(paste0("Across all countries included in this study, ",nrow(positive_compileData)," people have tested positive"))
  
  microsatMean(positive_compileData)
  
  microsat<-ggplot(positive_compileData,aes(x=country,y=microsat_num,fill=country)) +
    geom_boxplot() +
    xlab("Country") +
    ylab("# of Microsatellites Detected") +
    ylim(0,7) +
    theme_bw()
  print(microsat)
}


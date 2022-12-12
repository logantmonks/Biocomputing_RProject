convertToCSV <- function(dirCountry){
  #sets to country working directory
  setwd(dirCountry)
  #creates list of files with the .txt ending
  filelist = list.files(pattern = "\\.txt")
  #change the name and converts .txt to .csv
  for (i in 1:length(filelist)){
    input<-filelist[i]
    output<-paste0(gsub("\\.txt$","",input),".csv")
    data = read.table(input, header = TRUE)   
    write.table(data, file=output, sep=",", col.names=TRUE)
  }
}

compileXY <- function(dirX,dirY){
  X_file_screen <- list.files(dirX,pattern = "\\.csv$")
  temp_dfX <- data.frame()
  setwd(dirX)
  for(i in 1:length(X_file_screen)){
    #read the file
    currentFile = read.csv(X_file_screen[i],header = TRUE)
    # Add columns for country and dayofYear
    currentFile$country <- "X"
    currentFile$dayofYear <- (i + 119)
    #Append the current file
    temp_dfX = rbind(temp_dfX, currentFile)
  }
  Y_file_screen <- list.files(dirY,pattern = "\\.csv$")
  temp_dfY <- data.frame()
  setwd(dirY)
  for(i in 1:length(Y_file_screen)){
    #read the file
    currentFile = read.csv(Y_file_screen[i],header = TRUE)
    # Add columns for country and dayofYear
    currentFile$country <- "Y"
    currentFile$dayofYear <- (i + 119)
    #Append the current file
    temp_dfY = rbind(temp_dfY, currentFile)
  }
  allData <- rbind(temp_dfX,temp_dfY)
  return(allData)
}

screenStats <- function(allData){
  # Returns the total number of screens by the dimensions of a Dataframe of just screens
  screen_Data <- allData[,3:12]
  screen_Data_dim <- dim(screen_Data)
  totalScreens <- screen_Data_dim[1] * screen_Data_dim[2]
  
  # Number of Infected patients
  for(i in 1:nrow(allData)){
    allData$any_marker[i] <- sum(allData[i,3:12])
  }
  
  for(i in 1:nrow(allData)){
    if(allData$any_marker[i] < 1){
      allData$any_marker[i] = "Not Infected"
    }else{
      allData$any_marker[i] = "Infected"
    }
  }
  
  number_infected = 0 #initialize
  # Proportion of infected patients
  for(i in 1:nrow(allData)){
    if(allData$any_marker[i] == "Infected"){
      number_infected <- number_infected + 1
    }
  }
  percentInfected <- number_infected/nrow(allData) * 100

  # Male v Female Patients
  malePatients <- c()
  femalePatients <- c()
  for(i in 1:length(allData$gender)){
    malePatients <- sum(allData$gender == "male")
    femalePatients <- sum(allData$gender == "female")
  }
  
  # Removing extraneous age data, unlikely someone is over 120
  ages <- allData$age
  
  for(i in 1:length(ages)){
    if((!is.na(ages[i])) & (ages[i] > 120)){
      ages[i] <- NA
    }
  }
  allData$age <- ages
  
  # Plotting age distribution in ggplot by gender
  library(ggplot2)
  p <- ggplot(allData,aes(x = age, fill = gender)) +
    geom_histogram()
  
  output <- data.frame(totalScreens,percentInfected,malePatients,femalePatients)
  
  output_final <- c()
  output_final[[1]] <- p
  output_final[[2]] <- output
  return(output_final)
}

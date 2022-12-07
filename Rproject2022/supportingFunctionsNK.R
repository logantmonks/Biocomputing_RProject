
### COUNTRY X ####
setwd("~/GitHub/Biocomputing_RProject/Rproject2022/Rproject2022")

screen_files <- list.files("~/GitHub/Biocomputing_RProject/Rproject2022/Rproject2022/countryX")

#temporary data frame to load the contents on the current file
temp_dfX <- data.frame()

setwd("~/GitHub/Biocomputing_RProject/Rproject2022/Rproject2022/countryX")
#reading each file within the range and append them to create one file
for (i in 1:length(screen_files)){
  #read the file
  currentFile = read.csv(screen_files[i])
  # Add columns for country and dayofYear
  currentFile$country <- "X"
  currentFile$dayofYear <- (i + 119)
  #Append the current file
  temp_dfX = rbind(temp_dfX, currentFile)    
}

NAmat <- is.na(temp_dfX)
sum(NAmat)

setwd("~/GitHub/Biocomputing_RProject/Rproject2022/Rproject2022")
#writing the appended file  

write.csv(temp_dfX,"DataX.csv",row.names = F,quote = F)


### COUNTRY Y ###

setwd("~/GitHub/Biocomputing_RProject/Rproject2022/Rproject2022/countryY")

screen_files <- list.files("~/GitHub/Biocomputing_RProject/Rproject2022/Rproject2022/countryY",pattern = "\\.txt$")

#temporary data frame to load the contents on the current file
temp_dfY <- data.frame()

setwd("~/GitHub/Biocomputing_RProject/Rproject2022/Rproject2022/countryY")
#reading each file within the range and append them to create one file
for (i in 1:length(screen_files)){
  #read the file
  currentFile = read.table(screen_files[i],header = TRUE)
  # Add columns for country and dayofYear
  currentFile$country <- "Y"
  currentFile$dayofYear <- (i + 119)
  #Append the current file
  temp_dfY = rbind(temp_dfY, currentFile)    
}

NAmat <- is.na(temp_dfX)
sum(NAmat)

setwd("~/GitHub/Biocomputing_RProject/Rproject2022/Rproject2022")
#writing the appended file  

write.csv(temp_dfY,"DataY.csv",row.names = F,quote = F)

### Concatenate X and Y Data ###
allData <- rbind(temp_dfX,temp_dfY)
allDataProvided <- read.csv("allData.csv")

# remove later on, just a logical dataframe to check if they are the same
allData == allDataProvided


sudo rm -r .git

#Converting all files to comma-separated values
# Each name and number is not in each cell in excel....the output is the list of 
# all the information in one cell
setwd("~/Desktop/Biocomputing_RProject-main/Rproject2022/countryY/")
filelist = list.files(pattern = ".txt")
for (i in 1:length(filelist)){
  input <- filelist[i]
  output <- paste0(gsub("\\.txt$", "", input), ".csv")
  print(paste("Processing the file: ", input))
  data = read.delim(input, header = TRUE)   
  setwd("~/Desktop/Biocomputing_RProject-main/Rproject2022/new_countryY/")
  write.table(data, file=output, sep="," , col.names = TRUE, row.names=FALSE)
  setwd("~/Desktop/Biocomputing_RProject-main/Rproject2022/countryY/")
}

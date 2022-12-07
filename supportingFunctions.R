#First Function
Converting_txt_to_csv <- function(){
setwd("~/Desktop/Biocomputing_RProject-main/Rproject2022/countryY/")
dir.create("~/Desktop/Biocomputing_RProject-main/Rproject2022/new_countryY")
filelist = list.files(pattern = ".txt")
for (i in 1:length(filelist)){
  input <- filelist[i]
  output <- paste0(gsub("\\.txt$", "", input), ".csv")
  print(paste("Processing the file: ", input))
  data = read.table(input, header = TRUE)   
  setwd("~/Desktop/Biocomputing_RProject-main/Rproject2022/new_countryY/")
  write.csv(data, file=output, sep="," , col.names = TRUE, row.names=FALSE)
  setwd("~/Desktop/Biocomputing_RProject-main/Rproject2022/countryY/")
}
}

# Gretchen notes:
# I think we should use if statements to make our function applicable to all
# file types.
# Something like...
library(tools) # This loads a library with the function below
?file_ext # This function can pull a file type just from a path
# Now I set it to my working directory (just to test it)
setwd("~/Documents/Courses/Biocomputing_BIOS_60318/Tutorials/Biocomputing_RProject/Rproject2022/countryX")
# And use the function to get the file type of screen_120.csv!
file_ext("~/Documents/Courses/Biocomputing_BIOS_60318/Tutorials/Biocomputing_RProject/Rproject2022/countryX/screen120.csv")
# The function returns "csv"

# So maybe we could do something like..
for (file in filelist){
	if (file_ext(file) = "csv"){
		# blah blah blah
	}else if (file_ext(file) = "txt"){
		# blah blah blah
	}
}

# Something like this? Carol

for (i in 1:length(files.to.read)) {
  y_files <- (read.table(files.to.read[i], header = TRUE, fill = TRUE))
  write.csv(y_files, file = files.to.write[i])
}

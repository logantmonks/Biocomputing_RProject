
# FUNCTION 1: names function "txt2csv" that converts .txt files to .csv 

# "wd" argument provides a working directory for files to be converted in.
# Leaving "wd" blank will use the current working directory.

# "deltxt" argument, if set to TRUE, will delete .txt files after conversion.
# If left blank, or set to FALSE, will not delete files.

txt2csv <- function(wd = NULL, deltxt = NULL) {
# Sets given working directory
   if (!is.null(wd)) {
  setwd(wd)
   }
# Finds all .txt files in directory, writes list of filenames
  list_txt <- list.files(pattern = ".+\\.txt")
# Steps through list of file names, writing an object "file_name" that replaces
#   the .txt extension with .csv
  for (i in 1:length(list_txt)) {
    file_name <- gsub(pattern = "\\.txt", replacement = ".csv", x = list_txt[i])
# Tables from .txt files read to overwriting "data" object, also includes col names
    data <- read.table(file = list_txt[i], header = TRUE)
# Writes csv from "data" object, using "file_name" constructed above and
#   excludes row/col names
    write.csv(x = data, file = file_name, row.names = FALSE)
# If deltxt = TRUE, deletes .txt files from directory
      if (isTRUE(deltxt)) {
        unlink(list_txt[i])
      }
    else{}
  }
}

# FUNCTION 2: 


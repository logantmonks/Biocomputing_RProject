# convert .txt files of country Y to .csv
# make this a function -> 
#txt_to_csv = function(){
  setwd("~/biocomputing/Biocomputing_RProject/Rproject2022/countryY/")
  filelist = list.files(pattern = "*.txt")
  for (file in filelist) {
    data = read.table(file, header=TRUE, sep = "", stringsAsFactors = FALSE)
    output=gsub("txt","csv", file)
    write.table(data, file, sep = ',')
  }
#}

# create full dataset
# make this a function -> 
#compile_files <- function(){
  fulldata <- data.frame(matrix(ncol = 15, nrow = 0))
  colnames(fulldata) = c('gender','age','marker01','marker02','marker03','marker04','marker05','marker06','marker07','marker08','marker09','marker10','country','dayofYear','infected')

  # add all country Y data to full dataset
  filelist = list.files(pattern = "*.csv")
  for (file in filelist) {
    data = read.csv(file)
    day = as.numeric(substr(file, 8, 10))
    data$country = 'Y'
    data$dayofYear = day
    data$infected = FALSE
    fulldata = rbind(fulldata, data)
  }

  # add all country X data to full dataset
  setwd("~/biocomputing/Biocomputing_RProject/Rproject2022/countryX/")
  filelist = list.files(pattern = "*.csv")
  for (file in filelist) {
    data = read.csv(file)
    day = as.numeric(substr(file, 8, 10))
    data$country = 'X'
    data$dayofYear = day
    data$infected = FALSE
    fulldata = rbind(fulldata, data)
  }

  # Ask user for preference for handling NAs
  print("Remove NAs: (1) Warn for NAs: (2) Include NAs: (3)")
  # warnings = readline()
  # warnings = as.integer(warnings)
  warnings = 3
  # Handle NAs
  if (warnings == 1) {
    fulldata = na.omit(fulldata)
  } else if (warnings == 2) {
    print("Number of rows with NA values:")
    print(nrow(fulldata)-nrow(na.omit(fulldata)))
  } # else if warnings == 3 -> instead, just letting anything else mean that they 
  # don't care about NAs to prevent errors
#}


# summarize statistics function
#summary <- function(){
  # Number of screenings
  countryX <- subset(fulldata, country == "X")
  print(paste("Number of screens performed by Country X: ", length(unique(countryX$dayofYear))))
  countryY <- subset(fulldata, country == "Y")
  print(paste("Number of screens performed by Country Y: ", length(unique(countryY$dayofYear))))

  # percent of patients infected:
  num_infected = 0
  total_patients = 0
  for (i in 1:nrow(fulldata)) {
    row <- fulldata[i,]
    for (val in row[3:12]) {
      # if any marker is 1, person was infected. break immediately to prevent
      # over-recording cases due to multiple markers = to 1
      if (val == 1){
        num_infected = num_infected + 1
        fulldata[i,15] = TRUE
        break
      }
    }
    total_patients = total_patients + 1
  }
  print(paste("Total patients screened: ", total_patients))
  print(paste("Total patients infected: ", num_infected))
  print(paste("Percent infected: ", num_infected/total_patients*100))


  # male vs female patients
  infected_males = sum(fulldata[which(fulldata$gender=='male'), 15])
  total_males = sum(fulldata$gender=='male')
  infected_females = sum(fulldata[which(fulldata$gender=='male'), 15])
  total_females = sum(fulldata$gender=='female')
  
  gender_data <- data.frame(
    condition = c("infected", "infected", "uninfected", "uninfected"),
    gender = c("male", "female", "male", "female"),
    count = c(infected_males, infected_females, total_males-infected_males, total_females-infected_females)
  )
  
  ggplot(gender_data, aes(fill=gender, y=count, x=condition)) + 
    geom_bar(position="dodge", stat="identity") +
    ggtitle("Male vs Female") +
    theme_classic()
  
  # display the age distribution of the infected vs uninfected
  ggplot(fulldata, aes(x=age, color = infected)) +
    geom_density() +
    xlim(0,100) +
    ggtitle("Patient Age Distribution") +
    theme_classic()
  
  #display the infection rates of the two countries.
  ggplot() +
    geom_density(data=subset(fulldata, infected==1), aes(x=dayofYear, color = country)) +
    ggtitle("Infection Rates") +
    theme_classic()
  # as can be seen by the graph produced above, it appears that the infection began
  # in country X- there are positive cases starting at day 120, where as cases
  # don't start to appear in country Y until around day 140
  
  markers <- c(rep("01" , 2), rep("02" , 2), rep("03" , 2), rep("04" , 2), rep("05" , 2), rep("06" , 2), rep("07" , 2), rep("08" , 2), rep("09" , 2), rep("10" , 2))
  country <- rep(c("X" , "Y"), 10)
  counts <- list()
  
  for(j in 3:12) {
    x_val = 0
    y_val = 0
    for(i in 1:nrow(fulldata)) {    
      if (fulldata[i,13]=='X') {
        x_val = x_val + fulldata[i,j]
      }
      else if (fulldata[i,13]=='Y') {
        y_val = y_val + fulldata[i,j]
      }
    }
    counts = append(counts, x_val)
    counts = append(counts, y_val)
  }
  bar_data <- data.frame(markers=markers,country=country,counts=0)
  for(i in 1:nrow(bar_data)) {
    bar_data[i,3] = counts[i]
  }
  
  # plot marker data
  ggplot(bar_data, aes(fill=country, y=counts, x=markers)) + 
    geom_bar(position="dodge", stat="identity")
  # it appears unlikely that a vaccine developed in country Y would work in
  # country X because of the variation of the marker triggering in each country.
  # country X is almost entirely based on markers 1-5, while Y is 6-10.
  
  #}
  
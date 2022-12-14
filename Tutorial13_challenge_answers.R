#### Answers to Tutorial 13 challenges


# Lecture Challenge 2 - function to make a file with a single random number on each line, but numbers have to sum to less than 100

# define the function with the name assignment and it will take a filename as an argument when used
assignment<-function(filename){
  howMany<-sample(x=1:50,size=1) # randomly pick about how many lines in file; can be 1 to 50 numbers
  # randomly pick howMany numbers from a normal distribution with the mean = 100/howMany
  # we did the mean as 100/howMany because if we have 25 numbers in order for the sum to be close
  # to 100 the numbers have to on average be 100/25=4
  numbers<-rnorm(n=howMany,mean=100/howMany,sd=1) 
  
  # check if sum is less than 100 and if not fix it
  sumNumbers<-sum(numbers) # calculate sum of numbers
  if(sumNumbers<100){
    # the matrix() call converts the numbers vector to a howMany x 1 matrix so that numbers are saved one
    # per row in the output file
    write.csv(matrix(data=numbers,nrow=length(numbers)),filename,row.names=FALSE,col.names=FALSE)
  }else{
    # if the sum is more than 100...
    diff<-sum(numbers)-100 # figure out how far the sum is above 100
    # one way to fix the sum issue would be to remove numbers until we are below 100
    # we can do this by calculating cumulative sums and comparing this to diff (how much we are over 100)
    # this should tell us how many numbers we need to remove from the beginning of the numbers vector to get below 100
    cumulativeSumNumbers<-cumsum(numbers) # calculate cumulative sum of numbers
    removeUpTo<-min(which(cumulativeSumNumbers>diff)) # what is the first cumulative sum greater than the amount we are above 100?
    
    numbers<-numbers[-(1:removeUpTo)] # remove numbers from the beginning of the vector to get the sum below 100
    
    write.csv(matrix(data=numbers,nrow=length(numbers)),filename,row.names=FALSE,col.names=FALSE)
  }
}




# Extra Challenge 1 -  Function that returns rows from wages.csv

subsetWages<-function(gender,yearsEducation,yearsExperience){
  # load data -> could have additional argument with path to wages.csv
  # this assumes the file is in the current working directory
  data<-read.table("wages.csv",header=TRUE,stringsAsFactors=FALSE,sep=",")
  
  #subset to rows desired
  sub<-data[data$gender==gender & data$yearsSchool==yearsEducation & data$yearsExperience==yearsExperience,]
  
  # if no matches, sub will have zero rows
  if(nrow(sub)==0){
    print("I'm sorry there is no data in wages.csv that matches your criteria.")
  }else{
    return(sub)
  }
}

# Extra Challenge 2 - Converting between miles and km
# a default wasn't required, but seems like a reasonable thing to do
distConvert<-function(x,direction="miles2km"){
  if(direction=="miles2km"){
    out<-x*1.60934
    return(out)
  }else if(direction=="km2miles"){
    out<-x/1.60934
    return(out)
  # adding an informative error message in case the user doesn't know what to use for the direction argument 
  }else{
    print("Please use miles2km or km2miles as the direction argument to specify which conversion you desire.")
  }
}
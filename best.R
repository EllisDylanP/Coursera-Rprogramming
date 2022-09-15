## this function should take the state abbreviation and the medical condition and return the hospital with the 
## best 30-day mortality rate. The data is being pulled from outcome_of_care_measures.csv



best <- function(state, disease){
  data <- read.csv('C:/USers/dylan/Downloads/rprog_data_ProgAssignment3-data//outcome-of-care-measures.csv')
  list1 <- split(data, data$State)
  statetolook <- list1[state]
  diseases <- c("heart attack","heart failure", "pneumonia")
  listofstates<- c(levels(data$State))
 
  ## address invalid inputs
  
  if(!(state %in% listofstates)){
    
    stop('Invalid state')
  }
  else if(!(disease %in% diseases)){
    
    stop('Invalid disease')
  }
  
  ##proceed with valid inputs
  
  if(disease == "heart attack"){
    datatocompare <- statetolook[[state]][["Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack"]]
    interval <- as.character(datatocompare)
    intervaltwo <- as.numeric(interval)
    lowestdeath <- min(intervaltwo, na.rm = TRUE)
    
    indexoflowestdeath <- which(intervaltwo == lowestdeath)
    Hospdatatocompare <- statetolook[[state]][["Hospital.Name"]]
    besthospitalinstate <- Hospdatatocompare[indexoflowestdeath]
  }
  
  if(disease == "heart failure"){
    datatocompare <- statetolook[[state]][["Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure"]]
    interval <- as.character(datatocompare)
    intervaltwo <- as.numeric(interval)
    lowestdeath <- min(intervaltwo, na.rm = TRUE)
    
    indexoflowestdeath <- which(intervaltwo == lowestdeath)
    Hospdatatocompare <- statetolook[[state]][["Hospital.Name"]]
    besthospitalinstate <- Hospdatatocompare[indexoflowestdeath]
  }
  if(disease == "pneumonia"){
    datatocompare <- statetolook[[state]][["Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia"]]
    interval <- as.character(datatocompare)
    intervaltwo <- as.numeric(interval)
    lowestdeath <- min(intervaltwo, na.rm = TRUE)
    
    indexoflowestdeath <- which(intervaltwo == lowestdeath)
    Hospdatatocompare <- statetolook[[state]][["Hospital.Name"]]
    besthospitalinstate <- Hospdatatocompare[indexoflowestdeath]
  }
  
  alphabetical <- sort(besthospitalinstate, na.rm = TRUE)
  alphabetical[1]
}

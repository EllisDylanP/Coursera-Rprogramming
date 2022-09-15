## this function should take the state abbreviation, the medical condition
## and return the hospital with the 
## 30-day mortality rate corresponding to the ranking number or term provided. 
## The data is being pulled from outcome_of_care_measures.csv



rankhospital <- function(state, disease, ranking){
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
    deathorder <- sort(intervaltwo, decreasing = FALSE)
    
    Hospdatatocompare <- statetolook[[state]][["Hospital.Name"]]
    
    Hospdata <- data.frame(Hospdatatocompare,intervaltwo)
    vitaltable <- Hospdata[order(Hospdata$intervaltwo, Hospdata$Hospdatatocompare),]
    rankofstate <- c(1:as.numeric(as.character(length(datatocompare))))
    vitaltable$rankofstate <- rankofstate
    
    if(class(ranking) == "numeric"){
      besthospitalinstate <- vitaltable[ranking == vitaltable$rankofstate,]
    }else if(ranking == "best"){
      deathrateofrankedHosp <- min(deathorder)
      indexofdeath <- as.numeric(as.character(which(vitaltable$intervaltwo == deathrateofrankedHosp)))
      besthospitalinstate <- vitaltable[indexofdeath,]
      
    }else if(ranking == "worst"){
      deathrateofrankedHosp <- max(deathorder)
      indexofdeath <- as.numeric(as.character(which(vitaltable$intervaltwo == deathrateofrankedHosp)))
      besthospitalinstate <- vitaltable[indexofdeath,]
    }
  }
  
  if(disease == "heart failure"){
    datatocompare <- statetolook[[state]][["Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure"]]
    interval <- as.character(datatocompare)
    intervaltwo <- as.numeric(interval)
    deathorder <- sort(intervaltwo, decreasing = FALSE)
    
    Hospdatatocompare <- statetolook[[state]][["Hospital.Name"]]
    
    Hospdata <- data.frame(Hospdatatocompare,intervaltwo)
    vitaltable <- Hospdata[order(Hospdata$intervaltwo, Hospdata$Hospdatatocompare),]
    rankofstate <- c(1:as.numeric(as.character(length(datatocompare))))
    vitaltable$rankofstate <- rankofstate
    
    if(class(ranking) == "numeric"){
      besthospitalinstate <- vitaltable[ranking == vitaltable$rankofstate,]
    }else if(ranking == "best"){
      deathrateofrankedHosp <- min(deathorder)
      indexofdeath <- as.numeric(as.character(which(vitaltable$intervaltwo == deathrateofrankedHosp)))
      besthospitalinstate <- vitaltable[indexofdeath,]
      
    }else if(ranking == "worst"){
      deathrateofrankedHosp <- max(deathorder)
      indexofdeath <- as.numeric(as.character(which(vitaltable$intervaltwo == deathrateofrankedHosp)))
      besthospitalinstate <- vitaltable[indexofdeath,]
    }
  }
  
  if(disease == "pneumonia"){
    datatocompare <- statetolook[[state]][["Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia"]]
    interval <- as.character(datatocompare)
    intervaltwo <- as.numeric(interval)
    deathorder <- sort(intervaltwo, decreasing = FALSE)
    
    Hospdatatocompare <- statetolook[[state]][["Hospital.Name"]]
    
    Hospdata <- data.frame(Hospdatatocompare,intervaltwo)
    vitaltable <- Hospdata[order(Hospdata$intervaltwo, Hospdata$Hospdatatocompare),]
    rankofstate <- c(1:as.numeric(as.character(length(datatocompare))))
    vitaltable$rankofstate <- rankofstate
    
    if(class(ranking) == "numeric"){
      besthospitalinstate <- vitaltable[ranking == vitaltable$rankofstate,]
    }else if(ranking == "best"){
      deathrateofrankedHosp <- min(deathorder)
      indexofdeath <- as.numeric(as.character(which(vitaltable$intervaltwo == deathrateofrankedHosp)))
      besthospitalinstate <- vitaltable[indexofdeath,]
      
    }else if(ranking == "worst"){
      deathrateofrankedHosp <- max(deathorder)
      indexofdeath <- as.numeric(as.character(which(vitaltable$intervaltwo == deathrateofrankedHosp)))
      besthospitalinstate <- vitaltable[indexofdeath,]
    }
  }
  ##vitaltable
  besthospitalinstate
}

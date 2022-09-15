## this function should take the state abbreviation, the medical condition
## and return the hospital with the 
## 30-day mortality rate corresponding to the ranking number or term provided. 
## The data is being pulled from outcome_of_care_measures.csv



rankall <- function(disease, ranking = 1){
  data <- read.csv('C:/USers/dylan/Downloads/rprog_data_ProgAssignment3-data//outcome-of-care-measures.csv')
  list1 <- split(data, data$State)
  listofstates<- c(levels(data$State))
  empty <- character()
  listofhospitals <- character()
  
  diseases <- c("heart attack","heart failure", "pneumonia")
  if(!(disease %in% diseases)){
    
    stop('Invalid disease')
  }
  
  ##proceed with valid inputs
  
  for(x in levels(data$State)){
    statetolook <- list1[x]
    
    if(disease == "heart attack"){
      ##ordering disease death rate data
      datatocompare <- statetolook[[x]][["Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack"]]
      interval <- as.character(datatocompare)
      intervaltwo <- as.numeric(interval)
      deathorder <- sort(intervaltwo, decreasing = FALSE, na.last = TRUE)
      Hospdatatocompare <- statetolook[[x]][["Hospital.Name"]]
      Hospdata <- data.frame(Hospdatatocompare,intervaltwo)
      vitaltable <- Hospdata[order(Hospdata$intervaltwo, Hospdata$Hospdatatocompare),]
      rankofstate <- c(1:as.numeric(as.character(length(datatocompare))))
      vitaltable$rankofstate <- rankofstate
      
      if(class(ranking) == "numeric"){
        if(ranking < as.numeric(as.character(length(intervaltwo)))){
          Hospital <- as.character(vitaltable$Hospdatatocompare[ranking])
        }else{
          rank <- ranking
          Hospital <- as.character(vitaltable$Hospdatatocompare[rank])
        }
        
      }else if(ranking == "best"){
        
        Hospital <- as.character(vitaltable$Hospdatatocompare[1])
      }else if(ranking == "worst"){
        deathrateofrankedHosp <- max(deathorder)
        numb <- which(vitaltable$intervaltwo == deathrateofrankedHosp)
        number <- max(numb)
        Hospital <- as.character(vitaltable$Hospdatatocompare[number])
      }
      
      
      ##Hospitals of the state in order by death rate
      listofhospitals <- c(listofhospitals, Hospital)
    }
    
    if(disease == "heart failure"){
      datatocompare <- statetolook[[x]][["Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure"]]
      interval <- as.character(datatocompare)
      intervaltwo <- as.numeric(interval)
      deathorder <- sort(intervaltwo, decreasing = FALSE, na.last = TRUE)
      Hospdatatocompare <- statetolook[[x]][["Hospital.Name"]]
      Hospdata <- data.frame(Hospdatatocompare,intervaltwo)
      vitaltable <- Hospdata[order(Hospdata$intervaltwo, Hospdata$Hospdatatocompare),]
      rankofstate <- c(1:as.numeric(as.character(length(datatocompare))))
      vitaltable$rankofstate <- rankofstate
      
      if(class(ranking) == "numeric"){
        if(ranking < as.numeric(as.character(length(intervaltwo)))){
          Hospital <- as.character(vitaltable$Hospdatatocompare[ranking])
        }else{
          rank <- ranking
          Hospital <- as.character(vitaltable$Hospdatatocompare[rank])
        }
        
      }else if(ranking == "best"){
        
        Hospital <- as.character(vitaltable$Hospdatatocompare[1])
      }else if(ranking == "worst"){
        deathrateofrankedHosp <- max(deathorder)
        numb <- which(vitaltable$intervaltwo == deathrateofrankedHosp)
        number <- max(numb)
        Hospital <- as.character(vitaltable$Hospdatatocompare[number])
      }
      
      
      ##Hospitals of the state in order by death rate
      listofhospitals <- c(listofhospitals, Hospital)
    }
    
    if(disease == "pneumonia"){
      datatocompare <- statetolook[[x]][["Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia"]]
      interval <- as.character(datatocompare)
      intervaltwo <- as.numeric(interval)
      deathorder <- sort(intervaltwo, decreasing = FALSE, na.last = TRUE)
      Hospdatatocompare <- statetolook[[x]][["Hospital.Name"]]
      Hospdata <- data.frame(Hospdatatocompare,intervaltwo)
      vitaltable <- Hospdata[order(Hospdata$intervaltwo, Hospdata$Hospdatatocompare),]
      rankofstate <- c(1:as.numeric(as.character(length(datatocompare))))
      vitaltable$rankofstate <- rankofstate
      
      if(class(ranking) == "numeric"){
        if(ranking < as.numeric(as.character(length(intervaltwo)))){
          Hospital <- as.character(vitaltable$Hospdatatocompare[ranking])
        }else{
          rank <- ranking
          Hospital <- as.character(vitaltable$Hospdatatocompare[rank])
        }
      }else if(ranking == "best"){
        
        Hospital <- as.character(vitaltable$Hospdatatocompare[1])
      }else if(ranking == "worst"){
        deathrateofrankedHosp <- max(deathorder)
        numb <- which(vitaltable$intervaltwo == deathrateofrankedHosp)
        number <- max(as.numeric(as.character(numb)))
        Hospital <- as.character(vitaltable$Hospdatatocompare[number])
      }
      
      
      ##Hospitals of the state in order by death rate
      listofhospitals <- c(listofhospitals, Hospital)
    }
  }
  
  USAperstate <- data.frame(listofhospitals, listofstates)
  alphabetical <- USAperstate[order(USAperstate$listofstate),]
  alphabetical
}

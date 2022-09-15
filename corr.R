specdata <- "C:\\Users\\dylan\\Downloads\\rprog_data_specdata\\specdata"

complete <- function(specdata, ID = 1:332){
  listFiles <- list.files(specdata, pattern = ".csv", full.names = TRUE)
  completepairs <- numeric()
  
  for(i in ID){
    data <- read.csv(listFiles[i])
    completepairs <- c(completepairs, sum(complete.cases(data)))
  }
  data.frame(ID, completepairs)
}

#newest attempt
corr <- function(specdata, threshold = 0){
     listFiles <- list.files(specdata, pattern = ".csv", full.names = TRUE)
     result <- numeric()
     tableforthres <- complete(specdata, 1:332)
     for(x in tableforthres$completepairs){
         if(x > threshold){
             specificID <- x
             specificIDrow <- match(specificID, tableforthres$completepairs)
             for(row in specificIDrow){
               data1 <- read.csv(listFiles[as.numeric(as.character(specificIDrow))])
               colsforcor <- data.frame(data1$sulfate, data1$nitrate)
               cortable <- colsforcor[order(colsforcor$data1.sulfate),]
               correlation <- cor(colsforcor$data1.sulfate, colsforcor$data1.nitrate, use = "complete.obs")
               result <- c(result, correlation)
             }
         }
     }
     result
   }

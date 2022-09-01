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
  

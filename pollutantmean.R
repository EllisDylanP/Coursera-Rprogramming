specdata <- "C:\\Users\\dylan\\Downloads\\rprog_data_specdata\\specdata"



polluntant <- function(specdata, x, ID = 1:332, removeNA = TRUE){
  nf <- list.files(specdata, pattern = ".csv", full.names = TRUE)
  data <- numeric()
  for(i in ID){
    read <- read.csv(nf[i])
    data <- c(data, read[[x]])
  }
  mean(data, na.rm = TRUE)
}
  

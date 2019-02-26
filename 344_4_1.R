#Stephen Zimmer
#AerE 344
#Section 6 Group 3
#Lab 4

setwd("E:/Stephen/Documents/R/College Files/Aer E 344 Lab 04")
#Sets working directory, the place where the files are stored.
#Above wd is for desktop.

rm(list = ls()) #Clear the global environment

#Plan: make a list of the csv files, extract each file into an object (variable), remove the list of files, then
#produce the necessary calculations.

#The data naming scheme will be: "data[fan speed in Hz]", so 0Hz data will be in data0Hz.

#### Functions, packages, and conversions ####
"%e%" <- function(a,b) {a*10^(b)}
#A function made by me to make it easier to enter big and small numbers.

#### Data Collection ####
data_files <- list.files(path = paste0(getwd(), "/Data"))

for(i in seq_along(data_files)) {
  temp_data <- read.csv(paste0(getwd(), "/Data/", data_files[i]), stringsAsFactors = FALSE)
  #Creates a temporary object to store the current data.
  assign(paste0("data", gsub("([[:alnum:]]+)[[:punct:]][[:alpha:]]{3}", "\\1", data_files[i])),
         temp_data[,-1], envir = .GlobalEnv)
  #temp_data[,-1] removes the frame column that is merely a row counter.
  rm(list = ls(pattern = "^temp_"))
}

rm(data_files, i)

#### Known and Measured Values ####
diam <- 0.7 #Diameter of the cylinder in inches
K <- 1.1 #Calibration constant

#### Data Processing ####
#Remove the outliers, which should be very extreme values. To find them, I will attempt removing all values
#that are 3 times more or less than the standard deviation.
dataNames <- ls(pattern = "^data")
#A vector of the data names. The data_files vector served a different purpose to this.

stddev <- matrix(data = NA, nrow = 8, ncol = 32, dimnames = list(dataNames, colnames(get(dataNames[1]))))
#Makes an empty matrix 
for(i in seq_along(dataNames)) {
  stddev[i,] <- sapply(X = seq_along(colnames(get(dataNames[i]))), FUN = function(X) sd(get(dataNames[i])[,X]))
} #Fills previous matrix

rm(i)

#To remove the columns exceeding 3x the standard deviation, I need to make a boolean vector of all columns in all sets.
#To make the vectors, I can try sapply(X = ..., FUN = function(X) dataNHz[,X] > stddev[dataNHz,X])

datafilter <- array(data = NA, dim = c(nrow(data0Hz), ncol(data0Hz), length(dataNames)))
#Makes an empty array of the same size as the data, with as many matrices as there is data.
for(i in seq_along(dataNames)) {
  datafilter[,,i] <- sapply(X = seq_along(colnames(get(dataNames[i]))), FUN = function(X)
    {get(dataNames[i])[,X] > 3*stddev[i,X]}) #Check if the data is within desired range.
}
#An array is a three-dimensional matrix. It essentially stores one or more matrices.
#If I want row 1, column 2 of matrix 3, then I call it with array[1,2,3].
rm(i)

#Calculation to-do: average all data from all runs, plot pressure coefficient on the cylinder,
#calculate drag coefficients, plot drag coefficients as a function of Reynolds number

for(i in seq_along(dataNames)) {
  temp_data <- get(dataNames[i])
  temp_data[datafilter[,,i]] <- NA
  #Where datafilter is TRUE, the element is replaced with NA.
  assign(paste0("p", dataNames[i]), temp_data, envir = .GlobalEnv)
  #The processed data will be saved as "pdataNHz"
  rm(list = ls(pattern = "^temp_"))
}
rm(i)

save(list = ls(pattern = "^data[0-9]+Hz$"), file = "Storage/unprocessedData.RData")
#Saving the original data containing outliers to an RData file.
rm(list = ls(pattern = "^data[0-9]+Hz$"))
#Removing the unprocessed data from the environment for simplicity.
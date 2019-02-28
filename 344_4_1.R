#Stephen Zimmer
#AerE 344
#Section 6 Group 3
#Lab 4

#setwd("E:/Stephen/Documents/R/College Files/Aer E 344 Lab 04")
#Sets working directory, the place where the files are stored.
#Above wd is for desktop.

setwd("D:/Sam/Documents/AerE 344/AerE344_Lab4")

rm(list = ls()) #Clear the global environment

#Plan: make a list of the csv files, extract each file into an object (variable), remove the list of files, then
#produce the necessary calculations.

#The data naming scheme will be: "data[fan speed in Hz]", so 0Hz data will be in data0Hz.

#### Functions, packages, and conversions ####
"%e%" <- function(a,b) {a*10^(b)}
#A function made by me to make it easier to enter big and small numbers.
library(ggplot2)
d2r <- pi/180 #degrees to radians

airspd <- function(input) {
  #This function takes the name of the data or a string and looks for the motor speed (in Hz).
  #If a STRING is input, then it searches the string for the motor speed.
  #If a numeric is input, then it uses the numbers for motor speed.
  if(any(is.null(input))) stop("Input is missing!")
  if(any(is.na(input))) stop("Input is missing!")
  #Simple error catching.
  
  output <- single(length(input))
  if(is.character(input)) {
    #Grabs the section of string(s) that contains a number before "Hz" (case insensitive).
    input <- gsub("[A-Za-z]+([0-9]+)Hz", "\\1", input, ignore.case = TRUE)
    #print(input)
    input <- as.numeric(input)
  }
  
  output <- 0.98158*input - 3.4962
  output[output < 0] <- 0
  #The fan should not run in reverse and the equation comes from experimentation.
  return(output)
}

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
bsln <- matrix(data = NA, nrow = 8, ncol = 32, dimnames = list(dataNames, colnames(get(dataNames[1]))))
#Makes an empty matrix
#stddev is the standard deviation and bsln (baseline) is the average
for(i in seq_along(dataNames)) {
  stddev[i,] <- sapply(X = seq_along(colnames(get(dataNames[i]))), FUN = function(X) sd(get(dataNames[i])[,X]))
  bsln[i,] <- sapply(X = seq_along(colnames(get(dataNames[i]))), FUN = function(X) mean(get(dataNames[i])[,X]))
} #Fills previous matrix

rm(i)

#To remove the columns exceeding 3x the standard deviation, I need to make a boolean vector of all columns in all sets.
#To make the vectors, I can try sapply(X = ..., FUN = function(X) dataNHz[,X] > stddev[dataNHz,X])

datafilter <- array(data = NA, dim = c(nrow(data0Hz), ncol(data0Hz), length(dataNames)))
#Makes an empty array of the same size as the data, with as many matrices as there is data.
while(FALSE) {
for(i in seq_along(dataNames)) {
  datafilter[,,i] <- sapply(X = seq_along(colnames(get(dataNames[i]))), FUN = function(X)
    {get(dataNames[i])[,X] > bsln[i,X]+(3*stddev[i,X]) || get(dataNames[i])[,X] < bsln[i,X]-(3*stddev[i,X])})
  #Check if the data is within desired range.
}
rm(i)
} #Cutting off this portion of code as it could be producing undesired results.
#An array is a three-dimensional matrix. It essentially stores one or more matrices.
#If I want row 1, column 2 of matrix 3, then I call it with array[1,2,3].
for(i in seq_along(dataNames)) {
  datafilter[,,i] <- sapply(seq_along(colnames(get(dataNames[i]))), function(X) get(dataNames[i])[,X] == -999999)
}

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
rm(list = ls(pattern = "^data"))
#Removing the unprocessed data from the environment for simplicity.

deltaA <- as.double(20 * d2r) #The difference in angles from adjacent ports is 20 degrees, converted to radians.
#Double-precision floating-point may be necessary.
pdataNames <- ls(pattern = "^pdata")

avg <- matrix(data = NA, nrow = 8, ncol = 32, dimnames = list(pdataNames, colnames(get(pdataNames[1]))))
#Makes an empty matrix
#stddev is the standard deviation and bsln (baseline) is the average
for(i in seq_along(pdataNames)) {
  avg[i,] <- sapply(X = seq_along(colnames(get(pdataNames[i]))), FUN = function(X) mean(get(pdataNames[i])[,X]))
} #Fills previous matrix
rm(i)

zeroLvl <- avg[1,] #The zero pressure levels for all sensors.
#avg <- apply(avg, 1, '-', zeroLvl)
#avg <- sweep(avg, 1, zeroLvl, '-')
for(i in seq_len(nrow(avg))) {
  avg[i,] <- avg[i,] - zeroLvl
}
rm(i)
cavg <- colMeans(avg) #Average of all data of all runs.
write.csv(cavg, file = "Storage/all_data_averaged.csv") #average of all data over all runs

#Column 19 is A pressure, column 20 is E pressure
#Plot pressure coefficient on the cylinder
pcoef <- matrix(data = NA, nrow = nrow(avg), ncol = ncol(avg))
for(i in seq_len(nrow(avg))) {
  for(j in seq_len(18)) {
    pcoef[i,j] <- (avg[i,j] - avg[i,20]) / (K * (avg[i,19] - avg[i,20]))
  }
}
rm(i,j)

ppcoef <- colMeans(pcoef[2:8,1:18])
#ggplot(data = data.frame(Run_Length = c("1", "2", "10"), Standard_Deviation = stddev),
#       aes(x = Run_Length, y = Standard_Deviation, fill = Standard_Deviation)) +
#  geom_bar(stat = "identity")
ggplot(data = data.frame(P_Coefficient = ppcoef, Angle = seq(-pi, pi, length.out = 18)),
       aes(y = P_Coefficient, x = Angle)) +
  geom_point() +
  xlab("Angle (radians)") +
  ylab("Pressure Coefficent (unitless)")

ggplot(data = data.frame(Pco = pcoef[3,1:18], Angle = seq(-pi, pi, length.out = 18)),
       aes(y = Pco, x = Angle)) +
  geom_point() +
  xlab("Angle (radians)") +
  ylab("Pressure Coefficent (Unitless)")

#Calculate drag coefficients and plot drag coefficients as a function of Reynolds number -> Cd(R)
#FIRST, calculate the velocity of the fluid in m/s using the results from lab 2.
velo <- airspd(pdataNames) #wind velocity in m/s
p_ambient <- 992 %e% 2 #Pascals
T_ambient <- 273.15 + 21.5 #Temperature in Kelvin
R <- 287 #J/(kg*K)
rho <- p_ambient / (R * T_ambient) #Ambient air density in kg/m^3
rm(p_ambient, T_ambient, R)

#I should make a function for calculating rho when T and P are known.
#Need the drag coefficients from all runs, then compare each to the Reynolds number from said experiment.
Dcoef <- numeric(nrow(pcoef)-1) #Numeric vector of length (# of runs) - 1 (to exclude the 0Hz run).
for(i in seq_len(nrow(pcoef)-1)) {
  fi <- pcoef[i+1,1:18] * cos(seq(0, 2*pi, length.out = 18))
  Dcoef[i] <- -(deltaA/2)*sum(fi)
}
rm(i,fi)

write.csv(Dcoef, file = "Storage/Drag_Coefficients.csv")

#Finally, plot drag coefficients as a function of Reynolds number.
Reyn <- (rho * velo[-1] * (0.7*2.54 %e% -2)) / (1.79 %e% -5)

ggplot(data.frame(DragCoef = Dcoef, Reynolds = Reyn), aes(x = Reynolds, y = DragCoef)) +
  geom_point() +
  xlab("Reynolds Number (unitless)") +
  ylab("Drag Coefficient (unitless)")

#HEY!! TRY REPLACING THE COEFFICIENT OF PRESSURE EQUATION WITH THE OTHER ONE!!
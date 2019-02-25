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
         temp_data, envir = .GlobalEnv)
  rm(list = ls(pattern = "^temp_"))
}
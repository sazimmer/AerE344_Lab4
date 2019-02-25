#Stephen Zimmer
#AerE 344
#Section 6 Group 3
#Lab 4

setwd("E:/Stephen/Documents/R/College Files/Aer E 344 Lab 04")
#Sets working directory, the place where the files are stored.
#Above wd is for desktop.

rm(list = ls()) #Clear the global environment

#### Functions, packages, and conversions ####
"%e%" <- function(a,b) {a*10^(b)}
#A function made by me to make it easier to enter big and small numbers.

#### Data Collection ####
data_files <- list.files(path = paste0(getwd(), "/Data"))
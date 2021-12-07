# Install and load the required libraries ----------------------------------------
pacman::p_load(reshape2, ggplot2, dplyr, stringr, corrplot)

urlfile<-'https://raw.githubusercontent.com/JanaMey/CACI_Assignment2/main/indivData.csv'
indivData <-read.csv(urlfile)

str(indivData)
head(indivData)
dim(indivData) # 266 462

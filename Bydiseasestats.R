library(readr)
library(tidyr)
library(dplyr)
library(ggplot2)

#Reading in the data and cleaning it up
diseases.raw <- read.csv("MandatedDiseasesFinal.csv")

diseases.df <- subset(diseases.raw, !is.na(Disease) & Disease != "")

diseases.sep <- diseases.df %>%
  separate_rows(Disease, sep = ",")

#Smallpox subset
pox.df <- diseases.sep %>%
  filter(Disease== "Smallpox")

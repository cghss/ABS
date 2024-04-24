library(readr)
library(tidyverse)
library(dplyr)
library(ggplot2)

biod.raw <- read.csv("BiodiversityByCountryGBO1.csv")
gdp.raw <- read.csv("GDPLast4Years.csv")


df <- right_join(biod.raw, gdp.raw, by = c("iso_a3_eh" = "Code"))

ggplot(df, aes(x = X2019, y = NBI, label = iso_a3_eh, color = WHO_Region)) +
  geom_point() +      
  geom_text(vjust = -0.5) +   
  labs(x = "GDP in 2019", y = "Nagoya Biodiversity Index", color = "WHO Region") +  
  ggtitle("Dot Plot of GDP in 2019 vs Nagoya Biodiversity Index") + 
  coord_cartesian(xlim = c(0, 83000))

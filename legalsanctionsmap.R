library(readr)
library(dplyr)
library(countrycode)
library(rnaturalearth)
library(ggplot2)


abs.raw <- read.csv("AbsMaster.csv")

abs.raw$ISO <- countrycode(sourcevar = abs.raw$Country, origin = "country.name", destination = "iso3c")

enforce.df <- abs.raw %>%
  filter(grepl("Legal sanctions", Subtopic, ignore.case = TRUE)) %>%
  data.frame()

access.df <- abs.raw %>%
  filter(grepl("Access to resources", Subtopic, ignore.case = TRUE)) %>%
  data.frame()


map <- ne_countries(type = 'countries')

enforce.data <- inner_join(enforce.df, map, by = c("ISO" = "iso_a3_eh"))
access.data <- inner_join(access.df, map, by = c("ISO" = "iso_a3_eh"))

enforce.map <- ggplot() +
  #scale_fill_gradientn(colours = rev(viridis(45, option = "G")), na.value = "white", breaks = seq(0, max(df$n), by = 5))+
  theme_minimal() +
  scale_fill_manual(values =c("#B598DC","#005E98","#98D9DC", "#DEDBDB", "#A73B00", "blue", "black")) + 
  ggtitle("Legal Sanctions") +
  theme(
    plot.title = element_text(hjust = 0.5),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(), 
    legend.position = "right",  
    legend.title = element_blank())
print(enforce.map)

access.map <- ggplot() +
  geom_sf(data = access.data, aes(fill = Status, geometry = geometry),color = "black", size = 0.2) +
  #scale_fill_gradientn(colours = rev(viridis(45, option = "G")), na.value = "white", breaks = seq(0, max(df$n), by = 5))+
  theme_minimal() +
  scale_fill_manual(values =c("#B598DC","#005E98","#DEDBDB","#98D9DC", "#DEDBDB", "#A73B00", "blue", "black")) + 
  ggtitle("Access to resources") +
  theme(
    plot.title = element_text(hjust = 0.5),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(), 
    legend.position = "right",  
    legend.title = element_blank())
print(access.map)

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

scope.df <- abs.raw %>%
  filter(grepl("Scope of legislation", Subtopic, ignore.case = TRUE)) %>%
  data.frame()

dsi.df <- abs.raw %>%
  filter(grepl("Digital sequence information (DSI)", Subtopic, ignore.case = TRUE)) %>%
  data.frame()

pic.df <- abs.raw %>%
  filter(grepl("Prior informed consent (PIC)", Subtopic, ignore.case = TRUE)) %>%
  data.frame()

terms.df <- abs.raw %>%
  filter(grepl("Contractual terms", Subtopic, ignore.case = TRUE)) %>%
  data.frame()

ben.df<- abs.raw %>%
  filter(grepl("Benefit-sharing", Subtopic, ignore.case = TRUE)) %>%
  data.frame()

com.df<- abs.raw %>%
  filter(grepl("Compliance", Subtopic, ignore.case = TRUE)) %>%
  data.frame()

map <- ne_countries(type = 'countries')

enforce.data <- inner_join(enforce.df, map, by = c("ISO" = "iso_a3_eh"))
access.data <- inner_join(access.df, map, by = c("ISO" = "iso_a3_eh"))
scope.data <- inner_join(scope.df, map, by = c("ISO" = "iso_a3_eh"))
dsi.data <- inner_join(dsi.df, map, by = c("ISO" = "iso_a3_eh"))
terms.data <- inner_join(terms.df, map, by = c("ISO" = "iso_a3_eh"))
ben.data <- inner_join(ben.df, map, by = c("ISO" = "iso_a3_eh"))
com.data <- inner_join(com.df, map, by = c("ISO" = "iso_a3_eh"))

enforce.map <- ggplot() +
  geom_sf(data = enforce.data, aes(fill = Status, geometry = geometry),color = "black", size = 0.2) +
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


scope.map <- ggplot() +
  geom_sf(data = scope.data, aes(fill = Status, geometry = geometry),color = "black", size = 0.2) +
  #scale_fill_gradientn(colours = rev(viridis(45, option = "G")), na.value = "white", breaks = seq(0, max(df$n), by = 5))+
  theme_minimal() +
  scale_fill_manual(values =c("#B598DC","#005E98","#98D9DC", "#DEDBDB", "#DEDBDB", "#A73B00", "blue", "black")) + 
  ggtitle("Scope of legislation") +
  theme(
    plot.title = element_text(hjust = 0.5),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(), 
    legend.position = "right",  
    legend.title = element_blank())
print(scope.map)

dsi.map <- ggplot() +
  geom_sf(data = dsi.data, aes(fill = Status, geometry = geometry),color = "black", size = 0.2) +
  #scale_fill_gradientn(colours = rev(viridis(45, option = "G")), na.value = "white", breaks = seq(0, max(df$n), by = 5))+
  theme_minimal() +
  scale_fill_manual(values =c("#B598DC","#005E98","#98D9DC", "#DEDBDB", "#DEDBDB", "#A73B00", "blue", "black")) + 
  ggtitle("DSI") +
  theme(
    plot.title = element_text(hjust = 0.5),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(), 
    legend.position = "right",  
    legend.title = element_blank())
print(dsi.map)

terms.map <- ggplot() +
  geom_sf(data = terms.data, aes(fill = Status, geometry = geometry),color = "black", size = 0.2) +
  #scale_fill_gradientn(colours = rev(viridis(45, option = "G")), na.value = "white", breaks = seq(0, max(df$n), by = 5))+
  theme_minimal() +
  scale_fill_manual(values =c("#B598DC","#005E98","#98D9DC", "#DEDBDB", "#DEDBDB", "#A73B00", "blue", "black")) + 
  ggtitle("Contractual Terms") +
  theme(
    plot.title = element_text(hjust = 0.5),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(), 
    legend.position = "right",  
    legend.title = element_blank())
print(terms.map)

ben.map <- ggplot() +
  geom_sf(data = ben.data, aes(fill = Status, geometry = geometry),color = "black", size = 0.2) +
  #scale_fill_gradientn(colours = rev(viridis(45, option = "G")), na.value = "white", breaks = seq(0, max(df$n), by = 5))+
  theme_minimal() +
  scale_fill_manual(values =c("#B598DC","#005E98","#98D9DC", "#DEDBDB", "#DEDBDB", "#A73B00", "blue", "black")) + 
  ggtitle("Benefits-sharing") +
  theme(
    plot.title = element_text(hjust = 0.5),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(), 
    legend.position = "right",  
    legend.title = element_blank())
print(ben.map)

com.map <- ggplot() +
  geom_sf(data = com.data, aes(fill = Status, geometry = geometry),color = "black", size = 0.2) +
  #scale_fill_gradientn(colours = rev(viridis(45, option = "G")), na.value = "white", breaks = seq(0, max(df$n), by = 5))+
  theme_minimal() +
  scale_fill_manual(values =c("#B598DC","#DEDBDB","#005E98", "#DEDBDB", "#DEDBDB", "#A73B00", "blue", "black")) + 
  ggtitle("Compliance") +
  theme(
    plot.title = element_text(hjust = 0.5),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(), 
    legend.position = "right",  
    legend.title = element_blank())
print(com.map)

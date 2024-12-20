library(readr)
library(dplyr)
library(countrycode)
library(rnaturalearth)
library(ggplot2)
library(patchwork)

#First we are going to make all of our bar charts
#Loading our data
abs.raw <- read.csv("data/AbsMaster.csv")
who.raw <- read.csv("data/WHO_Regions.csv")

#Add iso codes to abd.raw
abs.raw$ISO <- countrycode(sourcevar = abs.raw$Country, origin = "country.name", destination = "iso3c")

#ugh we need to add back in three of these countries

lost.countries <- data.frame(Country.Name = c("Cote d'Ivoire", "Liechtenstein", "Niue"), 
                             ISO3 = c("CIV", "LIE", "NIU"), 
                             WHO_Region = c("AFRO", "EURO", "WPRO"))

who.raw <- rbind(who.raw, lost.countries)

#Add our who regions to the abs.raw df
merged_data <- left_join(abs.raw, who.raw, by = c("ISO" = "ISO3"))

#Double check to ensure that we are capturing only the 194 UN member states
unmatched.iso <- abs.raw$ISO[is.na(merged_data$WHO_Region)]

if (length(unmatched.iso) > 0) {
  print("Warning: Some ISO codes were not matched")
  print("Unmatched ISO codes:")
  print(unmatched.iso)
} else {
  print("All ISO codes were matched successfully.")
}


#The entities that are unmatched beyond the scope of the project as they are not one of the 194 UN member states. They were removed. 
abs.data <- merged_data %>% filter(!is.na(WHO_Region))

#Okay, so this^ is our cleaned df

#Now we need to separate out by subtopic so that we can understand how many member states in each region have each type of policy
unique.subtopics <- unique(abs.data$Subtopic)

for (subtopic in unique.subtopics) {
  
  subtopic.df <- abs.data %>% filter(Subtopic == subtopic)
  
  #name for df = (first letter of subtopic + ".df")
  df.name <- paste0(substr(subtopic, 1, 1), ".df")
  assign(df.name, subtopic.df)
}
#niceeeee

calculate_percentage <- function(df, who_region, status) {
  count_total_region <- nrow(filter(df, WHO_Region == who_region))
  count_matched <- nrow(filter(df, WHO_Region == who_region, Status == status))
  percentage <- (count_matched / count_total_region) * 100
  return(percentage)
}

# Iterate over the newly created dataframes
for (subtopic in unique.subtopics) {
  # Extract dataframe for the current subtopic
  subtopic.df <- get(paste0(substr(subtopic, 1, 1), ".df"))
  
  # Create an empty dataframe for statistics
  stats.df <- data.frame(Subtopic = character(),
                         WHO_Region = character(),
                         Status = character(),
                         Percentage = numeric(),
                         stringsAsFactors = FALSE)
  
  # Iterate over WHO regions
  for (region in unique(subtopic.df$WHO_Region)) {
    # Iterate over Status entries
    for (status in unique(subtopic.df$Status)) {
      # Calculate percentage
      percentage <- calculate_percentage(subtopic.df, region, status)
      
      # Add to statistics dataframe
      new.row <- data.frame(Subtopic = subtopic,
                            WHO_Region = region,
                            Status = status,
                            Percentage = percentage,
                            stringsAsFactors = FALSE)
      stats.df <- rbind(stats.df, new.row)
    }
  }
  
  # Assign the dataframe to the corresponding name
  stats_df_name <- paste0(substr(subtopic, 1, 1), ".df.stats")
  assign(stats_df_name, stats.df)
}

#okay gotta make these regions into factors so they will place nice with ggplot
L.df.stats$WHO_Region <- factor(L.df.stats$WHO_Region)

#make a bar chart
Fig2A <- ggplot(L.df.stats, aes(x = WHO_Region, y = Percentage, fill = Status)) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values =c("#B598DC","#005E98","#98D9DC", "#DEDBDB", "#A73B00", "blue", "black")) + 
  labs(x = "WHO Region", y = "Percentage", fill = "Status") +
  ggtitle("Legal Sanctions") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        plot.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(), 
        axis.ticks.y = element_blank())

ggplot(A.df.stats, aes(x = WHO_Region, y = Percentage, fill = Status)) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values =c("#B598DC","#005E98", "#DEDBDB", "#98D9DC", "#A73B00", "blue", "black")) + 
  labs(x = "WHO Region", y = "Percentage", fill = "Status") +
  ggtitle("Access") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        plot.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(), 
        axis.ticks.y = element_blank())

Fig1A <- ggplot(B.df.stats, aes(x = WHO_Region, y = Percentage, fill = Status)) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values =c("#B598DC","#005E98", "#98D9DC", "#DEDBDB")) + 
  labs(x = "WHO Region", y = "Percentage", fill = "Status") +
  ggtitle("Benefit-sharing") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        plot.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(), 
        axis.ticks.y = element_blank())

ggplot(C.df.stats, aes(x = WHO_Region, y = Percentage, fill = Status)) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values =c("#B598DC","#DEDBDB", "#005E98", "#98D9DC", "#A73B00", "blue", "black")) + 
  labs(x = "WHO Region", y = "Percentage", fill = "Status") +
  ggtitle("Compliance") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        plot.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(), 
        axis.ticks.y = element_blank())


dsi.bar <- ggplot(D.df.stats, aes(x = WHO_Region, y = Percentage, fill = Status)) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values =c("#B598DC", "#005E98", "#98D9DC", "#A73B00", "blue", "black")) + 
  labs(x = "WHO Region", y = "Percentage", fill = "Status") +
  ggtitle("DSI") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        plot.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(), 
        axis.ticks.y = element_blank())
print(dsi.bar)

ggplot(P.df.stats, aes(x = WHO_Region, y = Percentage, fill = Status)) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values =c("#B598DC", "#005E98", "#98D9DC", "#DEDBDB", "#A73B00", "blue", "black")) + 
  labs(x = "WHO Region", y = "Percentage", fill = "Status") +
  ggtitle("PIC") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        plot.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(), 
        axis.ticks.y = element_blank())

ggplot(S.df.stats, aes(x = WHO_Region, y = Percentage, fill = Status)) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values =c("#B598DC", "#005E98", "#98D9DC", "#DEDBDB", "#A73B00", "blue", "black")) + 
  labs(x = "WHO Region", y = "Percentage", fill = "Status") +
  ggtitle("Scope of Legislation") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        plot.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(), 
        axis.ticks.y = element_blank())

#Now we are going to make our maps
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
  filter(grepl("DSI", Subtopic, ignore.case = TRUE)) %>%
  data.frame()

pic.df <- abs.raw %>%
  filter(grepl("PIC", Subtopic, ignore.case = TRUE)) %>%
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

Fig2B <- ggplot() +
  geom_sf(data = enforce.data, aes(fill = Status, geometry = geometry),color = "black", size = 0.2) +
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
print(Fig2B)

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
#print(dsi.map)

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
#print(terms.map)

Fig1B <- ggplot() +
  geom_sf(data = ben.data, aes(fill = Status, geometry = geometry),color = "black", size = 0.2) +
  theme_minimal() +
  scale_fill_manual(values =c("#B598DC","#005E98","#98D9DC", "#DEDBDB")) + 
  ggtitle("Benefits-sharing") +
  theme(
    plot.title = element_text(hjust = 0.5),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(), 
    legend.position = "right",  
    legend.title = element_blank())
#print(Fig1B)

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
#print(com.map)

#Time to create the combined figures, just reloading these for ease

Fig1A <- ggplot(B.df.stats, aes(x = WHO_Region, y = Percentage, fill = Status)) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values = c("#B598DC", "#005E98", "#98D9DC", "#A73B00", "#DEDBDB")) + 
  labs(x = "WHO Region", y = "Percentage", fill = "Status") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        plot.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(), 
        axis.ticks.y = element_blank(), 
        legend.position = "right",  
        legend.title = element_blank())

Fig1B <- ggplot() +
  geom_sf(data = ben.data, aes(fill = Status, geometry = geometry), color = "black", size = 0.2, show.legend = FALSE) +
  geom_sf(data = ben.data, aes(fill = Status, geometry = geometry), color = NA) +
  scale_fill_manual(values = c("#B598DC", "#005E98", "#98D9DC", "#A73B00", "#DEDBDB")) + 
  labs(fill = "Status") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(), 
        panel.border = element_blank(), 
        legend.position = "right",  
        legend.title = element_blank())

#Figure 1
Figure1 <- Fig1A + Fig1B + plot_layout(guides = "collect") & theme(legend.position = "bottom")
print(Figure1)

#Now make figure 2
Fig2A <- ggplot(L.df.stats, aes(x = WHO_Region, y = Percentage, fill = Status)) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values =c("#B598DC","#005E98","#98D9DC", "#DEDBDB", "#A73B00", "blue", "black")) + 
  labs(x = "WHO Region", y = "Percentage", fill = "Status") +
  #ggtitle("Legal Sanctions") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        plot.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(), 
        axis.ticks.y = element_blank(), 
        legend.position = "right",  
        legend.title = element_blank())
  
Fig2B <- ggplot() +
  geom_sf(data = enforce.data, aes(fill = Status, geometry = geometry), color = "black", size = 0.2, show.legend = FALSE) +
  geom_sf(data = enforce.data, aes(fill = Status, geometry = geometry), color = NA) +
  scale_fill_manual(values = c("#B598DC", "#005E98", "#98D9DC", "#DEDBDB", "#A73B00")) + 
  labs(fill = "Status") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(), 
        panel.border = element_blank(), 
        legend.position = "right",  
        legend.title = element_blank())

#Figure 2
Figure2 <- Fig2A + Fig2B + plot_layout(guides = "collect") &
  theme(legend.position = "bottom")
print(Figure2)

Fig3A <- ggplot(D.df.stats, aes(x = WHO_Region, y = Percentage, fill = Status)) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values =c("#B598DC","#005E98","#98D9DC", "#DEDBDB", "#A73B00", "blue", "black")) + 
  labs(x = "WHO Region", y = "Percentage", fill = "Status") +
  #ggtitle("DSI") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        plot.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(), 
        axis.ticks.y = element_blank(), 
        legend.position = "right",  
        legend.title = element_blank())

Fig3B <- ggplot() +
  geom_sf(data = dsi.data, aes(fill = Status, geometry = geometry), color = "black", size = 0.2, show.legend = FALSE) +
  geom_sf(data = dsi.data, aes(fill = Status, geometry = geometry), color = NA) +
  scale_fill_manual(values = c("#B598DC", "#005E98", "#98D9DC", "#DEDBDB", "#A73B00")) + 
  labs(fill = "Status") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(), 
        panel.border = element_blank(), 
        legend.position = "right",  
        legend.title = element_blank())
Figure3 <- Fig3A + Fig3B + plot_layout(guides = "collect") &
  theme(legend.position = "bottom")
print(Figure3)


Figure1 <- (Fig1A | Fig1B) + plot_layout(guides = "collect") & theme(legend.position = "bottom")
Figure2 <- (Fig2A | Fig2B) + plot_layout(guides = "collect") & theme(legend.position = "bottom")
Figure3 <- (Fig3A | Fig3B) + plot_layout(guides = "collect") & theme(legend.position = "bottom")


# Combine the three figures vertically with equal proportions
CombinedFigure <- (Figure1 / Figure2 / Figure3) + 
  plot_layout(ncol = 1, heights = c(1, 1, 1))

# Print the combined figure
print(CombinedFigure)

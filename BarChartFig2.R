library(readr)
library(dplyr)
library(countrycode)
library(rnaturalearth)
library(ggplot2)

#Loading our data
abs.raw <- read.csv("AbsMaster.csv")
who.raw <- read.csv("WHO_Regions.csv")

#Add iso codes to abd.raw
abs.raw$ISO <- countrycode(sourcevar = abs.raw$Country, origin = "country.name", destination = "iso3c")

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


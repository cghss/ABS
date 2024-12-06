library(readr)
library(dplyr)
library(stringr)
library(countrycode)
library(rnaturalearth)
library(ggplot2)
library(patchwork)


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

#Now I am going to start working on the heat map
# I need to pull out the subtopics
subtopics <- unique(abs.data$Subtopic)

# now I'm making a function to pull apart abs.data into the different subtopics
create_df <- function(subtopic) {
  df <- abs.data %>%
    filter(Subtopic == subtopic)
  # just assigning names here
  first_word <- str_split(subtopic, " ")[[1]][1]
  assign(first_word, df, envir = .GlobalEnv)
}
lapply(subtopics, create_df)

Benefit <- abs.data %>%
  filter(Subtopic == "Benefit-sharing")

#What countries have each type of policy?
Scope <- Scope %>%
  mutate(Policy_Status = case_when(
    Status %in% c("Legislation covers genetic resources (including microorganisms) and traditional knowledge",
                  "Legislation covers genetic resources (including microorganisms)") ~ "Policy",
    Status %in% c("No ABS legislation identified", 
                "Legislation does not cover microorganisms")~ "No policy",
    TRUE ~ NA_character_  # This handles any other cases
  ))

Compliance <- Compliance %>%
  mutate(Policy_Status = case_when(
    Status %in% c("Compliance mechanisms in place in ABS legislation",
                  "No compliance mechanisms in place in ABS legislation") ~ "Policy",
    Status == "No ABS legislation identified" ~ "No policy",
    TRUE ~ NA_character_  # This handles any other cases
  ))

Contractual <- Contractual %>%
  mutate(Policy_Status = case_when(
    Status %in% c("Access is unrestricted",
                  "Access is restricted and contract is required",
                  "Access is restricted but contract requirements not addressed") ~ "Policy",
    Status == "No ABS legislation identified" ~ "No policy",
    TRUE ~ NA_character_  # This handles any other cases
  ))

Legal <- Legal %>%
  mutate(Policy_Status = case_when(
    Status %in% c("Fines, criminalization, and access restrictions",
                  "Fines and criminalization",
                  "Access restrictions",
                  "No penalties specified") ~ "Policy",
    Status == "No ABS legislation identified" ~ "No policy",
    TRUE ~ NA_character_  # This handles any other cases
  ))

Benefit <- Benefit %>%
  mutate(Policy_Status = case_when(
    Status %in% c("Access is unrestricted",
                  "Access is restricted and benefit-sharing is mandated",
                  "Access is restricted but benefit-sharing is not mandated") ~ "Policy",
    Status == "No ABS legislation identified" ~ "No policy",
    TRUE ~ NA_character_  # This handles any other cases
  ))

Prior <- PIC %>%
  mutate(Policy_Status = case_when(
    Status %in% c("Access is unrestricted",
                  "Access is restricted and PIC is required",
                  "Access is restricted but PIC is not addressed") ~ "Policy",
    Status == "No ABS legislation identified" ~ "No policy",
    TRUE ~ NA_character_  # This handles any other cases
  ))

Access <- Access %>%
  mutate(Policy_Status = case_when(
    Status %in% c("Access is unrestricted",
                  "Access is restricted") ~ "Policy",
    Status == "No ABS legislation identified" ~ "No policy",
    TRUE ~ NA_character_  # This handles any other cases
  ))

Digital <- DSI %>%
  mutate(Policy_Status = case_when(
    Status %in% c("DSI is included",
                  "DSI is explicitly excluded",
                  "DSI potentially included") ~ "Policy",
    Status == "DSI is not addressed" ~ "No policy",
    TRUE ~ NA_character_  # This handles any other cases
  ))

#Okay so now I want to go through and count the number of countries that have each type of policy in each WHO region
scoresults <- Scope %>%
  group_by(WHO_Region) %>%
  summarise(
    Total_Countries = n_distinct(Country.Name),
    Y = sum(Policy_Status == "Policy", na.rm = TRUE),
    N = sum(Policy_Status == "No policy", na.rm = TRUE),
    PercentagePol = (Y / Total_Countries) * 100
  )

benresult <- Benefit %>%
  group_by(WHO_Region) %>%
  summarise(
    Total_Countries = n_distinct(Country.Name),
    Y = sum(Policy_Status == "Policy", na.rm = TRUE),
    N = sum(Policy_Status == "No policy", na.rm = TRUE),
    PercentagePol = (Y / Total_Countries) * 100
  )

comresults <- Compliance %>%
  group_by(WHO_Region) %>%
  summarise(
    Total_Countries = n_distinct(Country.Name),
    Y = sum(Policy_Status == "Policy", na.rm = TRUE),
    N = sum(Policy_Status == "No policy", na.rm = TRUE),
    PercentagePol = (Y / Total_Countries) * 100
  )

conresults <- Contractual %>%
  group_by(WHO_Region) %>%
  summarise(
    Total_Countries = n_distinct(Country.Name),
    Y = sum(Policy_Status == "Policy", na.rm = TRUE),
    N = sum(Policy_Status == "No policy", na.rm = TRUE),
    PercentagePol = (Y / Total_Countries) * 100
  )

leresults <- Legal %>%
  group_by(WHO_Region) %>%
  summarise(
    Total_Countries = n_distinct(Country.Name),
    Y = sum(Policy_Status == "Policy", na.rm = TRUE),
    N = sum(Policy_Status == "No policy", na.rm = TRUE),
    PercentagePol = (Y / Total_Countries) * 100
  )

priresults <- Prior  %>%
  group_by(WHO_Region) %>%
  summarise(
    Total_Countries = n_distinct(Country.Name),
    Y = sum(Policy_Status == "Policy", na.rm = TRUE),
    N = sum(Policy_Status == "No policy", na.rm = TRUE),
    PercentagePol = (Y / Total_Countries) * 100
  )

accresults <- Access %>%
  group_by(WHO_Region) %>%
  summarise(
    Total_Countries = n_distinct(Country.Name),
    Y = sum(Policy_Status == "Policy", na.rm = TRUE),
    N = sum(Policy_Status == "No policy", na.rm = TRUE),
    PercentagePol = (Y / Total_Countries) * 100
  )

digresults <- Digital %>%
  group_by(WHO_Region) %>%
  summarise(
    Total_Countries = n_distinct(Country.Name),
    Y = sum(Policy_Status == "Policy", na.rm = TRUE),
    N = sum(Policy_Status == "No policy", na.rm = TRUE),
    PercentagePol = (Y / Total_Countries) * 100
  )

#Gotta subset so that I only have percentages and regions
scoresults_subset <- scoresults[, c("WHO_Region", "PercentagePol")]
benresult_subset <- benresult[, c("WHO_Region", "PercentagePol")]
comresults_subset <- comresults[, c("WHO_Region", "PercentagePol")]
conresults_subset <- conresults[, c("WHO_Region", "PercentagePol")]
leresults_subset <- leresults[, c("WHO_Region", "PercentagePol")]
priresults_subset <- priresults[, c("WHO_Region", "PercentagePol")]
accresults_subset <- accresults[, c("WHO_Region", "PercentagePol")]
digresults_subset <- digresults[, c("WHO_Region", "PercentagePol")]

#merge merge merge!
merged_df <- Reduce(function(x, y) merge(x, y, by = "WHO_Region", all = TRUE), list(scoresults_subset, benresult_subset, comresults_subset, conresults_subset, leresults_subset, priresults_subset, accresults_subset, digresults_subset))

# keep myself organized
names(merged_df)[grep("PercentagePol", names(merged_df))] <- c("PercentagePol_scoresults", "PercentagePol_benresult", "PercentagePol_comresults", "PercentagePol_conresults", "PercentagePol_leresults", "PercentagePol_priresults", "PercentagePol_accresults", "PercentagePol_digresults")

# Sort
merged_df <- merged_df[order(merged_df$WHO_Region), ]

#Reshape so that she plays nice with ggplot :)
long_merged_df <- tidyr::pivot_longer(merged_df, cols = starts_with("PercentagePol"), names_to = "Result_Category", values_to = "PercentagePol")

#I am just reordering the x axis to make it easier for a reader to understand with the other figures in the paper
desired_order <- c("PercentagePol_accresults","PercentagePol_comresults","PercentagePol_scoresults", "PercentagePol_benresult", "PercentagePol_digresults", "PercentagePol_priresults", "PercentagePol_conresults", "PercentagePol_leresults")

long_merged_df$Result_Category <- factor(long_merged_df$Result_Category, levels = desired_order)

#Make the x axis labels something intelligible
category_labels <- c(
  "PercentagePol_scoresults" = "Scope of Legislation",
  "PercentagePol_benresult" = "Benefit-Sharing",
  "PercentagePol_comresults" = "Compliance",
  "PercentagePol_conresults" = "Contractual Terms",
  "PercentagePol_leresults" = "Legal Sanctions",
  "PercentagePol_priresults" = "Prior Informed Consent",
  "PercentagePol_accresults" = "Access to Resources",
  "PercentagePol_digresults" = "DSI"
)

ggplot(long_merged_df, aes(x = Result_Category, y = WHO_Region, fill = PercentagePol)) +
  geom_tile(color = "white") +
  geom_text(aes(label = sprintf("%.1f%%", PercentagePol), 
            color = PercentagePol > 40), 
            size = 3, 
           show.legend = FALSE) + # Add text labels with conditional color
  scale_color_manual(values = c("TRUE" = "white", "FALSE" = "black")) +
  scale_fill_gradient(low = "#E8D4FC", high = "#220152", labels = scales::percent_format(scale = 1)) +
  labs(#title = "Percentage of Countries with ABS- and DSI- Relevant Policy by WHO Region",
       x = "Policy Category",
       y = "WHO Region",
       fill = "Percentage of Countries \n with Applicable Policy") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.title = element_text(size = 14, hjust = 0.5), # Center the legend title
        legend.key.height = unit(5, "lines"), # Increase legend key height
        legend.key.width = unit(2, "lines"), # Increase legend key width
        legend.justification = "center") + #
  scale_x_discrete(labels = category_labels)


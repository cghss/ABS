

library(dplyr)

abs.raw <- read.csv("AbsMaster.csv")

abs.raw$ISO <- countrycode(sourcevar = abs.raw$Country, origin = "country.name", destination = "iso3c")


access_restricted <- abs.raw %>%
  filter(Subtopic == "Access to resources" & Status == "Access is restricted")


dsi_status <- abs.raw %>%
  filter(Subtopic == "Digital sequence information (DSI)" & 
           (Status == "DSI is not addressed" | Status == "DSI potentially included"))


common_countries <- intersect(access_restricted$Country, dsi_status$Country)


num_common_countries <- length(unique(common_countries))


print(num_common_countries)

print(paste("Number of common countries:", num_common_countries))


print("Common countries:")
print(common_countries)

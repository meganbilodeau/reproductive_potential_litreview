##### Adding new species to the traits list for fecundity 

library(readxl)
library(dplyr)
library(xml2)
library(sf)
library(sf)
library(tidyr)

meantraits <- read.csv("R_code_litreview_sem12024/mean_traits.csv")

##### Add data below with name of the trait as either Colony fecundity, Polyp per area, Polyp fecundity
##### Add units preconverted to cm^2 for polyp fecundity and mm^2 for colony fecundity 

#Example
#new_row <- data.frame(
#  Species = c("Acropora hyacinthus","Acropora cytherea", "Acropor digitifera", "Acropora humilis", "Acropora nasuta", "Acropora spathulata", "Goniastrea pectinata", "Goniastrea retiformis"),
#  trait_name = c("Polyp fecundity","Polyp fecundity", "Polyp fecundity", "Polyp fecundity", "Polyp fecundity", "Polyp fecundity", "Polyp fecundity", "Polyp fecundity"),
#  standard_unit = c("units","units","units","units","units","units","units","units"),
#  Meanvalue = c(5.908, 1.023, 11.507, 14.999, 14.074, 9.496, 7.036, 4.966)
#)

new_row <- data.frame(
  Species = c(),
  trait_name = c(),
  standard_unit = c(),
  Meanvalue = c()
)

meantraits <- bind_rows(meantraits, new_row)

write.csv(meantraits, file = "R_code_litreview_sem12024/mean_traits.csv", row.names = TRUE)

coraldata <- read.csv("R_code_litreview_sem12024/coral_traits.csv")

#coral_merge <- merge(coraldata, meantraits, by = "Species")

### checking how many species there are

#new_df_base <- coral_merge[,c("Species", "MeanPCcover", "Range", "ReefArea_Range", "family_molecules", "larval", "sexual", "Lower.bound", "Mean", "Upper.bound", "trait_name","standard_unit" ,"Meanvalue")]

#write.csv(new_df_base, file = "R_code_litreview_sem12024//coralmerge.csv", row.names = TRUE)

###### Adding Coral Cover data to new_df_base #######

Indo_Pacific_data <- readRDS("R_code_litreview_sem12024/Indo_Pacific_data.rds")

coraldata$coralcover <- (coraldata$MeanPCcover * (sum(Indo_Pacific_data$area_km2)/0.7))

write.csv(coral_merge, "R_code_litreview_sem12024/coraldata_final.csv")


########## Calculating new fecundity #############

# Filter and select relevant data for polyp fecundity
polyp_fecundity_data <- coral_merge %>%
  filter(trait_name == "Polyp fecundity") %>%
  dplyr::select(Species, polyp_fecundity = Meanvalue)

# Filter and select relevant data for polyps per area
polyps_per_area_data <- coral_merge %>%
  filter(trait_name == "Polyps per area") %>%
  dplyr::select(Species, polyps_per_area = Meanvalue)

# Join the data by species
combined_data <- polyp_fecundity_data %>%
  inner_join(polyps_per_area_data, by = "Species")

# Calculate fecundity
combined_data <- combined_data %>%
  mutate(fecundity = polyp_fecundity * polyps_per_area)

# Convert polyp fecundity from cm² to km²
combined_data <- combined_data %>%
  mutate(fecundity = fecundity * 1e10)

# Select relevant columns and rename fecundity
polyp_combined_data <- combined_data %>%
  dplyr::select(Species, fecundity)

# Filter and select relevant data for colony fecundity
colony_fecundity_data <- coral_merge %>%
  filter(trait_name == "Colony fecundity") %>%
  dplyr::select(Species, fecundity = Meanvalue)

# Convert colony fecundity from mm² to km²
colony_fecundity_data <- colony_fecundity_data %>%
  mutate(fecundity = fecundity * 1e12)

# Identify species that have both colony and polyp fecundity
species_with_colony_fecundity <- colony_fecundity_data$Species

# Exclude polyp fecundity data for species that have colony fecundity
polyp_combined_data <- polyp_combined_data %>%
  filter(!Species %in% species_with_colony_fecundity)

# Combine the remaining polyp-based fecundity data with the colony fecundity data
final_combined_data <- bind_rows(polyp_combined_data, colony_fecundity_data)

# Display the result
print(final_combined_data)

#####################################

merge_fecund <- merge(coral_merge, final_combined_data, by = "Species")

#final_data <- merge_fecund[,c("Species", "MeanPCcover", "Range", "ReefArea_Range", "family_molecules", "larval", "sexual", "Lower.bound", "Mean", "Upper.bound", "coralcover", "fecundity")]

final_data <- distinct(merge_fecund)

write.csv(final_data, file = "R_code_litreview_sem12024/final_data.csv", row.names = TRUE)




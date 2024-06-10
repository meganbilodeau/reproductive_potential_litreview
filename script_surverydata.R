### data compile for determining the reproduction potential of the reef

### load data and create a data set containing the following:
### scientific name, mean number of colonies (in 100 million), average colony fecundity, reproduction type, sex

### load up initial data from Dietzel, Andreas, Bode, Michael, Connolly, Sean R., and Hughes, Terry P. (2021) The population sizes and global extinction risk of reef-building coral species at biogeographic scales. Nature Ecology & Evolution.
## grabbing data from Dietzel et al 2021 and creating dataset in separate folder
##load("R_code_litreview_sem12024/R code Dietzel et al/RData/Input_Files.RData")
##coraldata <- SupportData
##write.csv(coraldata, file = "coraldata.csv", row.names = TRUE)

library(dplyr)

## read in data
coral <- read.csv("R_code_litreview_sem12024/coraldata.csv")

install.packages("readxl")
library(readxl)

file_path <- "R_code_litreview_sem12024/coral_popsize.xlsx"

# Read the names of all sheets in the Excel file
sheet_names <- excel_sheets(file_path)
print(sheet_names)  # Print the names of the sheets

# Read each sheet into a list of data frames
data_list <- lapply(sheet_names, function(sheet) {
  read_excel(file_path, sheet = sheet)
})

# Optionally, name the list elements after the sheet names
names(data_list) <- sheet_names

# Access individual data frames by sheet name
species_abundance <- data_list[["Species abundances"]]
population_sizes <- data_list[["Population sizes"]]
habitat_maps <- data_list[["Reef habitat maps"]]

######### Get Fecundity Data #####

coraldata <- merge(coral, population_sizes, by = "Species")

write.csv(coraldata, file = "R_code_litreview_sem12024/coraldata_colnumber.csv", row.names = TRUE)

coraltraits <- read.csv("R_code_litreview_sem12024/ctdb_1.1.1_data.csv")

coraltraits <- coraltraits %>%
  rename(Species = specie_name)

coraltraits <- coraltraits %>%
  dplyr::select(-observation_id, -access, -user_id, -specie_id, -location_id, - latitude, -longitude, -measurement_id, -standard_id, -methodology_name, -methodology_id, -precision, -precision_type, -precision_upper, -replicates, -notes, -trait_class)

fecundity <- read.csv("R_code_litreview_sem12024/fecundity.csv")

colony_fecundity <- subset(fecundity, fecundity$trait_id == 216)

colony_fecundity <- colony_fecundity %>%
  rename(Species = specie_name)

colony_fecundity <- colony_fecundity %>%
  dplyr::select(-observation_id, -access, -user_id, -specie_id, -location_id, - latitude, -longitude, -measurement_id, -standard_id, -methodology_name, -methodology_id, -precision, -precision_type, -precision_upper, -replicates, -notes)


####### organizing data  ###########

###table(coraltraits$trait_name)

###polyp_fecundity <- subset(coraltraits, coraltraits$trait_id == 12)

###mesentary_fecundity <- subset(coraltraits, coraltraits$trait_id == 209)

###colony_area <- subset(coraltraits, coraltraits$trait_name == "Colony area")

###eggs_per_area <- subset(coraltraits, coraltraits$trait_name == "Eggs per area")

###polyps_per_area <- subset(coraltraits, coraltraits$trait_name == "Polyps per area")

###colony_surface_area <- subset(coraltraits, coraltraits$trait_name == "Colony surface area")

###range_size <- subset(coraltraits, coraltraits$trait_name == "Range size")


#######

#coraltrait <- subset(coraltraits, coraltraits$trait_id == 12 | coraltraits$trait_id == 209 | coraltraits$trait_id == 155 | coraltraits$trait_id == 211 | coraltraits$trait_id == 148 | coraltraits$trait_id == 231 | coraltraits$trait_id == 138)

#colony_fecundity$precision <- as.character(colony_fecundity$precision)
#colony_fecundity$replicates <- as.character(colony_fecundity$replicates)

# Combine the data frames using bind_rows
coral_trait <- bind_rows(coraltraits, colony_fecundity)

##mean value of traits

coral_traits <- merge(coral_trait, coraldata, by = "Species")

coral_traits <- coral_traits %>%
  dplyr::select(-X.2, -X.1, -X, -nominal.species)

coral_traits <- distinct(coral_traits)

coral_traits <- coral_traits %>%
  mutate(row_number = row_number())


############## Getting mean values of traits ################

# Separate numeric and string values
coral_traits$Numeric_Value <- as.numeric(as.character(coral_traits$value))
coral_traits$String_Value <- ifelse(is.na(coral_traits$Numeric_Value), as.character(coral_trait$value), NA)

write.csv(coral_trait, file = "R_code_litreview_sem12024/coral_traits.csv")

# Calculate mean for numeric values
mean_values <- coral_traits %>%
  filter(!is.na(Numeric_Value)) %>%
  group_by(Species, trait_name) %>%
  summarise(Mean_Value = mean(Numeric_Value))

# Retain string traits
#string_values <- coral_traits %>%
#  filter(!is.na(String_Value)) %>%
#  dplyr::select(Species, trait_name, String_Value)


new_row <- data.frame(
  Species = c("Acropora cytherea", "Acropor digitifera", "Acropora humilis", "Acropora nasuta", "Acropora spathulata", "Goniastrea pectinata", "Goniastrea retiformis"),
  trait_name = c("Polyp fecundity", "Polyp fecundity", "Polyp fecundity", "Polyp fecundity", "Polyp fecundity", "Polyp fecundity", "Polyp fecundity"),
  Mean_Value = c(1.023, 11.507, 14.999, 14.074, 9.496, 7.036, 4.966)
)

mean_values <- bind_rows(mean_values, new_row)

mean_values <- mean_values %>%
  mutate(Mean_Value = as.character(Mean_Value))

# Function to check if a value is numeric
is_numeric <- function(x) {
  !is.na(suppressWarnings(as.numeric(x)))
}

# Filter out rows with string values and select relevant columns
string_values <- coral_traits %>%
  filter(!is.na(value) & !is_numeric(value)) %>%
  dplyr::select(Species, trait_name, value) %>%
  rename(String_Value = value)

string_values <- distinct(string_values)


#Combine the results
combined_results <- bind_rows(
  mean_values %>% rename(value = Mean_Value),
  string_values %>% rename(value = String_Value)
)

coral_traits <- merge(coral_traits, combined_results, by = c("Species", "trait_name"), all = TRUE)

coral_traits <- coral_traits %>%
  dplyr::select(-Numeric_Value, -String_Value, -row_number)

##############################################################

### checking how many of each trait input there are
#group <- coral_trait %>% group_by(Species, trait_name)

### checking how many species there are


#write.csv(coral_merge, file = "R_code_litreview_sem12024//coralmerge.csv", row.names = TRUE)

#new_df_base <- coral_merge[, c("Species", "MeanPCcover", "Range", "ReefArea_Range", "family_molecules", "larval", "sexual", "Lower bound", "Mean", "Upper bound", "trait_name","standard_unit" ,"Meanvalue")]


#write.csv(new_df_base, file = "R_code_litreview_sem12024//coralmerge.csv", row.names = TRUE)



############# Shape File Extractions ###########

library(xml2)
library(sf)
library(dplyr)

# Read the shapefile
shapefile_path <- "R_code_litreview_sem12024/meow_ecos.shp"

# Read the shapefile
shapefile_data <- st_read(shapefile_path)

# Display the structure of the shapefile data
print(st_geometry_type(shapefile_data))
print(st_crs(shapefile_data))
print(head(shapefile_data))
################################

# Calculate the centroid of the dataset
centroid <- st_centroid(st_union(shapefile_data))

# Extract the longitude of the centroid
centroid_coords <- st_coordinates(centroid)
central_longitude <- centroid_coords[1, "X"]
central_longitude

# Function to determine UTM zone based on longitude
get_utm_zone <- function(longitude) {
  (floor((longitude + 180) / 6) %% 60) + 1
}

# Calculate the UTM zone for the central longitude
utm_zone <- get_utm_zone(central_longitude)
utm_zone

# Load necessary libraries
library(sf)
library(dplyr)

# Full file path to the shapefile
shapefile_path <- "/Users/megan/Desktop/R_code_litreview_sem12024/meow_ecos.shp"

# Set environment variable to attempt to restore missing .shx file if necessary
Sys.setenv(SHAPE_RESTORE_SHX = "YES")

# Function to read shapefile with error handling
read_shapefile <- function(path) {
  tryCatch({
    shp <- st_read(path)
    return(shp)
  }, error = function(e) {
    message("Error: ", e)
    return(NULL)
  })
}

# Attempt to read the shapefile
shapefile_data <- read_shapefile(shapefile_path)

# Check if the shapefile was read successfully
if (!is.null(shapefile_data)) {
  print("Shapefile loaded successfully")
  print(st_geometry_type(shapefile_data))
  print(st_crs(shapefile_data))
  print(head(shapefile_data))

  # Calculate the centroid of the dataset
  centroid <- st_centroid(st_union(shapefile_data))
  centroid_coords <- st_coordinates(centroid)
  central_longitude <- centroid_coords[1, "X"]
  central_longitude

  # Determine the UTM zone based on the central longitude
  get_utm_zone <- function(longitude) {
    (floor((longitude + 180) / 6) %% 60) + 1
  }
  utm_zone <- get_utm_zone(central_longitude)
  utm_zone

  # Determine the appropriate EPSG code for the UTM zone
  utm_crs <- if (centroid_coords[1, "Y"] >= 0) {
    # Northern Hemisphere
    32600 + utm_zone
  } else {
    # Southern Hemisphere
    32700 + utm_zone
  }
  utm_crs

  # Transform to the appropriate UTM CRS
  shapefile_data_proj <- st_transform(shapefile_data, crs = utm_crs)

  # Calculate the area for each ecoregion in square kilometers
  shapefile_data_proj <- shapefile_data_proj %>%
    mutate(area_km2 = as.numeric(st_area(.) / 10^6))  # Convert from m^2 to km^2

  # Select relevant columns and display the data
  ecoregion_area_data <- shapefile_data_proj %>%
    select(ECOREGION, PROVINCE, area_km2)

  # Display the ecoregion area data
  print(ecoregion_area_data)

  # Save the processed data to a CSV file
  write.csv(as.data.frame(ecoregion_area_data), "/Users/megan/Desktop/R_code_litreview_sem12024/ecoregion_area_data.csv")

  # Summarize the total area per province
  province_summary <- ecoregion_area_data %>%
    group_by(PROVINCE) %>%
    summarize(total_area_km2 = sum(area_km2, na.rm = TRUE))

  # Print the summary
  print(province_summary)

  # Plot the ecoregions
  library(ggplot2)
  ggplot(data = shapefile_data_proj) +
    geom_sf(aes(fill = PROVINCE)) +
    theme_minimal() +
    labs(title = "Marine Ecoregions of the World", fill = "Province")

} else {
  print("Failed to load shapefile. Please check the file path and ensure all necessary components (.shp, .shx, .dbf) are present.")
}


# Check for invalid geometries
invalid_geometries <- !st_is_valid(shapefile_data_proj)

# Print the number of invalid geometries
print(sum(invalid_geometries))

# Attempt to fix invalid geometries
shapefile_data_proj <- st_make_valid(shapefile_data_proj)

# Verify if the invalid geometries have been fixed
invalid_geometries <- !st_is_valid(shapefile_data_proj)
print(paste("Number of invalid geometries after fixing:", sum(invalid_geometries)))

# Recalculate the area for each ecoregion in square kilometers
shapefile_data_proj <- shapefile_data_proj %>%
  mutate(area_km2 = as.numeric(st_area(.) / 10^6))  # Convert from m^2 to km^2

# Select relevant columns and display the data
ecoregion_area_data <- shapefile_data_proj %>%
  dplyr::select(ECOREGION, REALM, area_km2)

# Display the ecoregion area data
print(ecoregion_area_data)

# Save the processed data to a CSV file
write.csv(as.data.frame(ecoregion_area_data), "/Users/megan/Desktop/R_code_litreview_sem12024/ecoregion_area_data.csv")

# Summarize the total area per province
province_summary <- ecoregion_area_data %>%
  group_by(REALM) %>%
  summarize(total_area_km2 = sum(area_km2, na.rm = TRUE))

# Print the summary
print(province_summary)

# Plot the ecoregions
ggplot(data = shapefile_data_proj) +
  geom_sf(aes(fill = PROVINCE)) +
  theme_minimal() +
  labs(title = "Marine Ecoregions of the World", fill = "Province")

ggplot(data = shapefile_data_proj) +
  geom_sf(aes(fill = REALM)) +
  theme_minimal() +
  labs(title = "Marine Ecoregions of the World", fill = "REALM")
############################
Indo_Pacific_data <- subset(shapefile_data_proj, REALM == "Central Indo-Pacific")

write.csv(as.data.frame(Indo_Pacific_data), "/Users/megan/Desktop/R_code_litreview_sem12024/indo_pacific_data.csv")

ggplot(data = Indo_Pacific_data) +
  geom_sf(aes(fill = PROVINCE)) +
  theme_minimal() +
  labs(title = "Marine Ecoregions of the World", fill = "Province")

# total survery area
total_area <- sum(Indo_Pacific_data$area_km2)
print(paste("Total survey area:", total_area, "km^2"))


#################################

Indo_Pacific_data <- subset(shapefile_data_proj, REALM == "Central Indo-Pacific" | REALM == "Eastern Indo-Pacific")

Indo_Pacific <- subset(Indo_Pacific_data, PROVINCE != "Hawaii")

write.csv(Indo_Pacific, file = "R_code_litreview_sem12024/Indo_pacific.csv")

saveRDS(Indo_Pacific, "R_code_litreview_sem12024//Indo_Pacific_data.rds")

ggplot(data = Indo_Pacific) +
  geom_sf(aes(fill = REALM)) +
  theme_minimal() +
  labs(title = "Marine Ecoregions of the World", fill = "Province")



# total survery area
total_area <- sum(Indo_Pacific$area_km2)
print(paste("Total survey area:", total_area, "km^2"))
#################################

# View the column names and a few rows of the data
colnames(shapefile_data)
head(shapefile_data)

# Summary of the shapefile data
summary(shapefile_data)

# Calculate the area for each ecoregion in square kilometers
shapefile_data <- shapefile_data %>%
  mutate(area_km2 = st_area(.) / 10^6)  # Assuming the coordinate system is in meters

# Select relevant columns and display the data
ecoregion_area_data <- shapefile_data %>%
  dplyr::select(ECOREGION, PROVINCE, area_km2)

# Display the ecoregion area data
print(ecoregion_area_data)

# Save to a new shapefile
st_write(ecoregion_area_data, "R_code_litreview_sem12024/new_shapefile.shp")

# Save to a CSV file
write.csv(as.data.frame(ecoregion_area_data), "R_code_litreview_sem12024/ecoregion_area_data.csv")

shapefile_data <- shapefile_data %>%
  mutate(area_km2 = st_area(.) / 10^6)  # Convert from m^2 to km^2

# Select relevant columns and display the data
ecoregion_area_data <- shapefile_data %>%
  dplyr::select(ECOREGION, PROVINCE, area_km2)

# Display the ecoregion area data
print(ecoregion_area_data)

# Save the processed data to a CSV file
write.csv(as.data.frame(ecoregion_area_data), "R_code_litreview_sem12024/ecoregion_area_data.csv")

coral_traits$coralcover <- (coral_traits$MeanPCcover * (sum(Indo_Pacific$area_km2)/0.7))

write.csv(as.data.frame(ecoregion_area_data), "R_code_litreview_sem12024/coraldata_final.csv")

subset(ecoregion_area_data, ECOREGION == "")

############ Determining the Fecundity #################
# Separate the data into fecundity and coral cover

for_coral_wide <- coral_traits %>%
  dplyr::select(-location_name, -resource_id, -resource_secondary_id, -value.x, -value_type, -SuscBleach_IUCN, -trait_id, -standard_unit)

for_coral_wide.2 <- distinct(for_coral_wide)

### getting rid of duplicate (unrealistic duplicate??)
for_coral_wide.3 <- subset(for_coral_wide.2, value.y != 45.818)


# Pivot the data to wide format
data_wide <- for_coral_wide.3 %>%
  pivot_wider(names_from = trait_name, values_from = value.y)

names(data_wide) <- gsub(" ", "_", names(data_wide))

####

list_columns <- sapply(data_wide, function(x) any(sapply(x, function(y) is.character(y) && length(y) > 1)))

names(list_columns[list_columns])

#### Make all lists the same length in each column 

# Function to pad lists with NA to the same length, handling NULL values
#pad_list <- function(lst) {
#  max_len <- max(sapply(lst, function(x) if (is.null(x)) 0 else length(x)))
# lapply(lst, function(x) {
#   if (is.null(x)) {
#     x <- rep(NA, max_len)
#   } else {
#     length(x) <- max_len
#     x
#   }
#   x
# })
#}

# Pad lists in each column
#data_wide_padded <- data_wide %>%
#  mutate(across(everything(), ~ if (is.list(.)) pad_list(.) else .))
#

data_wide_condensed <- data_wide %>%
  dplyr::select(-Genus_fossil_stage, -Symbiodinium_clade, -Water_temperature, -Season, -Turbidity, -Plankton_controlled, -Geographical_region, -Symbiodinium_subclade, -Habitat_type, -Wave_exposure, -Direction, -Shaded, -Growth_form_Veron, -Water_depth, -Irradiance, -Year, -Colony_part, -Damage, -'Indo-Pacific_faunal_province', -Month, -Flow_rate, -Fish_predation_controlled, -Fish_association_controlled) 

unnested_data <- data_wide_condensed %>%
  mutate(across(where(is.list), ~ ifelse(sapply(., length) == 1, unlist(.), .))) %>%
  unnest(cols = everything(), keep_empty = TRUE)

list_columns <- sapply(unnested_data, is.list)

print(list_columns)

#unnested_data <- data_wide %>%
#  unnest(cols = c(Genus_fossil_stage, Symbiodinium_clade, Water_temperature, Season, Turbidity, Plankton_controlled, Geographical_region, Symbiodinium_subclade, Habitat_type, Wave_exposure, Direction, Shaded, Growth_form_Veron, Water_depth, Irradiance, Year, Colony_part, Damage, 'Indo-Pacific_faunal_province',Polyp_fecundity,Month,Flow_rate,Fish_predation_controlled,Fish_association_controlled)) 

# Filter and select relevant data for polyp fecundity

### Replace NULL with NA in the dataset 


# Function to replace NULL with NA
replace_null_with_na <- function(x) {
  x[sapply(x, is.null)] <- NA
  return(x)
}

# Apply the function to all columns
data_wide_condensed2 <- unnested_data %>%
  mutate(across(everything(), replace_null_with_na))


# Now convert relevant columns to numeric
data_wide_condensed2 <- data_wide_condensed2 %>%
  mutate(Polyp_fecundity = as.numeric(Polyp_fecundity), Polyps_per_area = as.numeric(Polyps_per_area), Colony_fecundity = as.numeric(Colony_fecundity))

data_wide_condensed2 <- data_wide_condensed2 %>%
  mutate(
    Polyp_fecundity = as.numeric(replace(Polyp_fecundity, !grepl("^[0-9.]+$", Polyp_fecundity), NA)),
    Polyps_per_area = as.numeric(replace(Polyps_per_area, !grepl("^[0-9.]+$", Polyps_per_area), NA)),
    Colony_fecundity = as.numeric(replace(Colony_fecundity, !grepl("^[0-9.]+$", Colony_fecundity), NA))
  )

clean_numeric_column <- function(column) {
  column <- as.character(column)  # Ensure it's a character vector
  column[!grepl("^-?\\d+(\\.\\d+)?$", column)] <- NA  # Replace non-numeric values with NA
  as.numeric(column)  # Convert to numeric
}

# Apply the cleaning function to relevant columns
data_wide_condensed2 <- data_wide_condensed2 %>%
  mutate(
    Polyp_fecundity = clean_numeric_column(Polyp_fecundity),
    Polyps_per_area = clean_numeric_column(Polyps_per_area),
    Colony_fecundity = clean_numeric_column(Colony_fecundity)
  )

data_wide_condensed2 <- data_wide_condensed2 %>%
  mutate(pre_fecundity = if_else(is.na(Polyp_fecundity) | is.na(Polyps_per_area), NA_real_, Polyp_fecundity * Polyps_per_area *1e10))

data_wide_condensed2$Colony_fecundity <- ( data_wide_condensed2$Colony_fecundity * 1e12)

data_wide_condensed2 <- data_wide_condensed2 %>%
  mutate(fecundity = if_else(is.na(pre_fecundity) | is.na(coralcover), NA_real_, pre_fecundity * coralcover *1e10))

data_wide_condensed2$fecundity <- (data_wide_condensed2$Colony_fecundity * data_wide_condensed2$coralcover)

list_columns <- sapply(data_wide_condensed2, is.list)

print(list_columns)

unnested_data <- data_wide_condensed2 %>%
  mutate(across(where(is.list), ~ ifelse(sapply(., length) == 1, unlist(.), .))) %>%
  unnest(cols = everything(), keep_empty = TRUE)

write.csv(data_wide_condensed2, file = "R_code_litreview_sem12024/funspacedata.csv", row.names = TRUE)

# Filter and select relevant data for polyps per area
#polyps_per_area_data <- traitnames %>%
#  filter(trait_name == "Colony fecundity")
#  filter(trait_name == "Polyps per area") %>%
# dplyr::select(Species, polyps_per_area = Meanvalue)

# Join the data by species
#combined_data <- polyp_fecundity_data %>%
# inner_join(polyps_per_area_data, by = "Species")

# Calculate fecundity
#combined_data <- combined_data %>%
# mutate(fecundity = polyp_fecundity * polyps_per_area)

# Convert polyp fecundity from cm² to km²
#combined_data <- combined_data %>%
# mutate(fecundity = fecundity * 1e10)

# Select relevant columns and rename fecundity
#polyp_combined_data <- combined_data %>%
# dplyr::select(Species, fecundity)

# Filter and select relevant data for colony fecundity
#colony_fecundity_data <- traitnames %>%
#  filter(trait_name == "Colony fecundity") %>%
# dplyr::select(Species, fecundity = Meanvalue)

# Convert colony fecundity from mm² to km²
#colony_fecundity_data <- colony_fecundity_data %>%
# mutate(fecundity = fecundity * 1e12)

# Identify species that have both colony and polyp fecundity
#species_with_colony_fecundity <- colony_fecundity_data$Species

# Exclude polyp fecundity data for species that have colony fecundity
#polyp_combined_data <- polyp_combined_data %>%
#  filter(!Species %in% species_with_colony_fecundity)

# Combine the remaining polyp-based fecundity data with the colony fecundity data
#final_combined_data <- bind_rows(polyp_combined_data, colony_fecundity_data)

# Display the result
#print(final_combined_data)

#####################################

#new_dataset2 <- polyp_combined_data %>%
#  filter(Species %in% coraldata)


#new_dataset <- final_combined_data %>%
#  left_join(coraldata, by = "Species")

# Calculate survey fecundity
#new_dataset <- new_dataset %>%
#  mutate(survey_fecundity = fecundity * coralcover)

#coral_dataset <- new_dataset %>%
#  left_join(coraldata, by = "Species")

#subset(coral_dataset, )

# Display the result
#print(coral_dataset_final)

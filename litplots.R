library(dplyr)
library(tidyr)
library(funspace)
library(missForest)
library(caret)
library(xml2)
library(sf)
library(dplyr)
library(readxl)
library(ggplot2)
library(reshape2)

############## Using the computed data from funspace to create the plots for literature review ###########################

data <- read.csv("R_code_litreview_sem12024/imputedtraitdata.csv")

coral_data_imputed <- data %>%
  dplyr::select(-X)

names(coral_data_imputed)[names(coral_data_imputed) == "coral_data...categorical_identifiers."] <- "Species"

coral_data_imputed <- coral_data_imputed %>%
  mutate(Genus = sapply(strsplit(Species, " "), `[`, 1))


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

region_data <- species_abundance %>%
  dplyr::select(-Habitat, -"Number of intercepts")

coral_region <- coral_data_imputed %>%
  left_join(region_data, by = "Species")



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

coral <- coral_data_imputed %>% 
  group_by(Species, Genus)%>%
  mutate(average_fecundity = mean(fecundity))

coral <- coral %>% 
  dplyr::select(Species, Genus, fecundity)

coral_shape <- merge(coral, region, by = "Species")

coral_shape <- merge(coral_shape, Indo_Pacific_data, by = c("Species", "Region"))

coral_genus <- coral_shape %>% 
  dplyr::select(Species, Genus, Region, PROVINCE, REALM, total_area_km2)

distinct(coral_genus)

plotting <- merge(coral_trait, coral_genus, by = "Species")



#################################### Plotting Loaded Data #####################################################################################################################

# Calculate average fecundity for each growth form
average_fecundity <- coral_data_imputed %>%
  group_by(GrowthForm_CTD) %>%
  summarize(average_fecundity = mean(fecundity, na.rm = TRUE))

ggplot(average_fecundity, aes(x = GrowthForm_CTD, y = average_fecundity)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(title = "Average Fecundity by Growth Form", x = "Growth Form", y = "Average Fecundity")

coral_data_imputed <- coral_data_imputed %>%
  mutate(Oocyte_size_at_maturity = as.numeric(Oocyte_size_at_maturity))

# Plot Oocyte size at maturity vs fecundity
ggplot(coral_data_imputed, aes(x = Oocyte_size_at_maturity, y = fecundity)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "blue") +
  theme_minimal() +
  labs(title = "Scatter Plot of Oocyte Size at Maturity vs Fecundity", x = "Oocyte Size at Maturity", y = "Fecundity")


# Ensure Oocyte_size_at_maturity is numeric
coral_data_imputed <- coral_data_imputed %>%
  mutate(Oocyte_size_at_maturity = as.numeric(Oocyte_size_at_maturity))

# Scatter plot of Oocyte Size at Maturity vs. Fecundity
ggplot(coral_data_imputed, aes(x = fecundity, y = Oocyte_size_at_maturity, color = Genus, shape = larval)) +
  geom_point(size = 3) +
  geom_smooth(method = "lm", se = FALSE, aes(group = 1), color = "black") +
  theme_minimal() +
  labs(
    x = expression("Average Fecundity Acroos Indo-Pacific (km"^2*")"),
    y = "Mean Oocyte Size at Maturity (µm)"
  ) +
  scale_color_discrete(name = "Genus") +
  scale_shape_discrete(name = "Larval Type")

#######


# Scatter plot of Oocyte Size at Maturity vs. Fecundity by genus and larval type
ggplot(coral_data_imputed, aes(x = fecundity, y = Oocyte_size_at_maturity, color = Genus, shape = larval)) +
  geom_point(size = 3) +
  geom_smooth(method = "lm", se = FALSE, aes(group = Genus, color = Genus), show.legend = FALSE) +
  theme_minimal() +
  labs(
    title = "Oocyte Size at Maturity vs. Fecundity by Genus and Larval Type",
    x = "Fecundity",
    y = "Oocyte Size at Maturity"
  ) +
  scale_color_discrete(name = "Genus") +
  scale_shape_discrete(name = "Larval Type")

##########

ggplot(coral_data_imputed, aes(x = fecundity, y = Oocyte_size_at_maturity, color = Genus, shape = larval)) +
  geom_point(size = 3) +
  geom_smooth(method = "lm", se = FALSE, aes(group = Genus, color = Genus), show.legend = FALSE) +
  my_theme +
  labs(
    title = "Oocyte Size at Maturity vs. Fecundity by Genus and Larval Type",
    x = "Fecundity",
    y = "Oocyte Size at Maturity"
  ) +
  scale_color_discrete(name = "Genus") +
  scale_shape_discrete(name = "Larval Type") +
  facet_wrap(~ larval)

############## Mean Genus values 

mean_values <- coral_data_imputed %>%
  group_by(Genus, larval) %>%
  summarize(
    mean_fecundity = mean(fecundity, na.rm = TRUE),
    mean_coralcover = mean(coralcover, na.rm = TRUE)
  )

# Scatter plot of mean Oocyte Size at Maturity vs. mean Fecundity by genus
larval_labels <- c("Brooder", "Spawner")

# Use the labeller function in facet_wrap
ggplot(mean_values, aes(x = mean_fecundity, y = mean_coralcover, color = Genus, shape = larval)) +
  geom_point(size = 3) +
  geom_smooth(method = "lm", se = FALSE, aes(group = 1), color = "black") +
  my_theme +
  labs(
    x = expression("Average Fecundity Across Indo-Pacific (km"^2*")"),
    y = expression("Mean Coral Cover in Indo-Pacific (km"^2*")")
  ) +
  scale_color_discrete(name = "Genus") +
  scale_shape_discrete(name = "Larval Type") +
  facet_wrap(~ larval)

###################

#install.packages("psych")
library(psych)

# Check the structure of the dataframe
str(mean_values)

# Convert to factors if necessary
mean_values$Genus <- as.factor(mean_values$Genus)
mean_values$larval <- as.factor(mean_values$larval)

# Summary statistics
summary(mean_values)
describe(mean_values)

# Correlation matrix
cor(mean_values[, c("mean_fecundity", "mean_coralcover")])

# ANOVA for mean_fecundity
anova_fecundity <- aov(mean_fecundity ~ Genus + larval, data = mean_values)
summary(anova_fecundity)

anova_range <- aov(mean_coralcover ~ Genus + larval, data = mean_values)
summary(anova_range)

# Linear regression model
lm_model <- lm(mean_fecundity ~ mean_range, data = mean_values)
summary(lm_model)

# Diagnostic plots
par(mfrow = c(2, 2))
plot(lm_model)

# Linear regression with interaction effects
lm_interaction <- lm(mean_fecundity ~ mean_coralcover * Genus * larval, data = mean_values)
summary(lm_interaction)

# Boxplot for mean_fecundity by genera
ggplot(mean_values, aes(x = Genus, y = mean_fecundity, fill = larval)) +
  geom_boxplot() +
  labs(title = "Mean Fecundity by Genera and Larval Type",
       x = "Genera",
       y = "Mean Fecundity") +
  theme_minimal()

ggplot(mean_values, aes(x = mean_coralcover, y = mean_fecundity, color = Genus)) +
  geom_point() +
  labs(title = "Mean Fecundity vs Mean Range",
       x = "Mean Range",
       y = "Mean Fecundity") +
  theme_minimal()
#################

# Fit a linear regression model
lm_model <- lm(mean_fecundity ~ mean_coralcover, data = mean_values)

# Summary of the linear regression model
summary(lm_model)

par(mfrow = c(2, 2))
plot(lm_model)

# Scatter plot with regression line
ggplot(mean_values, aes(x = mean_range, y = mean_fecundity)) +
  geom_point() +
  geom_smooth(method = "lm", col = "red") +
  labs(title = "Mean Fecundity vs Mean Range",
       x = "Mean Range",
       y = "Mean Fecundity") +
  theme_minimal()

###################


# Filter data for brooders
brooders <- mean_values %>% filter(larval == "brooder")

# Filter data for spawners
spawners <- mean_values %>% filter(larval == "spawner")

# Linear regression for brooders
lm_brooders <- lm(mean_fecundity ~ mean_coralcover, data = brooders)

# Summary of the linear regression for brooders
summary(lm_brooders)

# Diagnostic plots for brooders
par(mfrow = c(2, 2))
plot(lm_brooders)

# Linear regression for spawners
lm_spawners <- lm(mean_fecundity ~ mean_coralcover, data = spawners)

# Summary of the linear regression for spawners
summary(lm_spawners)

# Diagnostic plots for spawners
par(mfrow = c(2, 2))
plot(lm_spawners)

# Scatter plot with regression line for brooders
ggplot(brooders, aes(x = mean_coralcover, y = mean_fecundity)) +
  geom_point() +
  geom_smooth(method = "lm", col = "red") +
  labs(title = "Mean Fecundity vs Mean Range (Brooders)",
       x = "Mean Range",
       y = "Mean Fecundity") +
  theme_minimal()

# Scatter plot with regression line for spawners
ggplot(spawners, aes(x = mean_coralcover, y = mean_fecundity)) +
  geom_point() +
  geom_smooth(method = "lm", col = "red") +
  labs(title = "Mean Fecundity vs Mean Range (Spawners)",
       x = "Mean Range",
       y = "Mean Fecundity") +
  theme_minimal()
#################


# Fit the linear regression model
regression <- lm(mean_range ~ mean_fecundity, data = mean_values)

# Summarize the regression model
summary_regression <- summary(regression)

# Retrieve the R² value
r_squared <- summary_regression$r.squared

# Print the R² value
print(paste("R²: ", r_squared))

# Create the plot
plot <- ggplot(mean_values, aes(x = mean_fecundity, y = mean_range, color = Genus, shape = larval)) +
  geom_point(size = 3) +
  geom_smooth(method = "lm", se = FALSE, aes(group = 1), color = "black") +
  theme_minimal() +  # Replace my_theme with a minimal theme for demonstration
  labs(
    x = expression("Average Fecundity Across Indo-Pacific (km"^2*")"),
    y = expression("Mean range (km"^2*")")
  ) +
  scale_color_discrete(name = "Genus") +
  scale_shape_discrete(name = "Larval Type") +
  facet_wrap(~ larval) +
  stat_cor(aes(label = paste(..rr.label.., ..p.label.., sep = "~`,`~")),
           label.x = 0.1, label.y = max(mean_values$mean_range))

# Print the plot
print(plot)
######### Genus mean with trend lines 

ggplot(coral_data_imputed, aes(x = fecundity, y = Oocyte_size_at_maturity, color = Genus, shape = larval)) +
  geom_point(size = 3) +
  geom_smooth(method = "lm", se = FALSE, aes(group = Genus, color = Genus), show.legend = FALSE) +
  theme_minimal() +
  labs(
    x = expression("Average Fecundity Acroos Indo-Pacific (km"^2*")"),
    y = "Oocyte Size at Maturity (µm)"
  ) +
  scale_color_discrete(name = "Genus") +
  scale_shape_discrete(name = "Larval Type")+
  facet_wrap(~ Genus)


ggplot(coral_data_imputed, aes(x = fecundity, y = Oocyte_size_at_maturity, color = Genus, shape = larval)) +
  geom_point(size = 3) +
  geom_smooth(method = "lm", se = FALSE, aes(group = Genus, color = Genus), show.legend = FALSE) +
  theme_minimal() +
  labs(
    x = expression("Average Fecundity Across Indo-Pacific (km"^2*")"),
    y = "Oocyte Size at Maturity (µm)"
  ) +
  scale_color_discrete(name = "Genus") +
  scale_shape_discrete(name = "Larval Type") +
  facet_wrap(~ Genus) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    strip.text = element_text(size = 8)
  )


# Boxplots of Fecundity by Growth Form
ggplot(coral_data_imputed, aes(x = GrowthForm_CTD, y = fecundity)) +
  geom_boxplot() +
  theme_minimal() +
  labs(title = "Boxplots of Fecundity by Growth Form", x = "Growth Form", y = "Fecundity")

# Calculate average fecundity for each growth form
average_fecundity <- coral_data_imputed %>%
  group_by(GrowthForm_CTD) %>%
  summarize(average_fecundity = mean(fecundity, na.rm = TRUE))

# Bar Plot of Average Fecundity by Growth Form
ggplot(average_fecundity, aes(x = GrowthForm_CTD, y = average_fecundity)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(title = "Average Fecundity by Growth Form", x = "Growth Form", y = "Average Fecundity")

# Compute correlations between traits
correlation_matrix <- coral_data_imputed %>%
  dplyr::select(fecundity, Oocyte_size_at_maturity) %>%
  cor()


# Faceted Scatter Plots of Fecundity vs Oocyte Size at Maturity by Growth Form
ggplot(coral_data_imputed, aes(x = Oocyte_size_at_maturity, y = fecundity)) +
  geom_point() +
  facet_wrap(~ GrowthForm_CTD) +
  theme_minimal() +
  labs(title = "Scatter Plot of Fecundity vs Oocyte Size at Maturity by Growth Form", x = "Oocyte Size at Maturity", y = "Fecundity")

# Calculate average fecundity for each growth form
# Summarize average fecundity by genus
average_fecundity <- coral_data_imputed %>%
  group_by(Genus) %>%
  summarize(average_fecundity = mean(fecundity, na.rm = TRUE))

# Bar plot of average fecundity by genus, ordered alphabetically
ggplot(average_fecundity, aes(x = Genus, y = average_fecundity, fill = Genus)) +
  geom_bar(stat = "identity") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(
    x = "Genus",
    y = expression("Average Fecundity Across Indo-Pacific (km"^2*")")
  ) +
  scale_x_discrete(limits = sort(unique(average_fecundity$Genus)))
###################### heat map 

average_fecundity <- coral_data_imputed %>%
  group_by(Genus) %>%
  summarize(average_fecundity = mean(fecundity, na.rm = TRUE)) %>%
  arrange(desc(average_fecundity))

# Bar plot of average fecundity by genus, ordered by fecundity
ggplot(average_fecundity, aes(x = reorder(Genus, -average_fecundity), y = average_fecundity, fill = reorder(Genus, -average_fecundity))) +
  geom_bar(stat = "identity") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(
    x = "Genus",
    y = expression("Average Fecundity Across Indo-Pacific (km"^2*")")
  ) +
  scale_fill_discrete(name = "Genus", guide = guide_legend(reverse = TRUE))

# Aggregate data to calculate average fecundity for each combination of Species and family_molecule
aggregated_data <- coral_data_imputed %>%
  group_by(GrowthForm_CTD, Genus) %>%
  summarize(average_fecundity = mean(fecundity, na.rm = TRUE))

##################################################### MAPPING PLOT ########################################################################################################################

library(rnaturalearth)
library(rnaturalearthdata)
library(sf)

# Load the Indo-Pacific data (replace this with your actual data loading step)
# Indo_Pacific_data <- st_read("path_to_your_shapefile.shp")

land <- ne_countries(scale = "medium", returnclass = "sf")

# Plot the land data
ggplot() +
  geom_sf(data = land, fill = "gray80", color = "black") +
  theme_minimal() +
  labs(
    title = "Land Map",
    fill = "Land"
  )

#Indonesia: Western Coral Triangle, Sunda Shelf, Northwest Australian Shelf, Sahul Shelf, Java Transitional

#American Samoa: Central Polynesia

#French Polynesia: Southeast Polynesia

#Papua New Guinea: Eastern Coral Triangle, 

#Solomon Islands: Tropical Southwestern Pacific 

coral_shapefile <- try(st_read(shapefile_path), silent = TRUE)

coral_shapefile <- shapefile_data %>%
  mutate(Region = case_when(
    PROVINCE %in% c("Western Coral Triangle", "Sunda Shelf", "Northwest Australian Shelf", "Sahul Shelf", "Java Transitional") ~ "Indonesia",
    PROVINCE == "Central Polynesia" ~ "American Samoa",
    PROVINCE == "Southeast Polynesia" ~ "French Polynesia",
    PROVINCE == "Eastern Coral Triangle" ~ "Papua New Guinea",
    PROVINCE == "Tropical Southwestern Pacific" ~ "Solomon Islands",
    TRUE ~ PROVINCE  # Default to the original region if no match is found
  ))

coral_shapefile_central <- subset(coral_shapefile, REALM == "Central Indo-Pacific")
coral_shapefile_eastern <- subset(coral_shapefile, REALM == "Eastern Indo-Pacific")

coral_shapefile <- rbind(coral_shapefile_central,coral_shapefile_eastern)

custom_crs <- st_crs("+proj=laea +lat_0=-25 +lon_0=140 +datum=WGS84 +units=m +no_defs")

# Transform the datasets to the custom CRS
land_transformed <- st_transform(land, crs = custom_crs)
coral_shapefile_transformed <- st_transform(Indo_Pacific_data, crs = custom_crs)

coral_shapefile_transformed <- subset(coral_shapefile_transformed, Region != "Hawaii")
coral_shapefile_transformed <- subset(coral_shapefile_transformed, Region != "Marquesas")
coral_shapefile_transformed <- subset(coral_shapefile_transformed, Region != "Easter Island")
coral_shapefile_transformed <- subset(coral_shapefile_transformed, Region != "Marshall, Gilbert and Ellis Islands")
coral_shapefile_transformed <- subset(coral_shapefile_transformed, Region != "Marshall, Gilbert and Ellis Islands")

# Define the bounding box limits to focus on the central and eastern Indo-Pacific
# Note: These values might need adjustment based on your specific data and region
# Extend further east by increasing xmax
xlim <- c(-6000000, 8000000)  # Extend further east and west
ylim <- c(-7000000, 6000000)  # Extend further north and south

ggplot(data = Indo_Pacific_data) +
  geom_sf(aes(fill = PROVINCE)) +
  theme_minimal() +
  labs(title = "Marine Ecoregions of the World", fill = "Province")


# Plot the Indo-Pacific data with the land map centered around Australia
ggplot() +
  geom_sf(data = land_transformed, fill = "white", color = "black") +
  geom_sf(data = coral_shapefile_transformed, aes(fill = PROVINCE), color = "black") +
  coord_sf(xlim = xlim, ylim = ylim) +
  theme_minimal() +
  labs(
    fill = "Region"
  )


##################################################### HEAT PLOT ########################################################################################################################


# Aggregate data to calculate average fecundity for each combination of Species and family_molecule
aggregated_data <- coral_data_imputed %>%
  group_by(GrowthForm_CTD, family_molecules) %>%
  summarize(average_fecundity = mean(fecundity, na.rm = TRUE))


ggplot(aggregated_data, aes(x = GrowthForm_CTD, y = family_molecules, fill = average_fecundity)) +
  geom_tile() +
  scale_fill_gradient(low = "yellow", high = "blue") +
  theme_minimal() +
  #theme(axis.text.x = element_blank(),  # Remove x-axis text
  #      axis.ticks.x = element_blank()) +  # Remove x-axis ticks
  labs(title = "Heatmap of Fecundity by Species and Family Molecule", x = "Growth Form", y = "Family", fill = "Average Fecundity")

# Aggregate data to calculate average fecundity for each combination of Species and family_molecule
aggregated_data <- coral_data_imputed %>%
  group_by(Genus, Ocean_basin) %>%
  summarize(average_fecundity = mean(fecundity, na.rm = TRUE))


ggplot(aggregated_data, aes(x = Genus, y = Ocean_basin, fill = average_fecundity)) +
  geom_tile() +
  scale_fill_gradient(low = "yellow", high = "blue") +
  theme_minimal() +
  #theme(axis.text.x = element_blank(),  # Remove x-axis text
  #      axis.ticks.x = element_blank()) +  # Remove x-axis ticks
  labs(title = "Heatmap of Fecundity by Species and Family Molecule", x = "Genus", y = "Ocean Basin", fill = "Average Fecundity")

# Aggregate data to calculate average fecundity for each combination of Species and family_molecule
aggregated_data <- coral_data_imputed %>%
  group_by(Genus, IUCN_Red_List_category) %>%
  summarize(average_fecundity = mean(fecundity, na.rm = TRUE))


ggplot(aggregated_data, aes(x = IUCN_Red_List_category, y = Genus, fill = average_fecundity)) +
  geom_tile() +
  scale_fill_gradient(low = "yellow", high = "blue") +
  theme_minimal() +
  #theme(axis.text.x = element_blank(),  # Remove x-axis text
  #      axis.ticks.x = element_blank()) +  # Remove x-axis ticks
  labs(title = "Heatmap of Fecundity by Species and Family Molecule", x = "IUCN Red List category", y = "GenusIUCN Red List category", fill = "Average Fecundity")


aggregated_data <- coral_data_imputed %>%
  group_by(Genus, IUCN_conservation_status) %>%
  summarize(average_fecundity = mean(fecundity, na.rm = TRUE))


ggplot(aggregated_data, aes(x = IUCN_conservation_status, y = Genus, fill = average_fecundity)) +
  geom_tile() +
  scale_fill_gradient(low = "yellow", high = "blue") +
  theme_minimal() +
  #theme(axis.text.x = element_blank(),  # Remove x-axis text
  #      axis.ticks.x = element_blank()) +  # Remove x-axis ticks
  labs(title = "Heatmap of Fecundity by Species and Family Molecule", x = "IUCN conservation status", y = "Genus", fill = "Average Fecundity")


aggregated_data <- coral_region %>%
  group_by(Genus, Region) %>%
  summarize(average_fecundity = mean(fecundity, na.rm = TRUE))


ggplot(aggregated_data, aes(x = Region, y = Genus, fill = average_fecundity)) +
  geom_tile() +
  scale_fill_gradient(low = "yellow", high = "blue") +
  theme_minimal() +
  #theme(axis.text.x = element_blank(),  # Remove x-axis text
  #      axis.ticks.x = element_blank()) +  # Remove x-axis ticks
  labs(title = "Heatmap of Fecundity by Species and Family Molecule", x = "Region", y = "Genus", fill = "Average Fecundity")


aggregated_data <- coral_region %>%
  group_by(family_molecules, Region) %>%
  summarize(average_fecundity = mean(fecundity, na.rm = TRUE))


ggplot(aggregated_data, aes(x = Region, y = family_molecules, fill = average_fecundity)) +
  geom_tile() +
  scale_fill_gradient(low = "red", high = "green") +
  theme_minimal() +
  #theme(axis.text.x = element_blank(),  # Remove x-axis text
  #      axis.ticks.x = element_blank()) +  # Remove x-axis ticks
  labs(title = "Heatmap of Fecundity by Species and Family Molecule", x = "Region", y = "Family", fill = "Average Fecundity")

aggregated_data <- coral_region %>%
  group_by(GrowthForm_CTD, Region) %>%
  summarize(average_fecundity = mean(fecundity, na.rm = TRUE))

ggplot(aggregated_data, aes(x = Region, y = GrowthForm_CTD, fill = average_fecundity)) +
  geom_tile() +
  scale_fill_gradient(low = "yellow", high = "blue") +
  theme_minimal() +
  #theme(axis.text.x = element_blank(),  # Remove x-axis text
  #      axis.ticks.x = element_blank()) +  # Remove x-axis ticks
  labs(title = "Heatmap of Fecundity by Species and Family Molecule", x = "Region", y = "Morphology", fill = "Average Fecundity")

################################################ Heat Plots with Spatial Data ########################################################################################################

coral_genus <- coral_data_imputed %>%
  dplyr::select(Species, Genus, fecundity)

coral_genus <- merge(coral_genus, species_abundance, by = "Species")

coral_genus <- merge(coral_genus, coral_shapefile, by = "Region")

coral_genus <- coral_genus %>%
  dplyr::select(Species, Genus, Region, fecundity, total_area_km2, PROVINCE, REALM, Region)

coral_genus <- distinct(coral_genus)

Province_genus <- coral_genus %>%
  group_by(Genus, Region.x) %>%
  summarize(average_coralcover = mean(`Number of intercepts`, na.rm = TRUE))

my_theme <- theme_minimal() +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.line = element_line(color = "black"),
    axis.text = element_text(size = 12),  # Adjust text size as needed
    axis.title = element_text(size = 14),
    axis.text.x = element_text(angle = 20, hjust = 1, vjust = 1),
    axis.text.y = element_text(size = 10, margin = margin(t = 0, r = 10, b = 0, l = 0))  # Add right margin to y-axis labels
  )


# Create the heatmap plot for Genus and PROVINCE
ggplot(Province_genus, aes(x = Region.x, y = Genus, fill = average_coralcover)) +
  geom_tile(color = "black") +  # Add outline to each tile in black
  scale_fill_gradient(low = "lightgreen", high = "darkgreen") +
  my_theme +
  labs(
    x = "Province",
    y = "Genus",
    fill = expression("Average Coral Abundance (# of intercepts)")
  )
################################
Province_genus <- coral_genus %>%
  group_by(Genus, PROVINCE) %>%
  summarize(average_coralcover = mean(`Number of intercepts`, na.rm = TRUE)) %>%
  ungroup() %>%
  arrange(desc(average_coralcover))

# Ensure Genus and Region.x are factors and set levels to reflect ordering by average_coralcover
Province_genus$Genus <- factor(Province_genus$Genus, levels = unique(Province_genus$Genus[order(-Province_genus$average_coralcover)]))
Province_genus$PROVINCE <- factor(Province_genus$PROVINCE, levels = unique(Province_genus$PROVINCE[order(-Province_genus$average_coralcover)]))

# Define the theme
my_theme <- theme_minimal() +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.line = element_line(color = "black"),
    axis.text = element_text(size = 12),  # Adjust text size as needed
    axis.title = element_text(size = 14),
    axis.text.x = element_text(angle = 20, hjust = 1, vjust = 1),
    axis.text.y = element_text(size = 10, margin = margin(t = 0, r = 10, b = 0, l = 0))  # Add right margin to y-axis labels
  )

# Create the heatmap plot for Genus and Region.x
ggplot(Province_genus, aes(x = PROVINCE, y = Genus, fill = average_coralcover)) +
  geom_tile(color = "black") +  # Add outline to each tile in black
  scale_fill_gradient(low = "lightgreen", high = "darkgreen") +
  my_theme +
  labs(
    x = "Province",
    y = "Genus",
    fill = expression("Average Coral Abundance (# of intercepts)")
  )


###################################

province_fecund <- coral_genus %>%
  group_by(Genus, PROVINCE) %>%
  summarize(average_fecundity = mean(fecundity, na.rm = TRUE)) %>%
  arrange(desc(average_fecundity))

# Convert columns to factors to maintain order in the plot
province_fecund$Genus <- factor(province_fecund$Genus, levels = unique(province_fecund$Genus))
province_fecund$PROVINCE <- factor(province_fecund$PROVINCE, levels = unique(province_fecund$PROVINCE))

# Create the plot
ggplot(province_fecund, aes(x = PROVINCE, y = Genus, fill = average_fecundity)) +
  geom_tile(color = "black") +  # Add outline to each tile in black
  scale_fill_gradient(low = "lightblue", high = "green4") +
  theme_minimal() +  # Replace my_theme with a minimal theme for demonstration
  labs(
    x = "Province",
    y = "Genus",
    fill = expression("Average Fecundity")
  )

##########

library(car)

province_fecund$Genus <- as.factor(province_fecund$Genus)
province_fecund$PROVINCE <- as.factor(province_fecund$PROVINCE)

# Perform two-way ANOVA
anova_result <- aov(average_fecundity ~ PROVINCE + Genus, data = province_fecund)

# Summary of the ANOVA
summary(anova_result)

Anova(anova_result, type = "III")

# Check assumptions of ANOVA: Homogeneity of variances
leveneTest(average_fecundity ~ Genus * PROVINCE, data = province_fecund)

# Plotting the interaction
interaction.plot(province_fecund$Genus, province_fecund$PROVINCE, province_fecund$average_fecundity,
                 xlab = "Genus", ylab = "Average Fecundity", trace.label = "Province", col = rainbow(length(unique(province_fecund$PROVINCE))))

# Tukey's HSD post-hoc test to determine which groups are different
TukeyHSD(anova_result)

# Visualizing the results with ggplot2
ggplot(province_fecund, aes(x = Genus, y = average_fecundity, fill = PROVINCE)) +
  geom_boxplot() +
  labs(title = "Average Fecundity by Genus and Province",
       x = "Genus",
       y = "Average Fecundity") +
  theme_minimal()

################################## 

coral_traits <- read.csv("coral_traits.csv", header = TRUE)



coral_traits <- coral_traits %>%
  mutate(Genus = sapply(strsplit(Species, " "), `[`, 1))

coral_traits$value <- as.numeric(coral_traits$value)

# Subset the data based on multiple trait names
fecundity_traits <- coral_traits %>%
  filter(trait_name %in% c('Colony fecundity', 'Eggs per area', 'Oocyte size at maturity', 'Polyp fecundity' ))


my_theme <- theme_minimal() +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.line = element_line(color = "black"),
    axis.text = element_text(size = 12),  # Adjust text size as needed
    axis.title = element_text(size = 14),
    axis.text.x = element_text(angle = 20, hjust = 1, vjust = 1),
    axis.text.y = element_text(size = 10, margin = margin(t = 0, r = 10, b = 0, l = 0))  # Add right margin to y-axis labels
  )
# Create the scatter plot with adjustments to the x-axis text
ggplot(fecundity_traits, aes(x = value, y = Genus, colour = Genus)) +
  geom_point() +
  facet_wrap(~ trait_name, scales = "free_x", ncol = 2) +  # Adjust number of columns
  theme_minimal() +
  labs(x = "Value", y = "Genus") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 8))  # Adjust angle and size


imputed <- read.csv("imputedtraitdata.csv", header = TRUE)

imputed <- imputed %>%
  dplyr::select(-X)

names(imputed)[names(imputed) == "coral_data...categorical_identifiers."] <- "Species"

imputed <- imputed %>%
  mutate(Genus = sapply(strsplit(Species, " "), `[`, 1))

# Subset the data based on multiple trait names

imputed_fecundity_traits <- imputed %>%
  dplyr::select(Genus, Colony_fecundity, Eggs_per_area, Oocyte_size_at_maturity, Polyp_fecundity) %>%
  mutate(across(everything(), as.character)) %>%
  mutate(across(c(Colony_fecundity, Eggs_per_area, Oocyte_size_at_maturity, Polyp_fecundity), as.numeric))

# Reshape the data to long format
imputed_fecundity_traits <- imputed_fecundity_traits %>%
  pivot_longer(cols = c(Colony_fecundity, Eggs_per_area, Oocyte_size_at_maturity, Polyp_fecundity),
               names_to = "trait_name", values_to = "value")



my_theme <- theme_minimal() +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.line = element_line(color = "black"),
    axis.text = element_text(size = 12),  # Adjust text size as needed
    axis.title = element_text(size = 15),
    axis.text.x = element_text(angle = 20, hjust = 1, vjust = 1),
    axis.text.y = element_text(size = 8, margin = margin(t = 500, r = 0, b = 0, l = 0))  # Add right margin to y-axis labels
  )

# Create the scatter plot with facet_wrap by trait_name
ggplot(imputed_fecundity_traits, aes(x = value, y = Genus, color = Genus)) +
  geom_point() +
  facet_wrap(~ trait_name, scales = "free_x", ncol = 2) +  # Adjust number of columns
  my_theme +
  labs(x = "Value", y = "Genus") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 8))  # Adjust angle and size


polyp <- subset(fecundity_traits, trait_name == "Polyp fecundity")

larval_test <- merge(polyp, coral_traits_genus, by = "Species")

polyp_imputed <- imputed %>%
  dplyr::select(Species, 'Polyp_fecundity')

pfecundity <- merge(polyp, polyp_imputed, by = "Species")

pfecundity <- merge(pfecundity, larval_test, by = "Species")

pfecundity <- distinct(pfecundity)

ggplot(pfecundity, aes(x = value, y = Polyp_fecundity, colour = Species, shape = larval))+
  geom_point()+
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "black") +
  labs(x = "Measured fecundity", y = "Imputed fecundity")

regression <- lm(Polyp_fecundity ~ value, data = pfecundity)


plot <- plot + 
  stat_cor(aes(label = paste(..rr.label.., ..p.label.., sep = "~`,`~")), 
           label.x = min(pfecundity$value), label.y = max(pfecundity$Polyp_fecundity)) +
  geom_smooth(method = "lm", se = FALSE, linetype = "solid", color = "blue")

print(plot)

common_limits <- c(min(pfecundity$value, pfecundity$Polyp_fecundity), max(pfecundity$value, pfecundity$Polyp_fecundity))

# Create the plot with custom axis limits
plot <- ggplot(pfecundity, aes(x = value, y = Polyp_fecundity, colour = Species, shape = larval)) +
  geom_point() +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "black") +
  labs(x = "Measured fecundity", y = "Imputed fecundity") +
  xlim(common_limits) +
  ylim(common_limits) +
  my_theme

# Print the plot
print(plot)


############## Acropora studies 

#install.packages("RColorBrewer")
library(RColorBrewer)

genus_df <- as.data.frame(genus)
colnames(genus_df) <- c("Genus", "Count")

# Calculate the percentage
genus_df$Percentage <- genus_df$Count / sum(genus_df$Count) * 100

# Reorder the data frame by Count
genus_df <- genus_df %>%
  arrange(desc(Count))

# Create the pie chart
ggplot(genus_df, aes(x = "", y = Percentage, fill = reorder(Genus, -Count))) +
  geom_bar(width = 0.2, stat = "identity", color = "white") +
  coord_polar(theta = "y") +
  theme_minimal() +
  labs(title = "Genus Distribution of Coral Traits", x = "", y = "") +
  theme(axis.text.x = element_blank(), axis.ticks = element_blank(), panel.grid = element_blank()) +
  scale_fill_discrete(name = "Genus")+
  geom_text(aes(label = ifelse(Percentage > 1, paste0(round(Percentage, 1), "%"), "")), 
            position = position_stack(vjust = 0.5), size = 3)


################ 
#install.packages("officer")
library(officer)
#install.packages("flextable")
library(flextable)

article <- read_xlsx("megan/sorted.xlsx")

cleaned_df <- na.omit(article)

write.csv(cleaned_df, file = "mydata.csv", row.names = TRUE)

ft <- flextable(cleaned_df)

ft <- set_table_properties(ft, width = 1.0, layout = "autofit")

# Save the flextable to a Word document
doc <- read_docx()
doc <- body_add_flextable(doc, value = ft)
print(doc, target = "my_wide_table.docx")





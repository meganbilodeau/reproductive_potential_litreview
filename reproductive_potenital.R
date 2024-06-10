### data compile for determining the reproduction potential of the reef

### load data and create a data set containing the following:
### scientific name, mean number of colonies (in 100 million), average colony fecundity, reproduction type, sex

### load up initial data from Dietzel, Andreas, Bode, Michael, Connolly, Sean R., and Hughes, Terry P. (2021) The population sizes and global extinction risk of reef-building coral species at biogeographic scales. Nature Ecology & Evolution.
## grabbing data from Dietzel et al 2021 and creating dataset in separate folder
##load("RData/Input_Files.RData")
##coraldata <- SupportData
##write.csv(coraldata, file = "coraldata.csv", row.names = TRUE)

## read in data
coral <- read_csv("R_code_litreview_sem12024/coraldata.csv")

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

coraltraits <- read_csv("R_code_litreview_sem12024/ctdb_1.1.1_data.csv")

coraltraits <- coraltraits %>%
  rename(Species = specie_name)

fecundity <- read_csv("R_code_litreview_sem12024/fecundity.csv")

colony_fecundity <- subset(fecundity, fecundity$trait_id == 216)

colony_fecundity <- colony_fecundity %>%
  rename(Species = specie_name)
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

coraltrait <- subset(coraltraits, coraltraits$trait_id == 12 | coraltraits$trait_id == 209 | coraltraits$trait_id == 155 | coraltraits$trait_id == 211 | coraltraits$trait_id == 148 | coraltraits$trait_id == 231 | coraltraits$trait_id == 138)

coral_trait <- bind_rows(coraltrait, colony_fecundity)


##### mean value of traits
coral_trait$value <- as.numeric(coral_trait$value)

# Group by Species and trait_name, then calculate the mean value
mean_traits <- coral_trait %>%
  group_by(Species, trait_name, standard_unit) %>%
  summarize(Meanvalue = mean(value, na.rm = TRUE), .groups = 'drop')


new_row <- data.frame(
  Species = c("Acropora hyacinthus","Acropora cytherea", "Acropor digitifera", "Acropora humilis", "Acropora nasuta", "Acropora spathulata", "Goniastrea pectinata", "Goniastrea retiformis"),
  trait_name = c("Polyp fecundity","Polyp fecundity", "Polyp fecundity", "Polyp fecundity", "Polyp fecundity", "Polyp fecundity", "Polyp fecundity", "Polyp fecundity"),
  standard_unit = c("units","units","units","units","units","units","units","units"),
  Meanvalue = c(5.908, 1.023, 11.507, 14.999, 14.074, 9.496, 7.036, 4.966)
)

meantraits <- bind_rows(mean_traits, new_row)

coral_merge <- merge(coraldata, mean_traits, by = "Species")


### checking how many of each trait input there are
group <- coral_trait %>% group_by(Species, trait_name)

### checking how many species there are


write.csv(coral_merge, file = "R_code_litreview_sem12024//coralmerge.csv", row.names = TRUE)

new_df_base <- coral_merge[, c("Species", "MeanPCcover", "Range", "ReefArea_Range", "family_molecules", "larval", "sexual", "Lower bound", "Mean", "Upper bound", "trait_name","standard_unit" ,"Meanvalue")]


write.csv(new_df_base, file = "R_code_litreview_sem12024//coralmerge.csv", row.names = TRUE)







##### trying out funspace script #######

library(dplyr)
library(tidyr)
library(funspace)
library(missForest)
library(caret)


coral_data <- read.csv("R_code_litreview_sem12024/funspacedata.csv")

coral_data <- coral_data %>%
  dplyr::select(-X)

############ First need to get rid of any categorical identifiers (species, Location etc) anything that isnt a continuous trait

# Convert character columns to factors
coral_data <- coral_data %>%
  mutate(across(where(is.character), as.factor))

# Define categorical identifiers
categorical_identifiers <- c("Species", "GrowthForm_CTD", "LHS_CTD", "Abund_CTD", "IUCNstatus", "major_clade", "family_molecules", "family_morphology", "larval", "sexual", "IUCN_conservation_status", "IUCN_Red_List_category", "Life_history_strategy", "Mode_of_larval_development", "Ocean_basin", "Sexual_system", "Substrate_attachment", "Zooxanthellate", "Growth_form_typical", "Growth_form_Wallace", "Growth_outline_type", "Sample_identifier", "Symbiodinium_sp._in_propagules", "Colony_bleached", "Individual_identifier")


############## Categorical data added back in 

coral_data <- coral_data %>%
  mutate(across(where(is.character), as.factor))


# Define categorical identifiers
categorical_identifiers <- c("Species")

# Exclude the categorical identifiers for imputation
traits_to_impute <- coral_data %>%
  dplyr::select(-all_of(categorical_identifiers))

# Impute missing values using missForest
imputed_traits <- missForest(traits_to_impute)$ximp

# Combine the imputed data with the categorical identifiers
coral_data_imputed <- cbind(coral_data[, categorical_identifiers], imputed_traits)

write.csv(coral_data_imputed, file="R_code_litreview_sem12024/imputedtraitdata.csv", row.names = TRUE)

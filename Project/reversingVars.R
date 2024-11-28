#Reversing the items

# List of reverse-coded items
reverse_items <- c(6, 21, 31, 2, 12, 27, 37, 8, 18, 23, 43, 9, 24, 34, 35, 41)

ds_rev <- ds_og

# Reverse coding: apply to each specified column
for (item in reverse_items) {
  column_name <- paste0("BFI_", item) # Column names are in the format "BFI_number"
  ds_rev[[column_name]] <- 6 - ds_rev[[column_name]]
}

# Check the dataset to confirm changes
head(ds_rev)
head(ds)
###########################################################################
# Define trait groups
extraversion_items <- c(1, 6, 11, 16, 21, 26, 31, 36)
agreeableness_items <- c(2, 7, 12, 17, 22, 27, 32, 37, 42)
conscientiousness_items <- c(3, 8, 13, 18, 23, 28, 33, 38, 43)
neuroticism_items <- c(4, 9, 14, 19, 24, 29, 34, 39)
openness_items <- c(5, 10, 15, 20, 25, 30, 35, 40, 41, 44)

# Compute sums for each trait
ds_rev$BFI_extraversion <- rowSums(ds_rev[paste0("BFI_", extraversion_items)])
ds_rev$BFI_agreeableness <- rowSums(ds_rev[paste0("BFI_", agreeableness_items)])
ds_rev$BFI_conscientiousness <- rowSums(ds_rev[paste0("BFI_", conscientiousness_items)])
ds_rev$BFI_neuroticism <- rowSums(ds_rev[paste0("BFI_", neuroticism_items)])
ds_rev$BFI_openness <- rowSums(ds_rev[paste0("BFI_", openness_items)])

# Check the dataset to confirm the new columns
head(ds_og)
ds_og$BFI_agreeableness
ds_rev$BFI_agreeableness
rowSums(ds_og[paste0("BFI_", agreeableness_items)])

################################################################################x

ds_close$BFI_closeones_1
ds_rev_close <- ds_close

for (item in reverse_items) {
  column_name <- paste0("BFI_closeones_", item) # Column names are in the format "BFI_number"
  ds_rev_close[[column_name]] <- 6 - ds_rev_close[[column_name]]
}

ds_rev_close$BFI_closeones_extraversion <- rowSums(ds_rev_close[paste0("BFI_closeones_", extraversion_items)])
ds_rev_close$BFI_closeones_agreeableness <- rowSums(ds_rev_close[paste0("BFI_closeones_", agreeableness_items)])
ds_rev_close$BFI_closeones_conscientiousness <- rowSums(ds_rev_close[paste0("BFI_closeones_", conscientiousness_items)])
ds_rev_close$BFI_closeones_neuroticism <- rowSums(ds_rev_close[paste0("BFI_closeones_", neuroticism_items)])
ds_rev_close$BFI_closeones_openness <- rowSums(ds_rev_close[paste0("BFI_closeones_", openness_items)])

ds_rev_close$BFI_closeones_agreeableness
rowSums(ds_close[paste0("BFI_closeones_", agreeableness_items)])
ds_close$BFI_closeones_agreeableness

##############################################################################
# Function to check equivalence for all traits
check_trait_equivalence <- function(ds1, ds2, use_closeones = 0) {
  # List of trait column names to check
  traits <- c("extraversion", "agreeableness", "conscientiousness", "neuroticism", "openness")
  
  # Initialize a named logical vector to store equivalence results
  equivalence_results <- setNames(logical(length(traits)), traits)
  
  # Prefix for column names based on `use_closeones` parameter
  prefix <- if (use_closeones == 1) "BFI_closeones_" else "BFI_"
  
  # Loop through each trait and check equivalence
  for (trait in traits) {
    col1 <- paste0(prefix, trait) # Column name in ds1
    col2 <- paste0(prefix, trait) # Column name in ds2
    equivalence_results[trait] <- all(ds1[[col1]] == ds2[[col2]])
  }
  
  return(equivalence_results)
}

# Example Usage
# For regular traits
equivalence_regular <- check_trait_equivalence(ds1 = ds_rev_close, ds2 = ds_close, use_closeones = 0)

# For closeones traits
equivalence_closeones <- check_trait_equivalence(ds1 = ds_rev_close, ds2 = ds_close, use_closeones = 1)

# Print results
print(equivalence_regular)
print(equivalence_closeones)
################################################################################
#Puting it together


ds_combined <- cbind(ds_rev, ds_rev_close)

# Reorder columns to match the original `ds`
ds_final_rev <- ds_combined[, colnames(ds)]

# Check the structure to confirm it matches `ds`
str(ds_final_rev)

# Optionally compare column names to verify the order
all(colnames(ds_final_rev) == colnames(ds)) # Should return TRUE

#BFI ANALYSIS
#ABE-KZA
#November 2024
##################################################################################
# Load necessary libraries
library(ggplot2)
library(dplyr)

# Theme
theme_fig <- function(base_size = 17, base_family = "") {
  theme_bw(base_size = base_size, base_family = base_family) +
    theme(
      legend.key = element_rect(fill = "white", colour = NA),
      axis.line = element_line(colour = "black"),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      panel.background = element_blank(),
      plot.title = element_blank(),
      legend.background = element_blank()
    )
}

###############################################################################
selected_columns <- c(
  "BFI_2", "BFI_7", "BFI_12", "BFI_17", "BFI_22", 
  "BFI_27", "BFI_32", "BFI_37", "BFI_42",
  "BFI_closeones_2", "BFI_closeones_7", "BFI_closeones_12", 
  "BFI_closeones_17", "BFI_closeones_22", "BFI_closeones_27", 
  "BFI_closeones_32", "BFI_closeones_37", "BFI_closeones_42",
  "BFI_agreeableness", "BFI_closeones_agreeableness"
  
)
subdataset <- ds_og[, c("BFI_2", "BFI_7", "BFI_12", "BFI_17", 
                        "BFI_22", "BFI_27", "BFI_32", "BFI_37", 
                        "BFI_42")]

subdataset_close <-ds_close[,c("BFI_closeones_2", "BFI_closeones_7", "BFI_closeones_12", 
                          "BFI_closeones_17", "BFI_closeones_22", "BFI_closeones_27", 
                          "BFI_closeones_32", "BFI_closeones_37", "BFI_closeones_42")]
##############################################################################
# Calculate means and standard deviations
stats <- data.frame(
  Item = names(subdataset),
  Mean = sapply(subdataset, mean),
  SD = sapply(subdataset, sd),
  Group = "Dataset 1"
)

# Create the ggplot
ggplot(stats, aes(x = Item, y = Mean)) +
  geom_point(size = 3, color = "blue") +  # Plot means
  geom_errorbar(aes(ymin = Mean - SD, ymax = Mean + SD), width = 0.2, color = "black") +  # Add error bars
  labs(
    title = "Mean and SD of Items",
    x = "Items",
    y = "Mean with Confidence Interval"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#For close ones
stats_close <- data.frame(
  Item = names(subdataset_close),
  Mean = sapply(subdataset_close, mean),
  SD = sapply(subdataset_close, sd),
  Group = "Dataset 2"
)


ggplot(stats_close, aes(x = Item, y = Mean)) +
  geom_point(size = 3, color = "blue") +  # Plot means
  geom_errorbar(aes(ymin = Mean - SD, ymax = Mean + SD), width = 0.2, color = "black") +  # Add error bars
  labs(
    title = "Mean and SD of Items",
    x = "Items",
    y = "Mean with Confidence Interval"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
#TODO: rename the labels

#Plotted together
combined_stats <- rbind(stats, stats_close)

# Create the ggplot
ggplot(combined_stats, aes(x = Item, y = Mean, color = Group, shape = Group)) +
  geom_point(size = 3) +  # Plot means
  geom_errorbar(aes(ymin = Mean - SD, ymax = Mean + SD), width = 0.2) +  # Add error bars
  labs(
    title = "Mean and SD of Items Across Datasets",
    x = "Items",
    y = "Mean with Confidence Interval"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_color_manual(values = c("blue", "red"))  # Custom colors

#################################################################
# Calculate the difference of means and combined standard deviation
difference_stats <- data.frame(
  Item = names(subdataset),
  MeanDifference = sapply(subdataset, mean) - sapply(subdataset_close, mean),
  SDDifference = sqrt(sapply(subdataset, sd)^2 + sapply(subdataset_close, sd)^2)  # Variance sum rule
)

# Create the ggplot for differences
ggplot(difference_stats, aes(x = Item, y = MeanDifference)) +
  geom_point(size = 3, color = "purple") +  # Plot mean differences
  geom_errorbar(aes(ymin = MeanDifference - SDDifference, ymax = MeanDifference + SDDifference), 
                width = 0.2, color = "black") +  # Add error bars
  labs(
    title = "Difference in Means with Combined SD",
    x = "Items",
    y = "Difference (Dataset1 - Dataset2)"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

###########################################################################
#Normalizing ordinal data
# Define the function to calculate πˆi
calculate_norm <- function(column) {
  mean_col <- mean(column)            # Average item score Ȳ•i
  min_col <- min(column)              # Minimum possible score min(Yi)
  max_col <- max(column)              # Maximum possible score max(Yi)
  (mean_col - min_col) / (max_col - min_col)  # Scaled score
}

# Apply the function to each column
pi_hat <- sapply(subdataset, calculate_norm)

# Display the results
pi_hat #TODO: interpretation

###############################################################################
#Item difficulty 

# Function to calculate πˆi for maximal score
calculate_pi_max <- function(column) {
  max_col <- max(column)  # Maximal possible score
  mean(column == max_col) # Proportion of respondents with max score
}

# Function to calculate πˆi for a cutoff value ki
calculate_pi_cutoffs <- function(column, cutoffs) {
  # Calculate proportions for each cutoff
  sapply(cutoffs, function(cutoff) mean(column >= cutoff))
}

# Calculate πˆi for maximal score
pi_hat_max <- sapply(subdataset, calculate_pi_max)

# Define cut-off values for each item (example cut-offs)
cutoffs_list <- c(3,4,5)  # Set your cut-off values here

# Calculate πˆi for cut-off values
calculate_pi_cutoffs <- function(dataset, cutoffs) {
  # Create an empty data frame to store results
  results <- data.frame(matrix(ncol = length(cutoffs), nrow = ncol(dataset)))
  
  # Set column and row names
  colnames(results) <- paste0("Cutoff_", cutoffs)
  rownames(results) <- colnames(dataset)
  
  # Loop through columns (items)
  for (i in seq_along(dataset)) {
    # Get the current column
    column <- dataset[[i]]
    
    # Loop through cutoffs
    for (j in seq_along(cutoffs)) {
      # Calculate the proportion for the current cutoff
      results[i, j] <- mean(column >= cutoffs[j])
    }
  }
  
  return(results)
}

result_df <- calculate_pi_cutoffs(subdataset, cutoffs)

#################################################################################
#Item discrimination

#A. RIT -TODO: Incorrect?
subdataset$'BFI_total' <- ds$BFI_agreeableness
sapply(subdataset[,1:9], function(i) cor(i, subdataset$BFI_total))
ItemAnalysis(subdataset[,1:9])$RIT
#TODO: I do not know about the negative correlation, what should we do, is it okay?

#B. RIR - TODO: Incorrect?
dataR <- subdataset$BFI_total - subdataset[, 1:9]
diag(cor(subdataset[, 1:9], dataR))
ItemAnalysis(subdataset[,1:9])$RIR #different results?

#ULI
gDiscrim(Data = subdataset[, 1:9])
?gDiscrim
# k number of groups to be divided in
# l: lower group
# i: upper group
gDiscrim(Data = subdataset[, 1:9], k = 5, l = 2,u = 5)

##################################################################################
# Plotting 
DDplot(Data = subdataset[, 1:9], discrim = "ULI")
DDplot(Data = subdataset[, 1:9], discrim = "ULI", k = 5, l = 2, u = 5, thr = 0.1)
#Why it is negative??? Interpretation of that

#ICC #####################################################################################

# Create a function to calculate the proportion of correct answers for each item grouped by total score
calculate_proportion <- function(subdataset, total_score_column) {
  item_names <- colnames(subdataset)[-which(names(subdataset) == total_score_column)] # Exclude total score
  proportions <- data.frame()
  
  for (item in item_names) {
    # Create a data frame with total score and the item response
    data <- data.frame(total_score = subdataset[[total_score_column]], item_response = subdataset[[item]])
    
    # Group by total score and calculate the mean (proportion of correct answers) for each group
    item_proportion <- data %>%
      group_by(total_score) %>%
      summarize(proportion_correct = mean(item_response, na.rm = TRUE))
    
    # Add item name
    item_proportion$item <- item
    proportions <- rbind(proportions, item_proportion)
  }
  
  return(proportions)
}

# Calculate proportions for each item
proportions_data <- calculate_proportion(subdataset, "BFI_total")

# Plot the ICCs using ggplot2
ggplot(proportions_data, aes(x = total_score, y = proportion_correct, color = item)) +
  geom_point() +
  geom_line() +
  labs(
    title = "Item Characteristic Curves (ICCs)",
    x = "Total Score",
    y = "Proportion of Correct Answers"
  ) +
  theme_minimal() +
  theme(legend.title = element_blank())

#for normalizing we could use probably the pi's above
########################################################################################
#Distractors#######################################


#######################################################################################
#Realibity

psych::alpha(x = subdataset[,1:9])$total[1]
psych::alpha(x = subdataset[,1:9])$alpha.drop[, 1]

#TODO: do it better -> incorrect
ItemAnalysis(Data = subdataset[,1:9])$Alpha.drop

#########################################################################################
#Item Validity
ItemAnalysis(Data = subdataset[,1:9], criterion = ds_close$BFI_closeones_agreeableness)$Corr.criterion
DDplot(Data = subdataset[,1:9], criterion = ds_close$BFI_closeones_agreeableness, thr = NULL)

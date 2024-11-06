# Exercise E.1.2 The LSAT7 dataset contains 5 dichotomously scored items obtained from
# the Law School Admissions Test, section 7 (Bock & Lieberman, 1970). Upload the LSAT7
# dataset into the ShinyItemAnalysis application and explore it. The csv file is available in
# the datasets directory on the book web page.
# • What are the mean and the standard deviation of the total scores?
#   • Calculate and interpret the Z-score and T-score for a respondent with a total score of 3
# points.
# • How many points did a respondent in the 69th percentile receive?

#   Exercise E.1.3 Create a short R script including the following tasks.
# • Upload data from Exercise E.1.2.
# • Compute the total scores for all students. Calculate mean, median, standard deviation,
# skewness, and kurtosis of the computed total scores.
# • Draw a histogram of the total scores.
# • Are the total scores approximately normally distributed? Why/Why not?
#   • Compute the Z-scores and T-scores for all respondents.

# Load necessary libraries
library(psych)      # for skewness and kurtosis
library(ggplot2)    # for histogram plotting

# Step 1: Load the LSAT7 dataset
LSAT7_data <- read.csv("data/LSAT7/LSAT7.csv")

summary(LSAT7_data)
tail(LSAT7_data)

# Step 2: Compute total scores for all respondents
# Assuming LSAT7_data has columns corresponding to the items (scored 0/1)
total_scores <- rowSums(LSAT7_data)

# Step 3: Calculate mean, median, standard deviation, skewness, and kurtosis
mean_score <- mean(total_scores)
median_score <- median(total_scores)
sd_score <- sd(total_scores)
skewness_score <- skew(total_scores) #left-tailed
kurtosis_score <- kurtosi(total_scores)

# Print the descriptive statistics
cat("Mean Score: ", mean_score, "\n")
cat("Median Score: ", median_score, "\n")
cat("Standard Deviation: ", sd_score, "\n")
cat("Skewness: ", skewness_score, "\n")
cat("Kurtosis: ", kurtosis_score, "\n")

# Step 4: Draw a histogram of the total scores
ggplot(data.frame(total_scores), aes(x = total_scores)) +
  geom_histogram(binwidth = 1, fill = "lightblue", color = "black") +
  labs(title = "Histogram of Total Scores", x = "Total Scores", y = "Frequency") +
  theme_minimal()

# Step 5: Check normality based on skewness, kurtosis, and histogram
if (abs(skewness_score) < 0.5 && abs(kurtosis_score - 3) < 0.5) {
  cat("The distribution is approximately normal.\n")
} else {
  cat("The distribution is not normal.\n")
}

# Step 6: Calculate Z-scores and T-scores for all respondents
z_scores <- scale(total_scores)  # Z-scores: (X - mean) / sd
t_scores <- 50 + 10 * z_scores   # T-scores: 50 + 10 * Z-score

# Step 7: For Exercise E.1.2:
# Calculate and interpret Z-score and T-score for a respondent with total score of 3
respondent_score <- 3
z_score_3 <- (respondent_score - mean_score) / sd_score
t_score_3 <- 50 + 10 * z_score_3
cat("Z-score for a respondent with total score of 3: ", z_score_3, "\n")
cat("T-score for a respondent with total score of 3: ", t_score_3, "\n") 
#A Z-score represents how many standard deviations a value is from the mean. 
#In this case, the Z-score of -0.59 indicates that a respondent who scored 3 on the test is 0.59 standard deviations below the mean score. 
#This means their performance is slightly below the average of all respondents

#6 points below the average


# Step 8: Percentile calculation - 69th percentile score
percentile_69th <- quantile(total_scores, 0.69)
cat("Score for the 69th percentile: ", percentile_69th, "\n")

#scored 4 on the test performed better than 69% of all test takers

#-------------------------------------------------------------------------------

data("CZmatura", package = "ShinyItemAnalysis")

View(CZmatura)
?CZmatura 

#matura exam in mathematics, 2019, cermat
#15 702 obs, 75 vars

#?? What is the difference in b1-b26


summary(CZmatura)
#86 % on first attempt
#significant amount of NA'S ->filled with 0?
#1-6 points question

hist(CZmatura$Total)
percentile69 <- quantile(CZmatura$Total, 0.69)





#ADD to the final code
# Load necessary libraries
library(dplyr)

# Assuming your dataset is named 'ds'

# Compute Z-scores and T-scores for the total scores



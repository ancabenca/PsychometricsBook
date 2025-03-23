#Project - supplementary R script
#AB-ZK
#Last update: 27.12.2024

################################################################################
################################################################################
# LIBRARIES AND TECHNICALLITIES

rm(list = ls())

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

library(ggplot2)
library(readxl)
library(dplyr)
library(tibble)
library(tidyr)
library(lmtest)
library(psych)
library(ggdendro)
library(ShinyItemAnalysis)
library(moments)
library(forecast)
library(gridExtra)
library(corrplot)
library(ggcorrplot)
library(psychometric)


library(foreign)
library(reshape2)
library(MASS)
library(Hmisc)
library(mirt)
library(VGAM)
library(msm)
library(deltaPlotR)
library(difR)
library(difNLR)
library(lordif)
################################################################################
################################################################################
#DATA LOADING AND DATA PREP

western_slovenia <- read_excel("Project\\Database-updated.xlsx", sheet = 1)  
eastern_slovenia <- read_excel("Project\\Database-updated.xlsx", sheet = 2)  

western_slovenia <- western_slovenia %>%
  mutate(Western = 1)
eastern_slovenia <- eastern_slovenia %>%
  mutate(Western = 0)

eastern_slovenia <- subset(eastern_slovenia, Code != "MZ31SG") #delete NA

ds_load <- bind_rows(western_slovenia, eastern_slovenia)

ds_load <- ds_load %>% rename(Year = `year of study`)
ds_load <- ds_load %>%
  mutate(YearNumber = case_when(
    Year == "2019/20" ~ 1,
    Year == "2020/21" ~ 2,
    Year == "2021/22" ~ 3,
    Year == "2022/23" ~ 4,
    Year == "2023/24" ~ 5
  ))

# Two questionnares separation
ds_og <- ds_load[,c(1:46,91:95,101:105,111:112)]
ds_close <- ds_load[,c(1:2,47:90,96:100,106:112)]


################################################################################
#REVERSING VARIABLES

# List of reverse-coded items
reverse_items <- c(6, 21, 31, 2, 12, 27, 37, 8, 18, 23, 43, 9, 24, 34, 35, 41)


#A. For SELF dataset-----------------------------------------------------------
ds_rev <- ds_og

for (item in reverse_items) {
  column_name <- paste0("BFI_", item) # Column names are in the format "BFI_number"
  ds_rev[[column_name]] <- 6 - ds_rev[[column_name]]
}

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


#B. For CLOSE dataset-----------------------------------------------------------
ds_rev_close <- ds_close

for (item in reverse_items) {
  column_name <- paste0("BFI_closeones_", item)
  ds_rev_close[[column_name]] <- 6 - ds_rev_close[[column_name]]
}

ds_rev_close$BFI_closeones_extraversion <- rowSums(ds_rev_close[paste0("BFI_closeones_", extraversion_items)])
ds_rev_close$BFI_closeones_agreeableness <- rowSums(ds_rev_close[paste0("BFI_closeones_", agreeableness_items)])
ds_rev_close$BFI_closeones_conscientiousness <- rowSums(ds_rev_close[paste0("BFI_closeones_", conscientiousness_items)])
ds_rev_close$BFI_closeones_neuroticism <- rowSums(ds_rev_close[paste0("BFI_closeones_", neuroticism_items)])
ds_rev_close$BFI_closeones_openness <- rowSums(ds_rev_close[paste0("BFI_closeones_", openness_items)])

#-------------------------------------------------------------------------------
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

equivalence_regular <- check_trait_equivalence(ds1 = ds_rev_close, ds2 = ds_close, use_closeones = 0)
equivalence_closeones <- check_trait_equivalence(ds1 = ds_rev_close, ds2 = ds_close, use_closeones = 1)

#C. Combined in right order-----------------------------------------------------
# Get the column names of both datasets
cols_rev <- colnames(ds_rev)
cols_rev_close <- colnames(ds_rev_close)

# Find the common columns and exclude them from ds_rev_close
unique_cols_rev_close <- setdiff(cols_rev_close, cols_rev)

# Combine the datasets by including only the unique columns from ds_rev_close
ds_combined <- cbind(ds_rev, ds_rev_close[, unique_cols_rev_close])

# Reorder columns to match the original `ds_load`
ds_final_rev <- ds_combined[, colnames(ds_load)]

all(colnames(ds_final_rev) == colnames(ds_load)) # Should return TRUE

################################################################################
################################################################################
#BASIC OVERVIEW
ds <- ds_combined
head(ds)
tail(ds)
str(ds)
View(ds)
summary(ds)


# Descriptive statistics and skewness/kurtosis for both total scores (Table 1)
descriptive_stats <- tibble(
  Score_Type = c("BFI_agreeableness", "BFI_closeones_agreeableness"),
  Mean = c(mean(ds$BFI_agreeableness, na.rm = TRUE), mean(ds$BFI_closeones_agreeableness, na.rm = TRUE)),
  SD = c(sd(ds$BFI_agreeableness, na.rm = TRUE), sd(ds$BFI_closeones_agreeableness, na.rm = TRUE)),
  Min = c(min(ds$BFI_agreeableness, na.rm = TRUE), min(ds$BFI_closeones_agreeableness, na.rm = TRUE)),
  Max = c(max(ds$BFI_agreeableness, na.rm = TRUE), max(ds$BFI_closeones_agreeableness, na.rm = TRUE)),
  Skewness = c(skewness(ds$BFI_agreeableness, na.rm = TRUE), skewness(ds$BFI_closeones_agreeableness, na.rm = TRUE)),
  Kurtosis = c(kurtosis(ds$BFI_agreeableness, na.rm = TRUE), kurtosis(ds$BFI_closeones_agreeableness, na.rm = TRUE))
)

# List of score variables
score_vars <- c(
  "BFI_2", "BFI_7", "BFI_12", "BFI_17", "BFI_22", 
  "BFI_27", "BFI_32", "BFI_37", "BFI_42",
  "BFI_closeones_2", "BFI_closeones_7", "BFI_closeones_12", 
  "BFI_closeones_17", "BFI_closeones_22", "BFI_closeones_27", 
  "BFI_closeones_32", "BFI_closeones_37", "BFI_closeones_42"
)

# Descriptive statistics and skewness/kurtosis for Agreeableness items (Table 4)
descriptive_stats_items <- tibble(
  Score_Type = score_vars,
  Mean = sapply(score_vars, function(var) mean(ds[[var]], na.rm = TRUE)),
  SD = sapply(score_vars, function(var) sd(ds[[var]], na.rm = TRUE)),
  Min = sapply(score_vars, function(var) min(ds[[var]], na.rm = TRUE)),
  Max = sapply(score_vars, function(var) max(ds[[var]], na.rm = TRUE)),
  Skewness = sapply(score_vars, function(var) skewness(ds[[var]], na.rm = TRUE)),
  Kurtosis = sapply(score_vars, function(var) kurtosis(ds[[var]], na.rm = TRUE))
)

# Descriptive statistics for Western (Table 2)
descriptive_stats_western <- ds %>%
  group_by(Western) %>%
  dplyr::summarize(
    Score_Type = c("Ys", "Yc"),
    Mean = c(mean(BFI_agreeableness, na.rm = TRUE), 
             mean(BFI_closeones_agreeableness, na.rm = TRUE)),
    SD = c(sd(BFI_agreeableness, na.rm = TRUE), 
           sd(BFI_closeones_agreeableness, na.rm = TRUE)),
    Min = c(min(BFI_agreeableness, na.rm = TRUE), 
            min(BFI_closeones_agreeableness, na.rm = TRUE)),
    Max = c(max(BFI_agreeableness, na.rm = TRUE), 
            max(BFI_closeones_agreeableness, na.rm = TRUE)),
    Skewness = c(skewness(BFI_agreeableness, na.rm = TRUE), 
                 skewness(BFI_closeones_agreeableness, na.rm = TRUE)),
    Kurtosis = c(kurtosis(BFI_agreeableness, na.rm = TRUE), 
                 kurtosis(BFI_closeones_agreeableness, na.rm = TRUE))
  )

# Create descriptive statistics by Year (Table 3)
descriptive_stats_year <- ds %>%
  group_by(Year) %>%
  dplyr::summarize(
    Score_Type = c("Ys", "Yc"),
    Mean = c(mean(BFI_agreeableness, na.rm = TRUE), 
             mean(BFI_closeones_agreeableness, na.rm = TRUE)),
    SD = c(sd(BFI_agreeableness, na.rm = TRUE), 
           sd(BFI_closeones_agreeableness, na.rm = TRUE)),
    Min = c(min(BFI_agreeableness, na.rm = TRUE), 
            min(BFI_closeones_agreeableness, na.rm = TRUE)),
    Max = c(max(BFI_agreeableness, na.rm = TRUE), 
            max(BFI_closeones_agreeableness, na.rm = TRUE)),
    Skewness = c(skewness(BFI_agreeableness, na.rm = TRUE), 
                 skewness(BFI_closeones_agreeableness, na.rm = TRUE)),
    Kurtosis = c(kurtosis(BFI_agreeableness, na.rm = TRUE), 
                 kurtosis(BFI_closeones_agreeableness, na.rm = TRUE))
  )

# Z-scores by hand
ds_res <- ds %>%
  mutate(
    BFI_agreeableness_z = (BFI_agreeableness - mean(BFI_agreeableness, na.rm = TRUE)) / sd(BFI_agreeableness, na.rm = TRUE),
    BFI_agreeableness_t = BFI_agreeableness_z * 10 + 50,
    BFI_agreeableness_percentile = pnorm(BFI_agreeableness_z) * 100,
    
    BFI_closeones_agreeableness_z = (BFI_closeones_agreeableness - mean(BFI_closeones_agreeableness, na.rm = TRUE)) / sd(BFI_closeones_agreeableness, na.rm = TRUE),
    BFI_closeones_agreeableness_t = BFI_closeones_agreeableness_z * 10 + 50,
    BFI_closeones_agreeableness_percentile = pnorm(BFI_closeones_agreeableness_z) * 100
  )

z_t_percentile_table <- ds_res %>%
  dplyr::select(
    BFI_agreeableness_z,
    BFI_agreeableness_t,
    BFI_agreeableness_percentile,
    BFI_closeones_agreeableness_z,
    BFI_closeones_agreeableness_t,
    BFI_closeones_agreeableness_percentile
  )

first_respondent_table <- tibble(
  Score_Type = c("BFI_agreeableness", "BFI_closeones_agreeableness"),
  Z_Score = c(ds_res$BFI_agreeableness_z[1], ds_res$BFI_closeones_agreeableness_z[1]),
  T_Score = c(ds_res$BFI_agreeableness_t[1], ds_res$BFI_closeones_agreeableness_t[1]),
  Percentile = c(ds_res$BFI_agreeableness_percentile[1], ds_res$BFI_closeones_agreeableness_percentile[1]),
  Success_Rate = c(ds_res$BFI_agreeableness_success[1], ds_res$BFI_closeones_agreeableness_success[1])
)



#-------------------------------------------------------------------------------
#Histograms for Total Scores (Figure 1)
plot1 <- ggplot(ds, aes(x = BFI_agreeableness)) +
  geom_histogram(binwidth = 1, fill = "skyblue", color = "black") +
  labs(title = "Histogram of BFI Agreeableness", x = "BFI Agreeableness Score", y = "Frequency") +
  theme_minimal()


plot2 <- ggplot(ds, aes(x = BFI_closeones_agreeableness)) +
  geom_histogram(binwidth = 1, fill = "lightcoral", color = "black") +
  labs(title = "Histogram of BFI Closeones Agreeableness", x = "BFI Closeones Agreeableness Score", y = "Frequency") +
  theme_minimal()


combined_plot<- grid.arrange(plot1, plot2, ncol = 2)
ggsave("combined_histograms.png", plot = combined_plot, width = 12, height = 6, dpi = 300)


###########################################################################################
################################################################################
# VALIDITY

var1 <- ds$BFI_agreeableness
var2 <- ds$BFI_closeones_agreeableness

# Boxplots for two populations  (Figure 2)
boxplotsAGG <- ggplot(data.frame(
  score = c(var1, var2),
  group = factor(rep(c("Self", "Close"), 
                     times = c(length(var1), length(var2))),  # Use 'times' for correct repetitions
                 levels = c("Self", "Close"))),
  aes(x = group, y = score, fill = group)) +
  geom_boxplot() + 
  ylab("Agreeableness") + 
  xlab("") +
  theme_minimal() +
  scale_fill_manual(values = c("Self" = "lightblue", "Close" = "lightcoral"))  # Set custom colors



t.test(var1,var2, paired = TRUE) #rejecting HO
ggsave("boxplotAGG.png", plot = boxplotsAGG, width = 12, height = 6, dpi = 300)
#-------------------------------------------------------------------------------
# Differences in Total Scores for each trait

# Differences for each trait
diff_extraversion <- ds$BFI_extraversion - ds$BFI_closeones_extraversion
diff_agreeableness <- ds$BFI_agreeableness - ds$BFI_closeones_agreeableness
diff_conscientiousness <- ds$BFI_conscientiousness - ds$BFI_closeones_conscientiousness
diff_neuroticism <- ds$BFI_neuroticism - ds$BFI_closeones_neuroticism
diff_openness <- ds$BFI_openness - ds$BFI_closeones_openness

# Combine into a single data frame
combined_diffs <- data.frame(
  difference = c(
    diff_extraversion, 
    diff_agreeableness, 
    diff_conscientiousness, 
    diff_neuroticism, 
    diff_openness
  ),
  trait = factor(rep(c("Extraversion", "Agreeableness", "Conscientiousness", "Neuroticism", "Openness"),
                     times = c(length(diff_extraversion), 
                               length(diff_agreeableness), 
                               length(diff_conscientiousness), 
                               length(diff_neuroticism), 
                               length(diff_openness)))
  )
)

# Boxplot for the differences (Figure 3)
diffBP <- ggplot(combined_diffs, aes(x = trait, y = difference)) +
  geom_boxplot(fill = "lightcoral") +
  ylab("Difference (Self - Close Ones)") +
  xlab("Traits") +
  theme_minimal()
ggsave("diffBP.png", plot = diffBP, width = 12, height = 6, dpi = 300)


################################################################################
#Western and Year variable
#Self dataset----------------------------------------------------------------------
traits <- c("BFI_extraversion", "BFI_agreeableness", "BFI_conscientiousness", "BFI_neuroticism", "BFI_openness")
t_test_results <- list()
anova_results <- list()
for (trait in traits) {
  # Defining groups
  group1 <- ds[[trait]][ds$Western == 0]  # Scores where Western == 0
  group2 <- ds[[trait]][ds$Western != 0]  # Scores where Western != 0
  
  # Perform the two-sample t-test
  t_test_results[[trait]] <- t.test(group1, group2)
}

for (trait in traits) {
  # Perform ANOVA using the formula interface
  anova_results[[trait]] <- aov(ds[[trait]] ~ as.factor(ds$YearNumber), data = ds)
}


# Print the results for each trait
for (trait in traits) {
  cat("\nResults for", trait, ":\n")
  print(t_test_results[[trait]])
}
for (trait in traits) {
  cat("\nANOVA results for", trait, ":\n")
  print(summary(anova_results[[trait]]))
}

#Close-ones dataset----------------------------------------------------------------
traits2 <- c("BFI_closeones_extraversion", "BFI_closeones_agreeableness", "BFI_closeones_conscientiousness", "BFI_closeones_neuroticism", "BFI_closeones_openness")

t_test_results2 <- list()
anova_results2 <- list()

for (trait in traits2) {
  group1 <- ds[[trait]][ds$Western == 0]
  group2 <- ds[[trait]][ds$Western != 0]
  
  t_test_results2[[trait]] <- t.test(group1, group2)
}
for (trait in traits2) {
  anova_results2[[trait]] <- aov(ds[[trait]] ~ as.factor(ds$YearNumber), data = ds)
}


for (trait in traits2) {
  cat("\nResults for", trait, ":\n")
  print(t_test_results2[[trait]])
}
for (trait in traits2) {
  cat("\nANOVA results for", trait, ":\n")
  print(summary(anova_results2[[trait]]))
}
################################################################################
#Assumption verification

plot(ds$BFI_agreeableness)
plot(ds$BFI_closeones_agreeableness)
hist(ds$BFI_agreeableness)
hist(ds$BFI_closeones_agreeableness)
hist(ds$BFI_agreeableness + ds$BFI_closeones_agreeableness) #Normality A+B

shapiro.test(ds$BFI_agreeableness) #Normality
var.test(ds$BFI_agreeableness,ds$BFI_closeones_agreeableness) #Equal Variance
cor.test(ds$BFI_agreeableness, ds$BFI_closeones_agreeableness)
################################################################################
#Linear Regression

lm.model1 <- lm(BFI_agreeableness ~ BFI_extraversion + BFI_conscientiousness + BFI_neuroticism +  BFI_openness, data = ds_rev)
summary(lm.model1)
drop1(lm.model1, test ="Chisq")
plot(lm.model1) #Diagnostics - checking for normality of residuals, autocorrelation, homoscedascty and zero conditional mean of res

#-----------------
lm.model1.1 <- lm(BFI_agreeableness ~ BFI_conscientiousness + BFI_neuroticism, data = ds_rev)
summary(lm.model1.1)
drop1(lm.model1.1, test ="Chisq") 

plot(lm.model1.1)

anova(lm.model1, lm.model1.1) #IThe null hypothesis states that the simpler model is sufficient, meaning the parameters omitted in the submodel do not significantly improve the model.

#-----------------
lm.model1.2 <- lm(BFI_agreeableness ~BFI_neuroticism, data = ds_rev)
summary(lm.model1.2) #(Table 5)
drop1(lm.model1.2, test ="Chisq") #more accurate than Wald from summary()

anova(lm.model1.1, lm.model1.2) #can be simplified
plot(lm.model1.2)

#Low R squared -> poor fit

residuals <- resid(lm.model1.2)
fitted_values <- fitted(lm.model1.2)
squared_residuals <- residuals^2

#Diagnostics plot (Figure 4)
# 1. Residuals vs. Fitted Plot
residuals_vs_fitted <- ggplot(data = data.frame(Fitted = fitted_values, Residuals = residuals), 
                              aes(x = Fitted, y = Residuals)) +
  geom_point() +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  labs(title = "Residuals vs Fitted Values", x = "Fitted Values", y = "Residuals") +
  theme_minimal()

# 2. QQ Plot
qq_plot <- ggplot(data = data.frame(Residuals = residuals), aes(sample = Residuals)) +
  stat_qq() +
  stat_qq_line(col = "red") +
  labs(title = "QQ Plot of Residuals", x = "Theoretical Quantiles", y = "Sample Quantiles") +
  theme_minimal()

# 3. Autocorrelation Plot
acf_data <- acf(residuals, plot = FALSE)  # Get ACF values
acf_df <- data.frame(lag = acf_data$lag, acf = acf_data$acf)

autocorrelation_plot <- ggplot(acf_df, aes(x = lag, y = acf)) +
  geom_segment(aes(xend = lag, yend = 0)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  labs(title = "Autocorrelation of Residuals", x = "Lag", y = "ACF") +
  theme_minimal()

# 4. Scale-Location Plot
scale_location_plot <- ggplot(data = data.frame(Fitted = fitted_values, Residuals = residuals), 
                              aes(x = Fitted, y = sqrt(abs(Residuals)))) +
  geom_point() +
  labs(title = "Scale-Location Plot", x = "Fitted Values", y = "Square Root of |Residuals|") +
  theme_minimal()


combined_plots3 <- grid.arrange(residuals_vs_fitted, qq_plot, autocorrelation_plot, scale_location_plot, ncol = 2)
ggsave("model_diagnostics.png", plot = combined_plots3, width = 12, height = 8)

################################################################################
#Correlation

#A. Pearson---------------------------------------------------------------------
selected_columns <- c(
  "BFI_2", "BFI_7", "BFI_12", "BFI_17", "BFI_22", 
  "BFI_27", "BFI_32", "BFI_37", "BFI_42",
  "BFI_closeones_2", "BFI_closeones_7", "BFI_closeones_12", 
  "BFI_closeones_17", "BFI_closeones_22", "BFI_closeones_27", 
  "BFI_closeones_32", "BFI_closeones_37", "BFI_closeones_42",
  "BFI_agreeableness", "BFI_closeones_agreeableness"
  
)
correlation_matrix <- cor(ds[, selected_columns], use = "complete.obs")
plot_labels <- c(
  "Z2", "Z7", "Z12", "Z17", "Z22", 
  "Z27", "Z32", "Z37", "Z42",
  "Zc2", "Zc7", "Zc12", 
  "Zc17", "Zc22", "Zc27", 
  "Zc32", "Zc37", "Zc42",
  "Ys", "Yc"
)

colnames(correlation_matrix) <- plot_labels
rownames(correlation_matrix) <- plot_labels

# Ggplot-based correlation plot (Figure 5)
corTESTscores <- ggcorrplot(
  correlation_matrix,
  hc.order = FALSE,                    # Hierarchically cluster to show stronger correlations together
  type = "lower",                     # Show only lower triangle
  lab = TRUE,                         # Display correlation coefficients in the plot
  lab_size = 3,                       # Set the size of correlation labels
  colors = c("red", "white", "blue"), # Set color scale
  legend.title = "Correlation"
) +
  theme_minimal() +                   # Use a clean theme for better readability
  theme(
    plot.title = element_text(hjust = 0.5) # Center the title
  )

ggsave("corTESTscores.png", plot = corTESTscores, width = 12, height = 6, dpi = 300)


#B. Polychoric-----------------------------------------------------------------
#Polychoric 
corFull <- polychoric(x = ds[,3:90])$rho #Not working -> does not register as a suitable dataset (bug in the function?)

#Create a subdataset for Self dataset
subdataset <- ds_rev[, c("BFI_2", "BFI_7", "BFI_12", "BFI_17", 
                         "BFI_22", "BFI_27", "BFI_32", "BFI_37", 
                         "BFI_42")]
subdataset2 <- ds_rev[, c("BFI_2", "BFI_7", "BFI_12", "BFI_17", 
                          "BFI_22", "BFI_27", "BFI_32", "BFI_37", 
                          "BFI_42")] #additional subdataset for testing

labels <- c("Z2", "Z7", "Z12", "Z17", "Z22", "Z27", "Z32", "Z37", "Z42")

rownames(subdataset2) <- labels
colnames(subdataset2) <- labels



subdataset[] <- lapply(subdataset, function(x) {
  if (is.numeric(x)) {
    factor(x, levels = 1:5, ordered = TRUE)
  } else {
    x
  }
}) 
corsub <- polychoric(as.data.frame(subdataset))
corsubrho <- corsub$rho
rownames(corsubrho) <- labels
colnames(corsubrho) <- labels

#Difference betweem the normal and polychoric (Table 6)
correlation_matrix["Z2",1:9] - corsubrho["Z2",]
correlation_matrix[1:9,1:9] - corsubrho

hc <- hclust(d = as.dist(1 - corsubrho), method = "ward.D2") # Clustering


# Polychoric correlation plot with dendrogram (Figure 6)
polychoricPlot <- ggcorrplot(
  corsubrho,
  hc.order = FALSE,                     # Do not hierarchically cluster
  type = "lower",                       # Show only lower triangle
  lab = TRUE,                           # Display correlation coefficients in the plot
  lab_size = 3,                         # Set the size of correlation labels
  colors = c("red", "white", "blue"),  # Set color scale
  legend.title = "Correlation"
) +
  theme_minimal() +                   
  theme(
    plot.title = element_text(hjust = 0.5),  # Center the title
    legend.position = "bottom",                # Move legend to bottom
    axis.title.x = element_blank(),            # Remove x-axis title
    axis.title.y = element_blank()           # Remove y-axis title          
  )

# Dendrogram plot without x, y axis labels
ddgram <- ggdendrogram(hc, rotate = TRUE) +  # Rotate the dendrogram if desired
  theme_minimal() +
  theme(  
    axis.title.y = element_blank(),
    axis.title.x = element_blank(),# Remove y-axis title
    plot.margin = margin(10, 10, 10, 10)    # Adjust margins
  ) +
  geom_segment(linewidth = 1) 

polychoricPlot2 <- grid.arrange(
  polychoricPlot,
  ddgram,
  ncol = 2  # Adjust height ratios if needed
)
ggsave("corTESTpoly.png", plot = polychoricPlot2, width = 12, height = 6, dpi = 300)


################################################################################
#Factor Analysis
FA1 <- fa(r = corsubrho, nfactors = 1, fm = "ml")
summary(FA1) 

#FA for polychoric
FA2 <- fa(subdataset2, nfactors = 2, cor = "polychoric") #Table 7, Table 8
FA3 <- fa(subdataset2, nfactors = 3, cor = "polychoric",rotate = "none")
FA4 <- fa(subdataset2, nfactors = 4, cor = "polychoric",rotate = "none")

subdataset2_no_Z32 <- subdataset2[, !names(subdataset2) %in% "Z32"]
FA2_no_Z32 <- fa(subdataset2_no_Z32, nfactors = 2, cor = "polychoric")

################################################################################
#PCA Analysis
pca_result <- principal(corsubrho, nfactors = ncol(subdataset2), rotate = "none")
plot(pca_result$values, type = "b", xlab = "Principal Component", ylab = "Eigenvalue", main = "Scree Plot")

explained_variance <- pca_result$values / sum(pca_result$values)
variance_df <- data.frame(
  Principal_Component = seq_along(explained_variance),
  Variance_Explained = explained_variance
)
variance_df$Principal_Component <- factor(variance_df$Principal_Component)

loadings_df <- data.frame(
  PC1 = pca_result$loadings[, 1],
  PC2 = pca_result$loadings[, 2],
  Variable = rownames(pca_result$loadings)
)
# Loadings and Screeplot (Figure 7)
screeplot <- ggplot(variance_df, aes(x = Principal_Component, y = Variance_Explained)) +
  geom_bar(stat = "identity", fill = "lightblue") +
  labs(
    x = "Principal Component",
    y = "Proportion of Variance Explained"
  ) +
  theme_minimal() +
  scale_x_discrete(labels = paste0("PC", 1:nrow(variance_df)))


loadings <- ggplot(loadings_df, aes(x = PC1, y = PC2, label = Variable)) +
  geom_point(color = "lightcoral", size = 3) +                     # Points in blue
  geom_text(hjust = 0.5, vjust = -0.5, color = "blue") +  # Labels in dark red
  labs(x = "PC1 Loadings", y = "PC2 Loadings") +
  theme_minimal()   

screeplotlod<- grid.arrange(screeplot, loadings, ncol = 2)

ggsave("screelodplot.png", plot = screeplotlod, width = 12, height = 6, dpi = 300)


#################################################################################
#################################################################################
#RELIABILITY

# SB formula--------------------------------------------------------------------
rho_original <- 0.637# reliability of original data
items_original <- 9 # number of items in original data
items_new <- 18 # number of items in new data
(m <- items_new / items_original) # ratio of tests lengths

m * rho_original / (1 + (m - 1) * rho_original)
SBrel(Nlength = m, rxx = rho_original) # Alternative

rho_new <- 0.90 # desired reliability
(m <- rho_new * (1 - rho_original) / (rho_original * (1 - rho_new)))
ceiling(m * items_original) # new test length

#------------------------------------------------------------------------------
#Plotting the items (Figure 8)
items_new_seq <- seq(1, 60, by = 1)
reliability_values <- sapply(items_new_seq, function(items_new) {
  m <- items_new / items_original # ratio of test lengths
  m * rho_original / (1 + (m - 1) * rho_original)
})

data <- data.frame(
  items_new = items_new_seq,
  reliability = reliability_values
)

reliability <- ggplot(data, aes(x = items_new, y = reliability)) +
  geom_line(color = "lightblue") +
  geom_point(color = "blue") +
  geom_hline(yintercept = 1, linetype = "dashed", color = "coral") +
  geom_hline(yintercept = 0.9, linetype = "dashed", color = "purple") +
  geom_hline(yintercept = 0.64, linetype = "dashed", color = "black") +
  annotate("text", x = 48, y = 1.03, label = "Perfect Correlation", color = "coral", hjust = 1.2, size = 5) +
  annotate("text", x = 48, y = 0.93, label = "Desired Correlation", color = "purple", hjust = 1.2, size = 5) +
  annotate("text", x = 48, y = 0.67, label = "Current Correlation", color = "black", hjust = 1.2, size = 5) +
  labs(
    x = "Number of Items",
    y = "Reliability (correlation)"
  ) +
  theme_minimal() +
  theme(
    axis.title.x = element_text(size = 14),  # Increase x-axis label size
    axis.title.y = element_text(size = 14)   # Increase y-axis label size
  )


ggsave("reliability.png", plot = reliability, width = 12, height = 6, dpi = 300)

###############################################################################
# Split testing (Table 9)
subdataset7 <- subdataset2[-7,] #Needs even number

#------------------------------------------------------------------------
#Random
set.seed(123)
samp <- sample(1:8, 4) # choose 4 random items for df1
df1 <- subdataset7[, samp]                 # df1 with 4 random columns
df2 <- subdataset7[, setdiff(1:8, samp)]   # df2 with the remaining 4 columns

#--------------------------------------------------------------------
#first half
df1 <- subdataset7[, 1:4]; df2 <- subdataset7[, 5:8]
#----------------------------------------------------------------------
#even-odd
df1 <- subdataset7[, seq(1, 8, 2)]; df2 <- subdataset7[, seq(2, 8, 2)]
#-------------------------------------------------------------------------

# calculating total scores for subdatasets
ts1 <- rowSums(df1)
ts2 <- rowSums(df2)
# calculating correlation between total scores of df1 and df2
cor_x <- cor(ts1, ts2)
# Spearman-Brown formula to estimate reliability
reliability <- 2 * cor_x / (1 + cor_x)
reliability


###############################################################################
# Cronbach alpha
psych::alpha(subdataset2,check.keys = TRUE) #Table 10
psych::alpha(subdataset2)$total[1]
ItemAnalysis(Data = subdataset2[,1:9])$Alpha.drop
################################################################################
################################################################################
#TRADITIONAL ITEM ANALYSIS

# Basic Overview----------------------------------------------------------------
subdataset_close <- ds_rev_close[, c("BFI_closeones_2", "BFI_closeones_7", "BFI_closeones_12", 
                                     "BFI_closeones_17", "BFI_closeones_22", "BFI_closeones_27", 
                                     "BFI_closeones_32","BFI_closeones_37", "BFI_closeones_42")]

colnames(subdataset_close) <- c("Zc2", "Zc7", "Zc12", "Zc17", 
                                "Zc22", "Zc27", "Zc32", "Zc37", 
                                "Zc42")

stats <- data.frame(
  Item = names(subdataset2),
  Mean = sapply(subdataset2, mean),
  SD = sapply(subdataset2, sd),
  Group = "Self"
)
stats_close <- data.frame(
  Item = names(subdataset_close),
  Mean = sapply(subdataset_close, mean),
  SD = sapply(subdataset_close, sd),
  Group = "Close"
)
combined_stats <- rbind(stats, stats_close)

# Confidence intervals of relevant items (Figure 9)
itemBasic <- ggplot(combined_stats, aes(x = Item, y = Mean, color = Group, shape = Group)) +
  geom_point(size = 3) +  # Plot means
  geom_errorbar(aes(ymin = Mean - SD, ymax = Mean + SD), width = 0.2) +  # Add error bars
  labs(
    x = "Items",
    y = "Mean with Confidence Interval"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_color_manual(values = c("blue", "red"))  # Custom colors

ggsave("itemsBasic.png", plot = itemBasic, width = 12, height = 6, dpi = 300)

difference_stats <- data.frame(
  Item = names(subdataset2[1:9]),
  MeanDifference = sapply(subdataset2[1:9], mean) - sapply(subdataset_close[1:9], mean),
  SDDifference = sqrt(sapply(subdataset2[1:9], sd)^2 + sapply(subdataset_close[1:9], sd)^2)  # Variance sum rule
)

# CI for differences of two datasets (Figure 10)
itemdif <- ggplot(difference_stats, aes(x = Item, y = MeanDifference)) +
  geom_point(size = 3, color = "blue") +  # Plot mean differences
  geom_errorbar(aes(ymin = MeanDifference - SDDifference, ymax = MeanDifference + SDDifference), 
                width = 0.2, color = "black") +  # Add error bars
  labs(
    x = "Items",
    y = "Difference (Self - Close)"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


ggsave("itemsdif.png", plot = itemdif, width = 12, height = 6, dpi = 300)

#################################################################################
# Item difficulty

# Table 11
# minmax -----------------------------------------------------------------------
# Define the function to calculate πˆi
calculate_norm <- function(column) {
  mean_col <- mean(column)            # Average item score Ȳ•i
  min_col <- min(column)              # Minimum possible score min(Yi)
  max_col <- max(column)              # Maximum possible score max(Yi)
  (mean_col - min_col) / (max_col - min_col)  # Scaled score
}

pi_hat <- sapply(subdataset2, calculate_norm)
pi_hat[1:9]

# max --------------------------------------------------------------------------
calculate_pi_max <- function(column) {
  max_col <- max(column)  # Maximal possible score
  mean(column == max_col) # Proportion of respondents with max score
}
pi_hat_max <- sapply(subdataset, calculate_pi_max)

# cutoff -----------------------------------------------------------------------
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
cutoffs_list <- c(3,4)
result_df <- calculate_pi_cutoffs(subdataset2, cutoffs_list)

# Neutral Counts-----------------------------------------------------------------
#Table 12
neutral_counts <- sapply(subdataset, function(column) sum(column == 3, na.rm = TRUE))
neutral_counts_df <- data.frame(Item = names(neutral_counts), Neutral_Count = neutral_counts)
total_respondents <- nrow(subdataset)
neutral_counts_df$Proportion_Neutral <- neutral_counts_df$Neutral_Count / total_respondents


#################################################################################
# Item discrimination
#Table 13
ItemAnalysis(subdataset2[,1:9])$RIT
ItemAnalysis(subdataset2[,1:9])$RIR
gDiscrim(Data = subdataset2[, 1:9]) # ULI
gDiscrim(Data = subdataset2[, 1:9], k = 5, l = 2,u = 5) #ULI*


# Plotting distribution of the three groups
subdataset2$partTotal <-rowSums(subdataset2[, c("Z2", "Z7", "Z12", "Z17", "Z22", "Z27", "Z32", "Z37", "Z42")])

subdataset_plot2<- subdataset2 %>%
  mutate(
    ULI_group = case_when(
      partTotal <= quantile(partTotal, 1/3) ~ "blue",   # Group 1: Blue
      partTotal <= quantile(partTotal, 2/3) ~ "white",  # Group 2: White
      TRUE ~ "red"                                     # Group 3: Red
    )
  )

subdataset_plot <- subdataset2 %>%
  mutate(score_group = ntile(partTotal, 3)) 

group_means <- subdataset_plot %>%
  group_by(score_group) %>%
  dplyr::summarize(mean_Z2 = mean(Z2, na.rm = TRUE))


pic1 <- ggplot(group_means, aes(x = score_group, y = mean_Z2)) +
  geom_point(color = "red", size = 3) +
  geom_line(color = "red") +
  geom_text(aes(label = round(mean_Z2, 2)), vjust = -0.5) +
  labs(
    x = "Group by total score",
    y = "Option selection proportion (Z2)"
  ) +
  theme_minimal()

pic2 <- ggplot(subdataset_plot2, aes(x = partTotal, fill = ULI_group)) +
  geom_histogram(binwidth = 1, color = "black", position = "identity") + 
  scale_fill_identity() +   # Use the colors already defined in `group_color`
  labs(
    x = "Total score",
    y = "Number of respondents") +
  theme_minimal()

ddplotHist <- grid.arrange(pic2, pic1, ncol = 2) # Figure 11
ggsave("ddplotHist.png", plot = ddplotHist, width = 12, height = 6, dpi = 300)


#----------------

DDplot(Data = subdataset2[, 1:9], discrim = "ULI") # Figure 12
#################################################################################
#Dichotomization

subdataset_dichotomized <- subdataset2[ , 1:9] %>%
  mutate(across(everything(), ~ case_when(
    . %in% c(1, 2, 3) ~ 0,   # Values 1, 2, 3 become 0
    . %in% c(4, 5) ~ 1,      # Values 4, 5 become 1
    TRUE ~ .                 # Keep other values unchanged (if any)
  )))
subdataset_dichotomized$total_score <- rowSums(subdataset_dichotomized)


# Compute proportions for each item and ICCs

proportions_data <- subdataset_dichotomized %>%
  tidyr::pivot_longer(cols = c(Z7, Z17), names_to = "item", values_to = "response") %>%
  group_by(total_score, item) %>%
  dplyr::summarize(
    proportion_correct = mean(response, na.rm = TRUE, .groups = "drop")
  )

#Figure 13 - ICCs
iccs <- ggplot(proportions_data, aes(x = total_score, y = proportion_correct, color = item)) +
  geom_point() +
  geom_line() +
  labs(
    x = "Total Score of Dichotomized Agreeableness Related Items",
    y = "Proportion of Correct Answers"
  ) +
  theme_minimal() +
  theme(legend.title = element_blank())

ggsave("iccs.png", plot = iccs, width = 12, height = 6, dpi = 300)
#-----------------------------------------------------------------------
#All items 
proportions_data2 <- subdataset_dichotomized %>%
  tidyr::pivot_longer(cols = c(Z2, Z7, Z12, Z17, Z22, Z27, Z32, Z37, Z42), names_to = "item", values_to = "response") %>%
  group_by(total_score, item) %>%
  dplyr::summarize(
    proportion_correct = mean(response, na.rm = TRUE, .groups = "drop")
  )
iccs2 <- ggplot(proportions_data2, aes(x = total_score, y = proportion_correct, color = item)) +
  geom_point() +
  geom_line() +
  labs(
    x = "Total Score of Dichotomized Agreeableness Related Items",
    y = "Proportion of Correct Answers"
  ) +
  theme_minimal() +
  theme(legend.title = element_blank())

################################################################################
#Ordinal logistic regression

#Standardized total score
mean_total <- mean(ds_rev$BFI_agreeableness, na.rm = TRUE)
sd_total <- sd(ds_rev$BFI_agreeableness, na.rm = TRUE)
ds_rev$BFI_agreeableness_z <- (ds_rev$BFI_agreeableness - mean_total) / sd_total
zscore <- ds_rev$BFI_agreeableness_z
A_items <- subdataset2[1:9] - 1
labels <- colnames(A_items)  # Or define it accordingly
for (label in labels) {
  A_items[[label]] <- ordered(factor(A_items[[label]], 
                                     levels = 0:max(A_items[[label]], na.rm = TRUE)))
}

# Cumulative---------------------------------------------------------------------------
model_fits <- list()
# Model for each item
for (label in labels) {
  # Fit the cumulative model for the current item
  model_fits[[label]] <- vglm(A_items[[label]] ~ zscore, 
                              family = cumulative(reverse = TRUE, parallel = TRUE)) #  parallel = TRUE implies the proportional odds assumption, meaning the effect of predictors (coefficients for zscore) is constant across all thresholds.
  
  # Optionally, print a summary of the fit
  cat("\nSummary for:", label, "\n")
  print(summary(model_fits[[label]]))
}

# Choosen models to fit
m7 <- model_fits[['Z7']]
m17 <- model_fits[['Z17']]


summary(m7) # Table 15
summary(m17) # Table 16
lrtest_vglm(m7)
lrtest_vglm(m17)

#IRT parametrization
model <- m17 #m7
c(-coef(model)[1:4] / coef(model)[5], coef(model)[5])
deltamethod(list(~ -x1 / x5, ~ -x2 / x5, ~ -x3 / x5, ~ -x4 / x5, ~x5),
            mean = coef(model), cov = vcov(model))

#Table 14
count_answers <- table(subdataset2$Z7)
count_answers2 <- table(subdataset2$Z17)

#Plotting
cumu17 <- plotCumulative(m17, type = "cumulative",matching.name = "Standardized Total Score, item Z17") #Figure 15
cumu7 <- plotCumulative(m7, type = "cumulative",matching.name = "Standardized Total Score, item Z7") #Figure 14

cumucat17 <- plotCumulative(m17, type = "category", matching.name = "Standardized Total Score, item Z17") #Figure 15
cumucat7 <- plotCumulative(m7, type = "category", matching.name = "Standardized Total Score, item Z7") #Figure 14
#not gridArrange nor ggsave  have worked, exported by hand


# Adjacent -----------------------------------------------------------------------------
model_fits_a <- list()

for (label in labels) {
  # Fit the cumulative model for the current item
  model_fits_a[[label]] <- vglm(A_items[[label]] ~ zscore, 
                                family = acat(reverse = FALSE, parallel = TRUE))
  
  # Optionally, print a summary of the fit
  cat("\nSummary for:", label, "\n")
  print(summary(model_fits_a[[label]]))
}


m12_a <- model_fits_a[['Z12']]
c(-coef(m12_a)[1:4] / coef(m12_a)[5], coef(m12_a)[5])
deltamethod(list(~ -x1 / x5, ~ -x2 / x5, ~ -x3 / x5, ~ -x4 / x5, ~x5),
            mean = coef(m12_a), cov = vcov(m12_a))


plotAdjacent(m12_a, matching.name = "Z-score")

################################################################################
################################################################################
#IRT MODELS

# A. GRM------------------------------------------------------------------------
# Convert the matrix of ordered factors (A_items) to numeric
A_items_numeric <- as.data.frame(lapply(A_items, function(x) as.numeric(as.character(x))))

fit.grm.mirt <- mirt(A_items_numeric, model = 1, itemtype = 'graded', SE = TRUE) 
fit.grm.mirt_2F <- mirt(A_items_numeric, model = 2, itemtype = 'graded', SE = TRUE) 

summary(fit.grm.mirt) #Table 17, 
summary(fit.grm.mirt_2F)
anova(fit.grm.mirt,fit.grm.mirt_2F)

coef(fit.grm.mirt, simplify = TRUE) 
coef(fit.grm.mirt, IRTpars = TRUE, simplify = TRUE)#Table 18

response_counts <- apply(A_items, 2, function(x) table(factor(x, levels = 0:4))) # Table 19

#Plotting ----------------------------------------------------------------------
plot(fit.grm.mirt, type = 'infotrace') # Figure 16
plot(fit.grm.mirt, type = 'trace') # Figure 17


itemplot(fit.grm.mirt, item = 'Z17', type = "trace")
itemplot(fit.grm.mirt, item = 'Z17', type = "info")
itemplot(fit.grm.mirt, item = 'Z17', type = "score")
itemplot(fit.grm.mirt, item = 'Z17', type = "infoSE")

plot1 <- itemplot(fit.grm.mirt, item = 'Z17', type = "trace")
plot2 <- itemplot(fit.grm.mirt, item = 'Z17', type = "score")
plot3 <- itemplot(fit.grm.mirt, item = 'Z17', type = "infotrace") 
plot4 <- itemplot(fit.grm.mirt, item = 'Z17', type = "infoSE")

griditems <- grid.arrange(plot2, plot4, ncol = 2) # Figure 18
ggsave("griditems.png",griditems,  width = 12, height = 6)

plot5 <- plot(fit.grm.mirt, MI = 200)
plot6 <- plot(fit.grm.mirt, type = "infoSE")
gridTotal <- grid.arrange(plot5, plot6, ncol = 2) # Figure 20
ggsave("gridTotal.png",gridTotal, width = 12, height = 6)

fSE <- fscores(fit.grm.mirt, full.scores.SE = TRUE) 
fSE <- as.data.frame(fSE)
#-------------------------------------------------------------------------------
db <- data.frame(
  F1 = fSE$F1, 
  SE = fSE$SE_F1, 
  numbers = 1:nrow(fSE),
  Lower =  fSE$F1 - 1.96 * fSE$SE_F1,
  Upper =  fSE$F1 + 1.96 * fSE$SE_F1
)

f1scores <-  ggplot(db, aes(x = reorder(numbers, F1), y = F1)) +
  geom_point(size = 3, color = "red") +
  geom_errorbar(aes(ymin = Lower, ymax = Upper), width = 0.2, color = "red") +
  labs(
    title = "Sorted F1 Scores with Confidence Intervals",
    x = "Index",
    y = "F1 Score"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 90, hjust = 1, size = 5) 
  )

ggsave("f1scores.png", plot = f1scores, width = 12, height = 6, dpi = 300) # Figure 19

# B. GRSM (not in the report)-----------------------------------------------------------------------
A_items_num_merge <- A_items_numeric

# Using apply to modify all columns
A_items_num_merge <- apply(A_items_num_merge, 2, function(x) ifelse(x == 0, 1, x))

A_items_num_merge <- as.data.frame(A_items_num_merge)
A_items_num_merge <- A_items_num_merge - 1

fit.GRSMirt.mirt <- mirt(A_items_num_merge, model = 1, itemtype = "grsmIRT")
fit.grm.mirt2 <- mirt(A_items_num_merge, model = 1, itemtype = 'graded', SE = TRUE, dentype = 'EH') #different density

coef(fit.grm.mirt2, IRTpars = TRUE, simplify = TRUE)
coef(fit.GRSMirt.mirt, simplify = TRUE)
anova(fit.GRSMirt.mirt, fit.grm.mirt2)

itemplot(fit.GRSMirt.mirt, item = 'Z17', type = "trace")
itemplot(fit.GRSMirt.mirt, item = 'Z17', type = "score")
itemplot(fit.GRSMirt.mirt, item = 'Z17', type = "info")
itemplot(fit.GRSMirt.mirt,  item = 'Z17', type = "infotrace")
itemplot(fit.GRSMirt.mirt, item = 'Z17', type = "infoSE")


plot(fit.GRSMirt.mirt, type = 'trace')
plot(fit.GRSMirt.mirt, type = "infoSE")

# Relationship Between IRT Ability Estimates and Standardized Scores (Figure 21)
fSE_values <- fSE[, 1]
data <- data.frame(IRT_ability = fSE_values, Standardized_Score = zscore)
realIRTZ <- ggplot(data, aes(x = IRT_ability, y = Standardized_Score)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "red") +  # Line of best fit
  labs(x = "IRT Ability Estimates (θ^)",
       y = "Standardized Total Scores (Z)") +
  theme_minimal()
ggsave("realIRTZ.png", plot = realIRTZ, width = 12, height = 6, dpi = 300)



#C. Dichom rasch (not in the report)----------------------------------------------------------------
fit_rasch_mirt <- mirt(data = subdataset_dichotomized[,-10], model = 1, itemtype = "Rasch", 
                       SE = TRUE)
coef(fit_rasch_mirt, simplify = TRUE)
coef(fit_rasch_mirt, printSE = TRUE)
coef(fit_rasch_mirt, IRTpars = TRUE, simplify = TRUE)
summary(fit_rasch_mirt)

# TSC (test score curve)
plot(fit_rasch_mirt)

# ICC
plot(fit_rasch_mirt, type = "trace", facet_items = FALSE)

# Further plots (not displayed in the book)
plot(fit_rasch_mirt, type = "trace", facet_items = TRUE) # ICC separately
plot(fit_rasch_mirt, type = "infotrace", facet_items = FALSE) # Item information curves (IIC)
plot(fit_rasch_mirt, type = "infotrace", facet_items = TRUE) # IICs separately
plot(fit_rasch_mirt, type = "info", facet_items = FALSE) # Test information curve (TIC)
plot(fit_rasch_mirt, type = "infoSE", facet_items = FALSE) # TIC and SE

# Latent abilities (factor scores) with SE
fs_rasch_mirt_SE <- fscores(fit_rasch_mirt, full.scores.SE = TRUE)
head(fs_rasch_mirt_SE, n = 3)

fit_2PL_mirt <- mirt(data = subdataset_dichotomized[,-10], model = 1, itemtype = "2PL")


################################################################################
################################################################################
#DIF

subdataset_dichotomizedDIF <- subdataset_dichotomized
subdataset_dichotomizedDIF$Western <- ds_rev$Western
subdataset_dichotomizedDIF <- subdataset_dichotomizedDIF[, !(names(subdataset_dichotomizedDIF) == "total_score")]

# Not in the report
counts_ones <- subdataset_dichotomizedDIF %>%
  group_by(Western) %>%
  summarise(across(everything(), ~ sum(. == 1, na.rm = TRUE), .names = "count_{col}"))
table(subdataset_dichotomizedDIF$Western)

# Contigency table (Table 20)
total_score_value <- 3
subdataset_dichotomizedDIF$total_score <- rowSums(subdataset_dichotomizedDIF[,1:9])
subset_data <- subdataset_dichotomizedDIF[subdataset_dichotomizedDIF$total_score == total_score_value, ]
items <- c('Z7')

contingency_tables <- list()
for (item in items) {
  contingency_table <- matrix(0, nrow = 2, ncol = 2,
                              dimnames = list(c("Reference group (0)", "Focal group (1)"),
                                              c("Yi = 1", "Yi = 0")))
  
  ref_group <- subset_data[subset_data$Western == 0, ]
  contingency_table["Reference group (0)", "Yi = 1"] <- sum(ref_group[[item]] == 1)
  contingency_table["Reference group (0)", "Yi = 0"] <- sum(ref_group[[item]] == 0)
  
  focal_group <- subset_data[subset_data$Western == 1, ]
  contingency_table["Focal group (1)", "Yi = 1"] <- sum(focal_group[[item]] == 1)
  contingency_table["Focal group (1)", "Yi = 0"] <- sum(focal_group[[item]] == 0)
  
  contingency_tables[[item]] <- contingency_table
}
contingency_tables
################################################################################
#MH test
difModel <- difMH(Data = subdataset_dichotomizedDIF[,1:10], group = "Western", focal.name = 1,match = "score",
                  p.adjust.method = "none", purify = FALSE)

difModel #Table 21

create_contingency_tables <- function(data, items, group_column, score_column) {
  # Initialize a list to store contingency tables for each item and total score
  contingency_tables <- list()
  
  # Loop through each item
  for (item in items) {
    item_tables <- list()
    
    # Get unique total scores
    total_scores <- unique(data[[score_column]])
    
    # Loop through each total score
    for (score in total_scores) {
      # Subset data for the current total score
      subset_data <- data[data[[score_column]] == score, ]
      
      # Initialize contingency table for the current score and item
      contingency_table <- matrix(0, nrow = 2, ncol = 2,
                                  dimnames = list(c("Reference group (0)", "Focal group (1)"),
                                                  c("Yi = 1", "Yi = 0")))
      
      # Calculate counts for the reference group (group == 0)
      ref_group <- subset_data[subset_data[[group_column]] == 0, ]
      contingency_table["Reference group (0)", "Yi = 1"] <- sum(ref_group[[item]] == 1, na.rm = TRUE)
      contingency_table["Reference group (0)", "Yi = 0"] <- sum(ref_group[[item]] == 0, na.rm = TRUE)
      
      # Calculate counts for the focal group (group == 1)
      focal_group <- subset_data[subset_data[[group_column]] == 1, ]
      contingency_table["Focal group (1)", "Yi = 1"] <- sum(focal_group[[item]] == 1, na.rm = TRUE)
      contingency_table["Focal group (1)", "Yi = 0"] <- sum(focal_group[[item]] == 0, na.rm = TRUE)
      
      # Store the contingency table for the current score
      item_tables[[as.character(score)]] <- contingency_table
    }
    
    # Store all contingency tables for the current item
    contingency_tables[[item]] <- item_tables
  }
  
  return(contingency_tables)
}


contingency_tables3 <- create_contingency_tables(
  data = subdataset_dichotomizedDIF,
  item = "Z32",
  group_column = "Western",
  score_column = "total_score"
) # Table 22


plot.MH(difModel, names = names_list) #Not in the report

################################################################################
# SIBTEST
difSIBTEST(Data = subdataset_dichotomizedDIF[1:10], group = "Western", focal.name = 1) #Uni DIF
difSIBTEST(Data = subdataset_dichotomizedDIF[1:10], group = "Western", focal.name = 1, type = "nudif") #Non-Uni DIF
# Table 23


###########################################################################
# Cumulative regression
fit_ORD1 <- difORD(Data = A_items_numeric, group = subdataset_dichotomizedDIF$Western, focal.name = 1, model = "cumulative")
fit_ORD1 # Table 24
coef(fit_ORD1, SE = TRUE, CI =0)$Z17 #Table 25
coef(fit_ORD1, SE = TRUE, CI =0)$Z32 #Table 25



#Figure 22
plot(fit_ORD1, item = "Z17", plot.type = "cumulative",
     group.names = c("Eastern", "Western"))
plot(fit_ORD1, item = "Z32", plot.type = "cumulative",
     group.names = c("Eastern", "Western"))
#Figure 23
plot(fit_ORD1, item = "Z17", plot.type = "category",
     group.names = c("Eastern", "Western"))
plot(fit_ORD1, item = "Z32", plot.type = "category",
     group.names = c("Eastern", "Western"))


# Prediction - not in the report
predict(fit_ORD1, item = "Z17", match = 0, group = c(0, 1))
predict(fit_ORD1, item = "Z17", match = 0, group = c(0, 1), type = "cumulative")

#####################################################################################
#IRT - not in the report
lordiffModel <- lordif(resp.data = A_items_numeric, group = subdataset_dichotomizedDIF$Western, alpha = 0.05)
summary(lordiffModel)
lordiffModel$ipar.sparse


G <- ds$BFI_agreeableness - ds$BFI_closeones_agreeableness
#Questions
data <- data.frame(G = G)

# Plot with ggplot
histQ <- ggplot(data, aes(x = G)) +
  # Histogram
  geom_histogram(aes(y = ..density..), bins = 20, fill = "skyblue", color = "black", alpha = 0.7) +
  # Smoothed density curve
  geom_density(color = "darkgreen", size = 1.2) +
  # Normal bell curve
  stat_function(fun = dnorm, 
                args = list(mean = mean(G), sd = sd(G)), 
                color = "red", size = 1, linetype = "dashed") +
  # Labels and title
  labs(title = "Histogram of Differences with Density Curves", 
       x = "Differences (G = Y_s - Y_c)", 
       y = "Density") +
  # Theme for better visualization
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, size = 14),
        axis.title = element_text(size = 12))
# QQ Plot of the Differences


data <- data.frame(
  SampleQuantiles = sort(G),
  TheoreticalQuantiles = qnorm((1:length(G) - 0.5) / length(G)) # Theoretical quantiles
)

# QQ Plot with ggplot2
QQQ<- ggplot(data, aes(x = TheoreticalQuantiles, y = SampleQuantiles)) +
  # Points for the QQ plot
  geom_point(color = "black", size = 2) +
  # QQ line
  geom_abline(slope = sd(G), intercept = mean(G), color = "red", size = 1.2, linetype = "dashed") +
  # Labels and title
  labs(title = "QQ Plot of Differences",
       x = "Theoretical Quantiles (Normal Distribution)",
       y = "Sample Quantiles") +
  # Theme for better visualization
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, size = 14),
        axis.title = element_text(size = 12))


question2 <- grid.arrange(histQ, QQQ, ncol = 2) # Figure 11
ggsave("question2.png", plot = question2, width = 12, height = 6, dpi = 300)

#BFI ANALYSIS
#ABE-KZA
#November 2024
################################################################################
library(ggplot2)
library(dplyr)
library(gridExtra)  # for arranging multiple plots
library(ggdendro)   # dendrogram plotting


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




#################################################################################
# Plotting histograms of Y_c/Y_s to check normality
plot1 <- ggplot(ds, aes(x = BFI_agreeableness)) +
  geom_histogram(binwidth = 5, fill = "skyblue", color = "black") +
  labs(title = "Histogram of BFI Agreeableness", x = "BFI Agreeableness Score", y = "Frequency") +
  theme_minimal()


plot2 <- ggplot(ds, aes(x = BFI_closeones_agreeableness)) +
  geom_histogram(binwidth = 5, fill = "lightcoral", color = "black") +
  labs(title = "Histogram of BFI Closeones Agreeableness", x = "BFI Closeones Agreeableness Score", y = "Frequency") +
  theme_minimal()

#combining 
combined_plot<- grid.arrange(plot1, plot2, ncol = 2)
ggsave("combined_histograms.png", plot = combined_plot, width = 12, height = 6, dpi = 300)


################################################################################
#Regression - checking assumptions

lm.model1.2 <- lm(BFI_agreeableness ~ BFI_neuroticism, data = ds_og)
checkresiduals(lm.model1.2) #automatic, but it is ugly

#residuals and fitted values
residuals <- resid(lm.model1.2)
fitted_values <- fitted(lm.model1.2)
squared_residuals <- residuals^2

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


# Arrange plots in a grid layout
combined_plots3 <- grid.arrange(residuals_vs_fitted, qq_plot, autocorrelation_plot, scale_location_plot, ncol = 2)

# Save the combined plots to a file
ggsave("model_diagnostics.png", plot = combined_plots3, width = 12, height = 8)

par(mfrow = c(1,1))

#################################################################################
#Corelation plotting

corTESTscores <- corrplot.mixed(
  correlation_matrix,
  lower = "circle",
  upper = "number"                    
)

#correlation plot without variable labels
polychoricPlot <- ggcorrplot(
  corsubrho,
  hc.order = FALSE,                     # Do not hierarchically cluster
  type = "lower",                       # Show only lower triangle
  lab = TRUE,                           # Display correlation coefficients in the plot
  lab_size = 3,                         # Set the size of correlation labels
  colors = c("red", "white", "blue"),  # Set color scale
  legend.title = "Correlation"
) +
  theme_minimal() +                    # Use a clean theme for better readability
  theme(
    plot.title = element_text(hjust = 0.5),  # Center the title
    legend.position = "bottom",                # Move legend to bottom
    axis.title.x = element_blank(),            # Remove x-axis title
    axis.title.y = element_blank()           # Remove y-axis title             # Remove y-axis text labels
  )

#dendrogram plot without x, y axis labels
ddgram <- ggdendrogram(hc, rotate = TRUE) +  # Rotate the dendrogram if desired
  theme_minimal() +
  theme(  # Remove y-axis text labels
    axis.title.y = element_blank(),
    axis.title.x = element_blank(),# Remove y-axis title
    plot.margin = margin(10, 10, 10, 10)    # Adjust margins
  ) +
  geom_segment(size = 1)  # Adjust the size of the lines in the dendrogram

#comb plots
polychoricPlot2 <- grid.arrange(
  polychoricPlot,
  ddgram,
  ncol = 2  # Adjust height ratios if needed
)


ggsave("corTESTpoly.png", plot = polychoricPlot2, width = 12, height = 6, dpi = 300)

################################################################################
#CH4 - Reliability testing

# Set parameters
rho_original <- 0.637 # reliability of original data
items_original <- 9  # number of items in original data

# Define a sequence for items_new from 1 to 50
items_new_seq <- seq(1, 60, by = 1)

# Calculate reliability for each value in items_new_seq
reliability_values <- sapply(items_new_seq, function(items_new) {
  m <- items_new / items_original # ratio of test lengths
  m * rho_original / (1 + (m - 1) * rho_original)
})

# Create a data frame for plotting
data <- data.frame(
  items_new = items_new_seq,
  reliability = reliability_values
)

#plot
reliability <- ggplot(data, aes(x = items_new, y = reliability)) +
  geom_line(color = "lightblue") +
  geom_point(color = "blue") +
  geom_hline(yintercept = 1, linetype = "dashed", color = "coral") +
  geom_hline(yintercept = 0.9, linetype = "dashed", color = "purple") +
  geom_hline(yintercept = 0.64, linetype = "dashed", color = "black") +
  annotate("text", x = 48, y = 1.03, label = "Perfect Correlation", color = "coral", hjust = 1.2, size = 3.5) +
  annotate("text", x = 48, y = 0.93, label = "Desired Correlation", color = "purple", hjust = 1.2, size = 3.5) +
  annotate("text", x = 48, y = 0.67, label = "Current Correlation", color = "black", hjust = 1.2, size = 3.5) +
  labs(
    x = "Number of Items",
    y = "Reliability (correlation)"
  ) +
  theme_minimal()


ggsave("reliability.png", plot = reliability, width = 12, height = 6, dpi = 300)

# Load necessary libraries
library(mirt)
library(directlabels)

# Assuming fit.GRSMirt.mirt is already defined in your environment

# Apply a minimal theme and create the plot
bwtheme <- standard.theme(color = TRUE, bg = "white", fg = "grey")  # Black and white minimal theme

# Plot the item trace with the minimal theme
plt <- itemplot(fit.GRSMirt.mirt, item = 'Z37', type = "trace", par.settings = bwtheme)

# Update the plot (optional for adding labels or customizations)
updated_plot <- update(plt, ylab = expression(Prob(theta)), main = "Item Trace for Z37")

# Use directlabels to add labels on the trace lines (optional for clarity)
direct.label(updated_plot, 'top.points')

# Display the plot
updated_plot

# Load required libraries
library(ggplot2)
library(dplyr)

# Assuming `subdataset` is your data frame

# 1. Histogram of total scores
ggplot(subdataset, aes(x = partTotal)) +
  geom_histogram(bins = 20, fill = "darkred", color = "black", alpha = 0.7) +
  labs(
    x = "Total score",
    y = "Number of respondents",
    title = "Histogram of total scores"
  ) +
  theme_minimal()

# 2. Item mean scores by group (for Z2)
# Create groups by total score
subdataset <- subdataset %>%
  mutate(score_group = ntile(partTotal, 3))  # Divide total score into 3 groups

# Calculate mean selection proportion for Z2
group_means <- subdataset %>%
  group_by(score_group) %>%
  summarize(mean_Z2 = mean(Z2, na.rm = TRUE))

# Plot the mean scores
pic1 <- ggplot(group_means, aes(x = score_group, y = mean_Z2)) +
  geom_point(color = "red", size = 3) +
  geom_line(color = "red") +
  geom_text(aes(label = round(mean_Z2, 2)), vjust = -0.5) +
  labs(
    x = "Group by total score",
    y = "Option selection proportion (Z2)"
  ) +
  theme_minimal()



# First, calculate the ULI values as you already have
item_analysis <- ItemAnalysis(subdataset[, 1:9])
ULI_values <- item_analysis$ULI

# Add group information based on the ULI values into the dataset
library(dplyr)

# Assuming you want to use 'partTotal' to divide into 3 groups
subdataset<- subdataset %>%
  mutate(
    ULI_group = case_when(
      partTotal <= quantile(partTotal, 1/3) ~ "blue",   # Group 1: Blue
      partTotal <= quantile(partTotal, 2/3) ~ "white",  # Group 2: White
      TRUE ~ "red"                                     # Group 3: Red
    )
  )

library(ggplot2)

# Create the histogram with the custom colors
pic2 <- ggplot(subdataset, aes(x = partTotal, fill = ULI_group)) +
  geom_histogram(binwidth = 1, color = "black", position = "identity") + 
  scale_fill_identity() +   # Use the colors already defined in `group_color`
  labs(
    x = "Total score",
    y = "Number of respondents") +
  theme_minimal()

library(gridExtra)
ddplotHist <- grid.arrange(pic2, pic1, ncol = 2)

ggsave("ddplotHist.png", plot = ddplotHist, width = 12, height = 6, dpi = 300)

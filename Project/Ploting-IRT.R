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


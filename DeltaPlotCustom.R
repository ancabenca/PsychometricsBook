deltaPlotCustom <- function(data, group, focal.name, thr = 1.5) {
  # Ensure required libraries are available
  if (!requireNamespace("dplyr", quietly = TRUE)) {
    stop("The 'dplyr' package is required. Install it with install.packages('dplyr').")
  }
  
  library(dplyr)
  
  # Check inputs
  if (!group %in% names(data)) {
    stop("Group variable not found in dataset.")
  }
  
  # Separate reference and focal groups
  ref_data <- data %>% filter(!!sym(group) != focal.name)
  focal_data <- data %>% filter(!!sym(group) == focal.name)
  
  # Compute proportions for each item
  ref_props <- ref_data %>% summarise(across(-all_of(group), ~ mean(. == 1, na.rm = TRUE)))
  focal_props <- focal_data %>% summarise(across(-all_of(group), ~ mean(. == 1, na.rm = TRUE)))
  
  # Calculate delta scores
  delta_scores <- as.numeric(focal_props - ref_props)
  item_names <- colnames(ref_props)
  
  # Fit a linear regression through origin
  lm_fit <- lm(delta_scores ~ item_names - 1) # Through origin
  
  # Compute perpendicular distances
  predicted <- predict(lm_fit)
  perpendicular_distances <- delta_scores - predicted
  
  # Identify DIF items based on threshold
  DIF_items <- item_names[abs(perpendicular_distances) > thr]
  
  # Return results as a list
  return(list(
    delta_scores = delta_scores,
    perpendicular_distances = perpendicular_distances,
    DIF_items = DIF_items,
    lm_fit = lm_fit
  ))
}

#TO LOAD NECESSARY OBJECTS INTO YOUR ENVIRONMENT, RUN LINES 1-101 IN NDVI_comparison.R
#Computing summary stats from each group (ARDO, veg, ground) and plotting
#lines 3-85 from NDVI_comparison to get list of dataframes loaded into environment

library(raster)
library(sp)
library(terra)
library(dplyr)
library(ggplot2)

#vertically combining dfs (don't merge by date- different lengths when combinig groups)
ARDO_combined <- list_of_ARDO_dfs[[1]]
for (i in 2:length(list_of_ARDO_dfs)) {
  ARDO_combined <- rbind(ARDO_combined, list_of_ARDO_dfs[[i]])
}
ARDO_combined$Group <- "ARDO"

veg_combined <- list_of_veg_dfs[[1]]
for (i in 2:length(list_of_veg_dfs)) {
  veg_combined <- rbind(veg_combined, list_of_veg_dfs[[i]])
}
veg_combined$Group <- "veg"

ground_combined <- list_of_ground_dfs[[1]]
for (i in 2:length(list_of_ground_dfs)) {
  ground_combined <- rbind(ground_combined, list_of_ground_dfs[[i]])
}
ground_combined$Group <- "ground"

#vertically combine each of the three dataframes into one large df
full_combine <- rbind(ARDO_combined, veg_combined, ground_combined)

#group by group and date to then calculate summary stats (min, max, mean, median, stdev)
#store results in new df: 
summary_df <- full_combine %>%
  group_by(Group, Date) %>%
  summarize(
    Min = min(Value, na.rm = T),
    Max = max(Value, na.rm = T),
    Median = median(Value, na.rm = T),
    Mean = mean(Value, na.rm = T),
    StDev = sd(Value, na.rm = T)) 

#reformatting df so it's easier to compare values by date
#not necessary for any subsequent code
#reformatted_df <- summary_df %>%
#  arrange(Date)

# Plot mean values by Date for each Group with shaded regions representing standard deviation
group_col <- c("ARDO" = "red", "veg" = "blue", "ground" = "tan")
ggplot(summary_df, aes(x = Date, y = Mean, color = Group, fill = Group)) +
  geom_point() +
  geom_ribbon(aes(ymin = Mean - StDev, ymax = Mean + StDev, fill = Group), color = NA, alpha = 0.1) +
  labs(title = "Average NDVI (May 2022-2023)", x = "Date", y = "Mean NDVI", color = "Group") +
  scale_color_manual(values = group_col) +
  scale_fill_manual(values = group_col) +
  theme_minimal()





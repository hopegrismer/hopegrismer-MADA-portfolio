# Install the 'here' package 
install.packages("here")

# Load necessary libraries
library(ggplot2)
library(dplyr)
library(here)  # Load the 'here' package

# Load the dataset using here::here() for the file path
data <- read.csv(here("cdcdata-exercise", "Weekly_Influenza_Vaccination_Coverage_Among_Pregnant_Women_18-49_Years__by_Race_and_Ethnicity_20250205.csv"))

# Visualize vaccination coverage by race and ethnicity
bloxplot1iteration = ggplot(data, aes(x = Percent, fill = Race.and.Ethnicity)) + 
  geom_histogram(binwidth = 100, color = "black", position = "dodge") +
  labs(title = "Vaccination Coverage Distribution by Race and Ethnicity - First Iteration",
       x = "Vaccination Coverage (%)",
       y = "Frequency") +
  theme_minimal()

# Save first iteration of barplot
figure_file = here("cdcdata-exercise","cdcdata-exercise-tables-graphs","bloxplot-coverage-by-race-first-iteration.png")
ggsave(filename = figure_file, plot=bloxplot1iteration)


# There is negative vaccination coverage (like -50%) appearing on histogram which doesn’t make sense in context. 
# Check for any negative values in the Percent column
negative_values <- data %>% filter(Percent < 0)
print(negative_values)

# Visualize vaccination coverage by race and ethnicity with updated BINS
ggplot(data, aes(x = Percent, fill = Race.and.Ethnicity)) + 
  geom_histogram(binwidth = 10, color = "black", position = "dodge") +
  labs(title = "Vaccination Coverage Distribution by Race and Ethnicity",
       x = "Vaccination Coverage (%)",
       y = "Frequency") +
  theme_minimal()

#### ------- Making the Distributions ----- ###

# Summarize vaccination coverage by race and ethnicity
data_summary <- data %>%
  group_by(Race.and.Ethnicity) %>%
  summarize(avg_vaccination_coverage = mean(Percent, na.rm = TRUE))

# View the summary data
print(data_summary)

# Boxplot of vaccination coverage by race and ethnicity
bloxplot1 = ggplot(data, aes(x = Race.and.Ethnicity, y = Percent, fill = Race.and.Ethnicity)) + 
  geom_boxplot() + 
  labs(title = "Vaccination Coverage by Race and Ethnicity",
       x = "Race and Ethnicity",
       y = "Vaccination Coverage (%)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotate x labels for better readability +
  theme(
  panel.background = element_rect(fill = "white"),  # White background for the plot panel
  plot.background = element_rect(fill = "white")    # White background for the overall plot
)


#Save bloxplot1 
figure_file = here("cdcdata-exercise","cdcdata-exercise-tables-graphs","bloxplot-coverage-by-race.png")
ggsave(filename = figure_file, plot=bloxplot1)

# Summarize the data by Race and Ethnicity to calculate the mean and standard deviation
data_summary <- data %>%
  group_by(Race.and.Ethnicity) %>%
  summarize(
    avg_vaccination_coverage = mean(Percent, na.rm = TRUE),
    sd_vaccination_coverage = sd(Percent, na.rm = TRUE)
  )

# View the summary data
print(data_summary)

# Load necessary libraries
library(ggplot2)

# Create a bar plot showing the mean vaccination coverage with standard deviation as error bars
barplot1 = ggplot(data_summary, aes(x = Race.and.Ethnicity, y = avg_vaccination_coverage, fill = Race.and.Ethnicity)) +
  geom_bar(stat = "identity", color = "black", show.legend = FALSE) +  # Bar plot
  geom_errorbar(aes(ymin = avg_vaccination_coverage - sd_vaccination_coverage, 
                    ymax = avg_vaccination_coverage + sd_vaccination_coverage), 
                width = 0.2) +  # Add error bars for SD
  labs(title = "Mean Vaccination Coverage by Race and Ethnicity with Standard Deviation",
       x = "Race and Ethnicity",
       y = "Vaccination Coverage (%)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotate x-axis labels for readability

#Save barplot1 showing the mean vaccination coverage with sd error bars
figure_file = here("cdcdata-exercise","cdcdata-exercise-tables-graphs","barplot-meanwithsd-by-race.png")
ggsave(filename = figure_file, plot=barplot1)

#### To visualize the distribution of vaccination coverage (like a bell curve or a smooth curve) for each race/ethnicity group
# Load necessary libraries
library(ggplot2)
library(dplyr)

# Create a density plot for vaccination coverage by race/ethnicity
densityplot1 = ggplot(data, aes(x = Percent, fill = Race.and.Ethnicity, color = Race.and.Ethnicity)) +
  geom_density(alpha = 0.4) +  # alpha for transparency
  labs(title = "Density Plot of Vaccination Coverage by Race and Ethnicity",
       x = "Vaccination Coverage (%)",
       y = "Density") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +  # Rotate x labels for readability
  scale_fill_brewer(palette = "Spectral") +  # Custom color palette
  scale_color_brewer(palette = "Spectral")  # Custom color palette for lines

#Save density plot for vaccination coverage by race/ethnicity
figure_file = here("cdcdata-exercise","cdcdata-exercise-tables-graphs","density-plot-vaxcov-by-race.png")
ggsave(filename = figure_file, plot=densityplot1)
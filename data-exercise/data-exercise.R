# Load necessary libraries
library(tidyverse)  # For data manipulation and visualization
library(ggplot2)      

# Creation of Synthetic Data Set Relating Diet to Adverse Events & --------

# Set a random seed for reproducibility
set.seed(42)

# Number of patients per diet group
n_patients_per_group <- 50  # 50 patients in each diet group

# Total number of observations (patients x 10 days)
n_patients <- n_patients_per_group * 2  # Two diet groups
n_days <- 10  # 10 days of sampling per patient

# Diet group assignments
diet_groups <- rep(c("High-Sodium Diet", "Mediterranean Diet"), each = n_patients_per_group)

# Generate the fake patient data
patient_data <- data.frame(
  patient_id = rep(1:n_patients, each = n_days),  # Unique patient ID
  diet_group = rep(diet_groups, each = n_days),
  day = rep(1:n_days, times = n_patients),  # Day of sample (1 to 10)
  cholesterol = NA,  # Placeholder for cholesterol levels
  systolic_bp = NA,  # Placeholder for systolic blood pressure
  diastolic_bp = NA,  # Placeholder for diastolic blood pressure
  drug_concentration = NA,  # Placeholder for drug concentrations
  adverse_event = NA  # Placeholder for adverse events
)

# Simulate baseline cholesterol levels (random, with some variation between patients)
patient_data$cholesterol <- rnorm(n_patients, mean = 200, sd = 25)[patient_data$patient_id]

# Blood pressure varies by diet group and is slightly elevated in the High-Sodium Diet group
patient_data$systolic_bp <- ifelse(patient_data$diet_group == "High-Sodium Diet", 
                                   rnorm(n_patients, mean = 145, sd = 10)[patient_data$patient_id],
                                   rnorm(n_patients, mean = 130, sd = 8)[patient_data$patient_id])

patient_data$diastolic_bp <- ifelse(patient_data$diet_group == "High-Sodium Diet", 
                                    rnorm(n_patients, mean = 95, sd = 8)[patient_data$patient_id],
                                    rnorm(n_patients, mean = 85, sd = 7)[patient_data$patient_id])

# Drug concentrations decay over time, with higher concentration for Mediterranean Diet group initially
patient_data$drug_concentration <- ifelse(patient_data$diet_group == "Mediterranean Diet",
                                          40 * exp(-0.08 * patient_data$day),  # Slower decay for Mediterranean diet
                                          50 * exp(-0.1 * patient_data$day))   # Faster decay for High-Sodium Diet

# Define a function to simulate adverse events based on drug concentration
simulate_adverse_event <- function(concentration, diet_group) {
  # Base probability of an adverse event as a function of drug concentration
  base_prob <- min(0.1 + (concentration / 100), 0.8)  # Capped at 80%
  
  # Diet type influences the adverse event rate: High-Sodium Diet may have more adverse events
  if (diet_group == "High-Sodium Diet") {
    base_prob <- min(base_prob + 0.15, 0.90)  # Increase risk for High-Sodium Diet group
  }
  
  rbinom(1, 1, base_prob)  # Simulate a binary outcome (0 = no event, 1 = event)
}

# Apply adverse event simulation to each observation (per patient, per day)
patient_data$adverse_event <- mapply(simulate_adverse_event, 
                                     patient_data$drug_concentration, 
                                     patient_data$diet_group)

# Simulate some minor changes in cholesterol and blood pressure based on drug adherence and diet
# Let's assume cholesterol improves slightly for Mediterranean diet over time and worsens for High-Sodium

patient_data$cholesterol <- ifelse(patient_data$diet_group == "Mediterranean Diet",
                                   patient_data$cholesterol - (patient_data$day * 0.2),  # Slow reduction
                                   patient_data$cholesterol + (patient_data$day * 0.3))  # Slow increase for High-Sodium

patient_data$systolic_bp <- patient_data$systolic_bp - (patient_data$day * 0.5)  # Systolic BP drops with time
patient_data$diastolic_bp <- patient_data$diastolic_bp - (patient_data$day * 0.3)  # Diastolic BP drops with time

# Ensure BP stays within reasonable ranges
patient_data$systolic_bp <- pmax(patient_data$systolic_bp, 120)  # Prevent systolic BP from dropping too low
patient_data$diastolic_bp <- pmax(patient_data$diastolic_bp, 70)  # Prevent diastolic BP from dropping too low

# Preview the first few rows of the dataset
head(patient_data)

# Optionally: Save the dataset to a CSV file
write.csv(patient_data, "synthetic_hypertension_lifestyle_data.csv", row.names = FALSE)

# Final output summary
summary(patient_data)  # Check summary statistics of the dataset


# Create a copy of the original dataset - did not konw that you could make a copy to play with within the same script
patient_data_copy <- patient_data


# Making Fake Data More Real - 4 ways -------------------------------------

# Add random noise to drug concentration (e.g., small fluctuations)
patient_data_copy$drug_concentration <- patient_data_copy$drug_concentration + rnorm(nrow(patient_data_copy), mean = 0, sd = 2)

# Increase cholesterol levels by 5% for all patients (simulating a small change)
patient_data_copy$cholesterol <- patient_data_copy$cholesterol * 1.05

# Change blood pressure by adding a constant value to systolic and diastolic
patient_data_copy$systolic_bp <- patient_data_copy$systolic_bp + 3  # Increase systolic BP by 3 mmHg
patient_data_copy$diastolic_bp <- patient_data_copy$diastolic_bp + 2  # Increase diastolic BP by 2 mmHg

# Randomly alter the treatment group for some patients (e.g., swap a small percentage)
set.seed(123)  # For reproducibility
swap_indices <- sample(1:nrow(patient_data_copy), size = round(0.1 * nrow(patient_data_copy)), replace = FALSE)  # 10% of patients
patient_data_copy$diet_group[swap_indices] <- ifelse(patient_data_copy$diet_group[swap_indices] == "High-Sodium Diet", 
                                                     "Mediterranean Diet", 
                                                     "High-Sodium Diet")
# Check the first few rows of the modified copy
head(patient_data_copy)


# Creating The Graphs -----------------------------------------------------


# Plot for the diet study hypothetical - the original ---------------------

# Load necessary libraries
library(ggplot2)
library(dplyr)

p2 <- ggplot(patient_data, aes(x = as.factor(adverse_event), y = drug_concentration, 
                               fill = diet_group)) +
  # Boxplot showing the distribution of drug concentration by adverse event and diet
  geom_boxplot(width = 0.7, position = position_dodge(width = 0.8), color = "black") +
  
  # Overlay raw data points on the boxplot
  geom_point(aes(color = diet_group), position = position_dodge(width = 0.8), 
             size = 3, shape = 16) + 
  
  # Add axis labels and a title
  labs(
    x = "Adverse Events",   # X-axis label
    y = "Drug Concentration",   # Y-axis label
    title = "Drug Concentration by Adverse Events and Diet Type"   # Plot title
  ) +
  
  # Custom color scale for diet groups
  scale_color_manual(values = c("High-Sodium Diet" = "red", "Mediterranean Diet" = "blue")) +  # Customize color for each diet group
  scale_fill_manual(values = c("High-Sodium Diet" = "red", "Mediterranean Diet" = "blue")) +  # Fill color for boxplot
  
  # Set the theme for minimal styling
  theme_minimal() +
  
  # Adjust legend and other aesthetic settings
  theme(legend.position = "top", 
        legend.title = element_blank(),
        legend.text = element_text(size = 12),
        axis.text = element_text(size = 12),
        axis.title = element_text(size = 14))

# Plot the graphic
plot(p2)

# Creating a Copy of the Data, Adding Noise, and Changing Conditions --------

# Create a copy of the original dataset
patient_data_copy <- patient_data

# Example 1: Add random noise to drug concentration (e.g., small fluctuations)
patient_data_copy$drug_concentration <- patient_data_copy$drug_concentration + rnorm(nrow(patient_data_copy), mean = 0, sd = 2)

# Example 2: Increase cholesterol levels by 5% for all patients (simulating a small change)
patient_data_copy$cholesterol <- patient_data_copy$cholesterol * 1.05

# Example 3: Change blood pressure by adding a constant value to systolic and diastolic
patient_data_copy$systolic_bp <- patient_data_copy$systolic_bp + 3  # Increase systolic BP by 3 mmHg
patient_data_copy$diastolic_bp <- patient_data_copy$diastolic_bp + 2  # Increase diastolic BP by 2 mmHg

# Example 4: Randomly alter the treatment group for some patients (e.g., swap a small percentage)
set.seed(123)  # For reproducibility
swap_indices <- sample(1:nrow(patient_data_copy), size = round(0.1 * nrow(patient_data_copy)), replace = FALSE)  # 10% of patients
patient_data_copy$diet_group[swap_indices] <- ifelse(patient_data_copy$diet_group[swap_indices] == "High-Sodium Diet", 
                                                     "Mediterranean Diet", 
                                                     "High-Sodium Diet")
# Check the first few rows of the modified copy
head(patient_data_copy)


# Creating Second Plot with Added Noise  ----------------------------------

p3 <- ggplot(patient_data_copy, aes(x = as.factor(adverse_event), y = drug_concentration, 
                               fill = diet_group)) +
  # Boxplot showing the distribution of drug concentration by adverse event and diet
  geom_boxplot(width = 0.7, position = position_dodge(width = 0.8), color = "black") +
  
  # Overlay raw data points on the boxplot
  geom_point(aes(color = diet_group), position = position_dodge(width = 0.8), 
             size = 3, shape = 16) + 
  
  # Add axis labels and a title
  labs(
    x = "Adverse Events",   # X-axis label
    y = "Drug Concentration",   # Y-axis label
    title = "Drug Concentration by Adverse Events and Diet Type with Complications"   # Plot title
  ) +
  
  # Custom color scale for diet groups
  scale_color_manual(values = c("High-Sodium Diet" = "red", "Mediterranean Diet" = "blue")) +  # Customize color for each diet group
  scale_fill_manual(values = c("High-Sodium Diet" = "red", "Mediterranean Diet" = "blue")) +  # Fill color for boxplot
  
  # Set the theme for minimal styling
  theme_minimal() +
  
  # Adjust legend and other aesthetic settings
  theme(legend.position = "top", 
        legend.title = element_blank(),
        legend.text = element_text(size = 12),
        axis.text = element_text(size = 12),
        axis.title = element_text(size = 14))

# Plot the graphic
plot(p3)


# Inserting Realism through the SimStudy Package --------------------------


# Install and load simstudy package if not already installed
install.packages("simstudy")
library(simstudy)

# Set seed for reproducibility
set.seed(42)

# Introduce extreme cholesterol outliers
cholesterol_outliers <- sample(1:nrow(patient_data), size = 6)  # Randomly pick 5 rows
patient_data$cholesterol[cholesterol_outliers] <- patient_data$cholesterol[cholesterol_outliers] + rnorm(5, mean = 100, sd = 20)  # Adding extreme high values
cholesterol_outliers_data = patient_data$cholesterol[cholesterol_outliers] 

# Create Plots with Extreme Patients Added In -----------------------------

# Create a boxplot for drug concentration with the outliers in cholesterol
p4 <- ggplot(patient_data, aes(x = factor(cholesterol > 300), y = drug_concentration, fill = cholesterol > 300)) +
  geom_boxplot() +
  labs(x = "Cholesterol (Outliers vs Normal)", 
       y = "Drug Concentration", 
       title = "Boxplot of Drug Concentration with Cholesterol Outliers") +
  scale_fill_manual(values = c("red", "blue")) +
  scale_x_discrete(labels = c("FALSE" = "Normal", "TRUE" = "Outlier")) +  # Change labels for cholesterol categories
  # Different color and different labels for data with outliers included v. normal (excluded)
  theme_minimal()

# Plot the graphic
plot(p4)



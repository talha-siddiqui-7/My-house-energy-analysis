# Load necessary libraries
library(tidyverse)
library(lubridate)
library(viridis)
library(ggplot2)
library(ggrepel)
library(gt)
library(zoo)
library(gridExtra)

# Set working directory and paths
WD <- getwd()
DIR_DATA <- file.path(WD, "Data")
plot_dir <- file.path(WD, "Plots")

# Check if the plot directory exists, if not, create it
if (!dir.exists(plot_dir)) {
  dir.create(plot_dir)
}

# Read preprocess data
file_path <- file.path(DIR_DATA, "2024-11-17-12-38 Chronograf Data_Talha House.csv")
data <- read_csv(file_path)


# Apply forward fill to the specified columns
data <- data %>%
  mutate(
    Microwave = na.locf(Microwave, na.rm = FALSE),
    `Small devices` = na.locf(`Small devices`, na.rm = FALSE)
  )


# Calculate actual values by subtracting the previous value from the current one
data <- data %>%
  mutate(
    date = as.Date(time),  # Extract the date part of the 'time' column
    actual_Microwave = Microwave - lag(Microwave, order_by = time),
    actual_Small_devices = `Small devices` - lag(`Small devices`, order_by = time)
  ) %>%
  group_by(date) %>%
  mutate(
    # Replace NA values (result of lag() at the start of each day) with 0
    actual_Microwave = if_else(is.na(actual_Microwave), 0, actual_Microwave),
    actual_Small_devices = if_else(is.na(actual_Small_devices), 0, actual_Small_devices)
  ) %>%
  ungroup()  %>%
  mutate(
    # Replace negative values with 0
    actual_Microwave = if_else(actual_Microwave < 0, 0, actual_Microwave),
    actual_Small_devices = if_else(actual_Small_devices < 0, 0, actual_Small_devices)
  )  

# Histogram for Actual Microwave Values
ggplot(data, aes(x = actual_Microwave)) +
  geom_histogram(binwidth = 0.005, fill = "skyblue", color = "black", alpha = 0.7) +
  labs(title = "Histogram of Actual Microwave Values", x = "Actual Microwave", y = "Frequency") +
  theme_minimal()

# Histogram for Actual Small Devices Values
ggplot(data, aes(x = actual_Small_devices)) +
  geom_histogram(binwidth = 0.005, fill = "lightgreen", color = "black", alpha = 0.7) +
  labs(title = "Histogram of Actual Small Devices Values", x = "Actual Small Devices", y = "Frequency") +
  theme_minimal()

# Density plot for Actual Microwave Values
ggplot(data, aes(x = actual_Microwave)) +
  geom_density(fill = "skyblue", color = "black", alpha = 0.7) +
  labs(title = "Density Plot of Actual Microwave Values", x = "Actual Microwave", y = "Density") +
  theme_minimal()

# Density plot for Actual Small Devices Values
ggplot(data, aes(x = actual_Small_devices)) +
  geom_density(fill = "lightgreen", color = "black", alpha = 0.7) +
  labs(title = "Density Plot of Actual Small Devices Values", x = "Actual Small Devices", y = "Density") +
  theme_minimal()

# Calculate the sum of both variables
data_summary <- data %>%
  summarise(
    Microwave_Sum = sum(actual_Microwave, na.rm = TRUE),
    Small_Devices_Sum = sum(actual_Small_devices, na.rm = TRUE)
  ) %>%
  gather(key = "Variable", value = "Value")

# Calculate the sum of both variables
data_summary <- data %>%
  summarise(
    Microwave_Sum = sum(actual_Microwave, na.rm = TRUE),
    Small_Devices_Sum = sum(actual_Small_devices, na.rm = TRUE)
  ) %>%
  gather(key = "Variable", value = "Value")

# Calculate the total sum to compute percentages
total_sum <- sum(data_summary$Value)

# Add a new column for percentage
data_summary <- data_summary %>%
  mutate(Percentage = Value / total_sum * 100)

# Create the pie chart with percentages
ggplot(data_summary, aes(x = "", y = Value, fill = Variable)) +
  geom_bar(stat = "identity", width = 1) + 
  coord_polar(theta = "y") +
  labs(title = "Contribution of Microwave and Small Devices") +
  theme_void() +  # Removes axis and gridlines
  scale_fill_manual(values = c("skyblue", "lightgreen")) +  # Customize colors
  geom_text(aes(label = paste0(round(Percentage, 1), "%")),  # Add percentages as labels
            position = position_stack(vjust = 0.5),  # Adjust position of the labels
            color = "white", size = 6)  # Customize label appearance
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

# Step 1: Add tariff period information
data <- data %>%
  mutate(hour_of_day = as.numeric(format(time, "%H")),  # Extract the hour from the datetime
         tariff_period = case_when(
           hour_of_day >= 10 & hour_of_day < 14 ~ "Peak",
           hour_of_day >= 18 & hour_of_day < 22 ~ "Peak",
           hour_of_day >= 8 & hour_of_day < 10 ~ "Flat",
           hour_of_day >= 14 & hour_of_day < 18 ~ "Flat",
           hour_of_day >= 22 & hour_of_day < 24 ~ "Flat",
           TRUE ~ "Off-Peak"
         ))

# Step 2: Summarize total load per tariff period for each variable
tariff_summary <- data %>%
  group_by(tariff_period) %>%
  summarise(
    Microwave_Load = sum(actual_Microwave, na.rm = TRUE),
    Small_Devices_Load = sum(actual_Small_devices, na.rm = TRUE)
  ) %>%
  pivot_longer(cols = c(Microwave_Load, Small_Devices_Load), names_to = "Load_Type", values_to = "Load_kWh")

# Step 3: Create a cumulative stacked bar plot
ggplot(tariff_summary, aes(x = tariff_period, y = Load_kWh, fill = Load_Type)) +
  geom_bar(stat = "identity", position = "stack") +
  labs(
    title = "Cumulative Load by Tariff Period",
    x = "Tariff Period",
    y = "Load (kWh)",
    fill = "Load Type"
  ) +
  theme_minimal() +
  scale_fill_manual(values = c("skyblue", "lightgreen"))  # Custom colors for bars

# Step 1: Summarize total load per tariff period
tariff_total <- data %>%
  group_by(tariff_period) %>%
  summarise(Total_Load = sum(actual_Microwave + actual_Small_devices, na.rm = TRUE)) %>%
  mutate(Percentage = (Total_Load / sum(Total_Load)) * 100)  # Calculate percentage

# Create the bar plot with percentage labels inside the bars
ggplot(tariff_total, aes(x = tariff_period, y = Total_Load, fill = tariff_period)) +
  geom_bar(stat = "identity", show.legend = FALSE) +  # Create bars, no legend needed
  geom_text(aes(label = paste0(round(Percentage, 1), "%")),  # Add percentage labels
            position = position_stack(vjust = 0.5),  # Center labels inside the bars
            color = "black", size = 5) +  # Text style
  labs(
    title = "Total Load by Tariff Period with Percentage",
    x = "Tariff Period",
    y = "Total Load (kWh)"
  ) +
  theme_minimal() +
  scale_fill_manual(values = c("skyblue", "lightgreen", "gold", "orange")) +  # Custom colors
  theme(
    axis.title.x = element_text(size = 12),
    axis.title.y = element_text(size = 12),
    plot.title = element_text(size = 14, face = "bold", hjust = 0.5)
  )


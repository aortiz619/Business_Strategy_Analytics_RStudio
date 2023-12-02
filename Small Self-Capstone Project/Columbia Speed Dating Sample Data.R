
###########################################################
####        Capstone M°4 - SPEED DATING                ####
###########################################################

# Speed dating data with over 8,000 observations of matches and non-matches
# It has answers to survey questions about how people rate themselves and how they rate others on several dimensions.


############################################################
####             CLEAR DATA COMMAND                     ####
############################################################

# Set your directory to the folder where you have downloaded the Chocolate dataset 
# to clean up the memory of your current R session run the following line
rm(list=ls())

############################################################
####             LOAD DATA COMMAND                       ####
############################################################

# Let's load the data
data=read.table('Speed_DatingData.csv',sep=',',header=TRUE)
data = read.table('Speed_DatingData.csv', sep = ',', header = TRUE, fill = TRUE)

#OR

install.packages("readr")
library(readr)
data <- read_csv("Speed_DatingData.csv")


############################################################
#### sUMMARIZED/vIEWing DAT" VARIABLE COMMAND  #
############################################################

# Now let's have a look at our variables and see some summary statistics
# The str() function shows the structure of your dataset and details the type of variables that it contains
# Here should examine each variable or column (Here it show us the "average")
# observation means rows
str(data)

# The summary() function provides for each variable in your dataset the minimum, mean, maximum and quartiles
# Here should examine each variable or column (Min, Medium, and Max)
summary(data)

############################################################
###               Males and Females Totals             ####
############################################################

# GENDER TOTALS
# Assuming data frame with a 'gender' column
gender_count <- table(data$gender)
# Now you can access the counts
count_gender_0 <- gender_count[1]
count_gender_1 <- gender_count[2]
# Print the counts
print(count_gender_0)
print(count_gender_1)


# GENDER TOTALS (PERCENT)
# Assuming 'gender_count' is table of gender counts
gender_percentages <- prop.table(gender_count) * 100
# Access the percentages
percentage_gender_0 <- gender_percentages[1]
percentage_gender_1 <- gender_percentages[2]
# Print the percentages
print(percentage_gender_0)
print(percentage_gender_1)


# GENDER(S) OLDEST AGE
# Assuming 'data' is your data frame with a 'gender' column and 'age_o' variable
# Subset the data for gender 0
subset_gender_0 <- data[data$gender == 0,]
# Subset the data for gender 1
subset_gender_1 <- data[data$gender == 1,]
# Calculate the oldest age for each gender
oldest_age_gender_0 <- max(na.omit(subset_gender_0$age_o))
oldest_age_gender_1 <- max(na.omit(subset_gender_1$age_o))
# Print the results
print(paste("Oldest age for gender 0:", oldest_age_gender_0))
print(paste("Oldest age for gender 1:", oldest_age_gender_1))

# GENDERS(S) MEDIAN AGE & AvERAGE PERCENTAGE/WHOLE NUMBER SIZE
# Assuming data frame with a 'gender' column
# Calculate the median age for gender '0'
median_age_gender_0 <- median(data[data$gender == 0, ]$age_o, na.rm = TRUE)
# Calculate the count of individuals in gender '0' whose age matches the median age
count_matching_median_age_gender_0 <- sum(data[data$gender == 0, ]$age_o == median_age_gender_0, na.rm = TRUE)
# Calculate the percentage of individuals in gender '0' whose age matches the median age
percentage_matching_median_age_gender_0 <- (count_matching_median_age_gender_0 / count_gender_0) * 100
# Print the results
print(paste("Median age for gender 0:", median_age_gender_0))
print(paste("Count of individuals in gender 0 matching the median age:", count_matching_median_age_gender_0))
print(paste("Percentage of individuals in gender 0 matching the median age:", percentage_matching_median_age_gender_0))
# Calculate the median age for gender '1'
median_age_gender_1 <- median(data[data$gender == 1, ]$age_o, na.rm = TRUE)
# Calculate the count of individuals in gender '1' whose age matches the median age
count_matching_median_age_gender_1 <- sum(data[data$gender == 1, ]$age_o == median_age_gender_1, na.rm = TRUE)
# Calculate the percentage of individuals in gender '1' whose age matches the median age
percentage_matching_median_age_gender_1 <- (count_matching_median_age_gender_1 / count_gender_1) * 100
# Print the results
print(paste("Median age for gender 1:", median_age_gender_1))
print(paste("Count of individuals in gender 1 matching the median age:", count_matching_median_age_gender_1))
print(paste("Percentage of individuals in gender 1 matching the median age:", percentage_matching_median_age_gender_1))


############################################################
### Males and Females Income by Age (Side By Side Look) ####
############################################################

# Males and Females Income by Age (Side By Side Look)
library(ggplot2)
# Filter out missing values in the income and age columns
filtered_data <- data[!is.na(data$income) & !is.na(data$age_o),]
# Create a grouped bar plot with custom fill colors and legend labels, split by gender
ggplot(filtered_data, aes(x = as.factor(age), y = income, fill = as.factor(gender))) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Income by Gender and Age", x = "Age", y = "Income") +
  scale_fill_manual(values = c("0" = "lightblue", "1" = "pink"), name = "Gender",
                    labels = c("Males (0)", "Females (1)")) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +  # Rotate x-axis labels for better readability
  guides(fill = guide_legend(title = "Gender")) +
  facet_wrap(~ as.factor(gender), scales = "free_y", ncol = 2)


# INCOME FOR GENDER "1" OR FEMALE
# Filter data for gender 1
filtered_data_gender_1 <- filtered_data[filtered_data$gender == 1, ]
# Find the maximum and minimum income for gender 1
highest_income_gender_1 <- max(filtered_data_gender_1$income, na.rm = TRUE)
lowest_income_gender_1 <- min(filtered_data_gender_1$income, na.rm = TRUE)
# Print the results
print(paste("Highest income for gender 1:", highest_income_gender_1))
print(paste("Lowest income for gender 1:", lowest_income_gender_1))
# Filter data for gender 1
filtered_data_gender_1 <- filtered_data[filtered_data$gender == 1, ]
# Calculate the median income for gender 1
median_income_gender_1 <- median(filtered_data_gender_1$income, na.rm = TRUE)
# Print the result
print(paste("Median income for gender 1:", median_income_gender_1))


# INCOME FOR GENDER "0" OR MALE
# Filter data for gender 0
filtered_data_gender_0 <- filtered_data[filtered_data$gender == 0, ]
# Find the maximum and minimum income for gender 0
highest_income_gender_0 <- max(filtered_data_gender_0$income, na.rm = TRUE)
lowest_income_gender_0 <- min(filtered_data_gender_0$income, na.rm = TRUE)
# Print the results
print(paste("Highest income for gender 0:", highest_income_gender_0))
print(paste("Lowest income for gender 0:", lowest_income_gender_0))
# Filter data for gender 0
filtered_data_gender_0 <- filtered_data[filtered_data$gender == 0, ]
# Calculate the median income for gender 0
median_income_gender_0 <- median(filtered_data_gender_0$income, na.rm = TRUE)
# Print the result
print(paste("Median income for gender 0:", median_income_gender_0))


############################################################
###     Males and Females Location by WorldMap          ####
############################################################

# Install and load necessary packages
if (!requireNamespace("ggplot2", quietly = TRUE)) {
  install.packages("ggplot2")
}
if (!requireNamespace("rnaturalearth", quietly = TRUE)) {
  install.packages("rnaturalearth")
}
if (!requireNamespace("sf", quietly = TRUE)) {
  install.packages("sf")
}
library(ggplot2)
library(rnaturalearth)
library(sf)
# Assuming  'data' and has 'from' and 'gender' columns
# Count the number of occurrences of each gender in each city
gender_count <- table(data$from, data$gender)
# Convert the result to a data frame
gender_count_df <- as.data.frame(gender_count)
# Rename the columns for better clarity
colnames(gender_count_df) <- c('City', 'Male', 'Female')
# Melt the data frame for better plotting
gender_count_melted <- reshape2::melt(gender_count_df, id.vars = 'City')
# Get high-resolution world map
world <- ne_countries(scale = "medium", returnclass = "sf")
# Merge with map data for cities
gender_count_map <- merge(world, gender_count_melted, by.x = "iso_a3", by.y = "City", all.x = TRUE)
# Convert to sf object
gender_count_sf <- st_as_sf(gender_count_map)
# Create a heatmap
ggplot() +
  geom_sf(data = gender_count_sf, aes(fill = as.factor(value)), color = "white") +
  scale_fill_manual(values = c("0" = "lightblue", "1" = "pink"), name = "Population",
                    labels = c("Males (0)", "Females (1)")) +
  labs(title = "Gender Population Heatmap by City", fill = "Population") +
  theme_void()


############################################################
###     Males and Females Top 10 Careers              ####
############################################################


library(dplyr)
# Assuming data frame is named 'data'
data <- data %>%
  mutate(career_combined = tolower(career)) %>%
  mutate(career_combined = gsub("social worker", "social work", career_combined))
# Males Top 10 Careers
top_careers_gender_0 <- data %>%
  filter(gender == 0) %>%
  group_by(career_combined) %>%
  summarise(count = n()) %>%
  arrange(desc(count)) %>%
  head(10)
# Print the top 10 careers for Males
print(top_careers_gender_0)


library(dplyr)
# Assuming data frame is named 'data'
data <- data %>%
  mutate(career_combined = tolower(career))
# Women Top 10 Careers
top_careers_gender_1 <- data %>%
  filter(gender == 1) %>%
  group_by(career_combined) %>%
  summarise(count = n()) %>%
  arrange(desc(count)) %>%
  head(10)
# Print the top 10 careers for Females
print(top_careers_gender_1)


# - - - - - - Visualization - - - - - 

# GENDER TOP 10 CAREERS
# Install and load necessary packages
if (!requireNamespace("dplyr", quietly = TRUE)) {
  install.packages("dplyr")
}
if (!requireNamespace("ggplot2", quietly = TRUE)) {
  install.packages("ggplot2")
}
if (!requireNamespace("patchwork", quietly = TRUE)) {
  install.packages("patchwork")
}
library(dplyr)
library(ggplot2)
library(patchwork)
# Assuming data frame is named 'data'
data <- data %>%
  mutate(career_combined = tolower(career)) %>%
  mutate(career_combined = gsub("social worker", "social work", career_combined))
# Males Top 10 Careers
top_careers_gender_0 <- data %>%
  filter(gender == 0) %>%
  group_by(career_combined) %>%
  summarise(count = n()) %>%
  arrange(desc(count)) %>%
  head(10)
# Women Top 10 Careers
top_careers_gender_1 <- data %>%
  filter(gender == 1) %>%
  group_by(career_combined) %>%
  summarise(count = n()) %>%
  arrange(desc(count)) %>%
  head(10)
# Create separate bar plots for each gender with count labels
plot_gender_0 <- ggplot(top_careers_gender_0, aes(x = reorder(career_combined, -count), y = count)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  geom_text(aes(label = count), vjust = -0.5, size = 3) +  # Add count labels
  labs(title = "Top 10 Careers for Males", x = "Career", y = "Count") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
plot_gender_1 <- ggplot(top_careers_gender_1, aes(x = reorder(career_combined, -count), y = count)) +
  geom_bar(stat = "identity", fill = "pink") +
  geom_text(aes(label = count), vjust = -0.5, size = 3) +  # Add count labels
  labs(title = "Top 10 Careers for Females", x = "Career", y = "Count") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
# Arrange the plots side by side
plot_gender_0 + plot_gender_1


# GENDER TOP 4 CAREERS
# Create separate bar plots for each gender with count labels
plot_gender_0 <- ggplot(top_careers_gender_0, aes(x = count, y = reorder(career_combined, -count))) +
  geom_bar(stat = "identity", fill = "skyblue") +
  geom_text(aes(label = count), hjust = -0.2, size = 3) +  # Add count labels
  labs(title = "Top 4 Careers for Males", x = "Count", y = "Career") +
  theme_minimal() +
  theme(axis.text.y = element_text(angle = 0, hjust = 1, size = 10, face = "bold"))  # Adjust size and boldness

plot_gender_1 <- ggplot(top_careers_gender_1, aes(x = count, y = reorder(career_combined, -count))) +
  geom_bar(stat = "identity", fill = "pink") +
  geom_text(aes(label = count), hjust = -0.2, size = 3) +  # Add count labels
  labs(title = "Top 4 Careers for Females", x = "Count", y = "Career") +
  theme_minimal() +
  theme(axis.text.y = element_text(angle = 0, hjust = 1, size = 10, face = "bold"))  # Adjust size and boldness

# Arrange the plots side by side
plot_gender_0 + plot_gender_1



############################################################
###            Males and Females hObbies              ####
############################################################

# TOP ACTIVITIES FOR GENDERS "1" AND "0"
# Load necessary libraries
library(ggplot2)
library(dplyr)
library(tidyr)
# Select columns of interest
selected_columns <- c("age_o", "sports", "tvsports", "exercise", "dining", "museums", "art", "hiking", "gaming", "clubbing", "reading", "tv", "theater", "movies", "concerts", "music", "shopping", "yoga", "gender")
# Subset the data to selected columns
subset_data <- data[, selected_columns]
# Reshape the data for plotting
data_long <- tidyr::gather(subset_data, key = "variable", value = "value", -age_o, -gender)
# Ensuring 'value' is numeric
data_long$value <- as.numeric(data_long$value)
# Plot the data with facets for each gender
plot <- ggplot(data_long, aes(x = factor(age_o), fill = variable)) +
  geom_bar(position = "dodge", stat = "count") +
  labs(x = "Age Group", y = "Count", fill = "Variable") +
  ggtitle("Total Counts of Variables Based on age_o") +
  theme_minimal() +
  facet_grid(rows = vars(gender))
# Calculate the most common variable for each gender
most_common_variable <- data_long %>%
  group_by(gender, variable) %>%
  summarise(count = sum(value, na.rm = TRUE)) %>%
  arrange(desc(count)) %>%
  slice(1)  # Select the most common variable for each gender
# Print the most common variables
print(most_common_variable)
# Show the plot
print(plot)


# TOP 9 Similar Activities For Gender "1" or "0"
# Calculate the top activities for gender 0
top_activities_gender_0 <- data_long %>%
  filter(gender == 0) %>%
  group_by(variable) %>%
  summarise(count = sum(value, na.rm = TRUE)) %>%
  arrange(desc(count))
# Calculate the top activities for gender 1
top_activities_gender_1 <- data_long %>%
  filter(gender == 1) %>%
  group_by(variable) %>%
  summarise(count = sum(value, na.rm = TRUE)) %>%
  arrange(desc(count))
# Find the common top 10 variables
common_top_variables <- inner_join(
  head(top_activities_gender_0, 10),
  head(top_activities_gender_1, 10),
  by = "variable"
)
# Print the common top 10 variables
print("Common Top 10 Activities for Both Genders:")
print(common_top_variables)




############################################################
###            Males and Females By States/From        ####
############################################################


# GENDER "0" AND "1" FROM/STATE DEMOGRAPHIC INFORMATION
# Install necessary packages
if (!requireNamespace("knitr", quietly = TRUE)) {
  install.packages("knitr")
}
if (!requireNamespace("kableExtra", quietly = TRUE)) {
  install.packages("kableExtra")
}
# Load necessary libraries
library(dplyr)
library(knitr)
library(kableExtra)
# Assuming data frame is named 'data' and has 'gender' and 'from' columns
# Function to standardize state names
standardize_state <- function(state) {
  # Convert to lowercase and remove spaces
  cleaned_state <- tolower(gsub(" ", "", state))
  # Additional rules to handle variations
  cleaned_state <- gsub("newyor[kc]ity", "newyork", cleaned_state)
  cleaned_state <- gsub("nyc", "newyork", cleaned_state)
  cleaned_state <- gsub("losangeles", "california", cleaned_state)
  return(cleaned_state)
}
# Apply the standardize_state function to the 'from' column
data$from <- sapply(data$from, standardize_state)
# Group by gender and standardized state, calculate the count of occurrences, and arrange in descending order
grouped_data <- data %>%
  group_by(gender, from) %>%
  summarise(count = n()) %>%
  arrange(gender, desc(count))
# Find the top 15 states for each gender
top_states_male <- grouped_data %>%
  filter(gender == 0) %>%
  slice(1:15)
top_states_female <- grouped_data %>%
  filter(gender == 1) %>%
  slice(1:15)
# Find the common states
common_states <- intersect(top_states_male$from, top_states_female$from)
# Print the results in a table
cat("Top 15 states for Male (Gender 0):\n")
kable(top_states_male, format = "markdown")
cat("\nTop 15 states for Female (Gender 1):\n")
kable(top_states_female, format = "markdown")
cat("\nCommon states between Male (Gender 0) and Female (Gender 1):\n")
kable(data.frame(Common_States = common_states), format = "markdown")


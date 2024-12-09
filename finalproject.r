data <- read.csv("kaggle_survey_2020_responses.csv", stringsAsFactors = FALSE)
data <- data[-1, ]
library(dplyr)
library(ggplot2)
library(tidyr)

# q1
selected_data <- data %>%
    select(Q1, Q2, Q3, Q4, Q24) %>%
    rename(
        Age = Q1,
        Gender = Q2,
        Country = Q3,
        Education = Q4,
        Compensation = Q24
    )

# Clean and preprocess the data
# Convert compensation ranges into numeric midpoints
selected_data$Compensation <- gsub("[^0-9\\-]", "", selected_data$Compensation) %>%
    sapply(function(x) {
        if (grepl("-", x)) {
            bounds <- as.numeric(unlist(strsplit(x, "-")))
            return(runif(1, min = bounds[1], max = bounds[2]))
        } else if (x != "") {
            return(as.numeric(x))
        } else {
            return(NA)
        }
    })

# Remove rows with missing values in critical columns
selected_data <- selected_data %>%
    filter(!is.na(Age) & !is.na(Gender) & !is.na(Country) &
        !is.na(Education) & !is.na(Compensation))

# Calculate average income by age, gender, and country
avg_income_by_demo <- selected_data %>%
    group_by(Age, Gender, Country) %>%
    summarise(Average_Compensation = mean(Compensation, na.rm = TRUE), .groups = "drop")

# Calculate average income by education
avg_income_by_edu <- selected_data %>%
    group_by(Education) %>%
    summarise(Average_Compensation = mean(Compensation, na.rm = TRUE), .groups = "drop")

# Visualize: Income Distribution by Education Level
ggplot(selected_data, aes(x = Education, y = Compensation, fill = Gender)) +
    geom_boxplot() +
    labs(
        title = "Income Distribution by Education Level and Gender",
        x = "Education Level",
        y = "Compensation"
    ) +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Visualize: Average Income by Country
ggplot(avg_income_by_demo, aes(x = reorder(Country, -Average_Compensation), y = Average_Compensation, fill = Gender)) +
    geom_bar(stat = "identity", position = "dodge") +
    labs(
        title = "Average Income by Country and Gender",
        x = "Country",
        y = "Average Compensation"
    ) +
    theme(axis.text.x = element_text(angle = 90, hjust = 1))

# Visualize: Average Income by Age
ggplot(avg_income_by_demo, aes(x = Age, y = Average_Compensation, fill = Gender)) +
    geom_bar(stat = "identity", position = "dodge") +
    labs(
        title = "Average Income by Age and Gender",
        x = "Age",
        y = "Average Compensation",
        fill = "Gender"
    ) +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))

# q2
selected_data <- data %>%
    select(Q6, Q7_Part_1:Q7_Part_12) %>%
    rename(
        Experience = Q6,
        Python = Q7_Part_1,
        R = Q7_Part_2,
        SQL = Q7_Part_3,
        C = Q7_Part_4,
        Cpp = Q7_Part_5,
        Java = Q7_Part_6,
        JavaScript = Q7_Part_7,
        Julia = Q7_Part_8,
        Swift = Q7_Part_9,
        Bash = Q7_Part_10,
        MATLAB = Q7_Part_11,
        None = Q7_Part_12
    )

selected_data <- selected_data %>%
    filter(Experience != "I have never written code")
# Pivot table
pivot_data <- selected_data %>%
    pivot_longer(
        cols = Python:None, # Columns for programming languages
        names_to = "Language", # Create a new column for language names
        values_to = "Value" # Create a new column for values (e.g., Yes/No or other indicators)
    ) %>%
    mutate(Value = ifelse(Value != "", 1, 0)) %>% # Convert "Yes" to 1 and others to 0
    group_by(Experience, Language) %>%
    summarize(Count = sum(Value, na.rm = TRUE), .groups = "drop") %>%
    pivot_wider(
        names_from = Language, # Spread languages into columns
        values_from = Count, # Fill with counts
        values_fill = 0 # Fill missing values with 0
    )

# Print the final pivot table
pivot_data <- pivot_data[-1, ]
pivot_data

create_pie_chart_with_legend <- function(column_name) {
    values <- pivot_data[[column_name]]
    labels <- pivot_data$Experience
    total <- sum(values)

    # Calculate percentages
    percentages <- round(values / total * 100, 1)
    legend_labels <- paste(labels, "(", percentages, "%)", sep = "")

    # Generate pie chart
    pie(values,
        labels = NA, # Remove labels from the pie chart
        main = paste("Pie Chart for", column_name),
        col = rainbow(length(values))
    )

    # Add a legend
    legend("topright", legend = legend_labels, fill = rainbow(length(values)), cex = 0.8)
}

# Generate pie charts for each column
for (column in colnames(pivot_data)[-1]) {
    create_pie_chart_with_legend(column)
}

pivot_data_long <- pivot_data %>%
    pivot_longer(cols = -Experience, names_to = "Language", values_to = "Usage")
pivot_data_long <- pivot_data_long %>%
    group_by(Language) %>%
    mutate(Proportion = Usage / sum(Usage))
pivot_data_long <- pivot_data_long %>%
    group_by(Language) %>%
    mutate(Proportion = Usage / sum(Usage))

# Create proportional stacked bar chart
ggplot(pivot_data_long, aes(x = Language, y = Proportion, fill = Experience)) +
    geom_bar(stat = "identity", position = "fill") +
    labs(
        title = "Proportional Programming Language Usage by Experience Level",
        x = "Programming Language",
        y = "Proportion"
    ) +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))

selected_data <- data %>%
    select(Q5, Q7_Part_1:Q7_Part_12) %>%
    rename(
        Industry = Q5,
        Python = Q7_Part_1,
        R = Q7_Part_2,
        SQL = Q7_Part_3,
        C = Q7_Part_4,
        Cpp = Q7_Part_5,
        Java = Q7_Part_6,
        JavaScript = Q7_Part_7,
        Julia = Q7_Part_8,
        Swift = Q7_Part_9,
        Bash = Q7_Part_10,
        MATLAB = Q7_Part_11,
        None = Q7_Part_12
    )
selected_data

pivot_data <- selected_data %>%
    pivot_longer(
        cols = Python:None, # Columns for programming languages
        names_to = "Language", # Create a new column for language names
        values_to = "Value" # Create a new column for values (e.g., Yes/No or other indicators)
    ) %>%
    mutate(Value = ifelse(Value != "", 1, 0)) %>% # Convert "Yes" to 1 and others to 0
    group_by(Industry, Language) %>%
    summarize(Count = sum(Value, na.rm = TRUE), .groups = "drop") %>%
    pivot_wider(
        names_from = Language, # Spread languages into columns
        values_from = Count, # Fill with counts
        values_fill = 0 # Fill missing values with 0
    )

# Print the final pivot table
pivot_data <- pivot_data[-1, ]
pivot_data

pivot_data_long <- pivot_data %>%
    pivot_longer(cols = -Industry, names_to = "Language", values_to = "Usage")
pivot_data_long <- pivot_data_long %>%
    group_by(Language) %>%
    mutate(Proportion = Usage / sum(Usage))
pivot_data_long <- pivot_data_long %>%
    group_by(Language) %>%
    mutate(Proportion = Usage / sum(Usage))

# Create proportional stacked bar chart
ggplot(pivot_data_long, aes(x = Language, y = Proportion, fill = Industry)) +
    geom_bar(stat = "identity", position = "fill") +
    labs(
        title = "Proportional Programming Language Usage by Experience Level",
        x = "Programming Language",
        y = "Proportion"
    ) +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))

library(RColorBrewer)
# Define a larger, more varied color palette
color_palette <- brewer.pal(n = min(length(unique(pivot_data_long$Industry)), 12), "Set3")

# If more than 12 industries, expand the palette
if (length(unique(pivot_data_long$Industry)) > 12) {
    color_palette <- colorRampPalette(brewer.pal(12, "Set3"))(length(unique(pivot_data_long$Industry)))
}

# Plot with custom colors
ggplot(pivot_data_long, aes(x = Language, y = Proportion, fill = Industry)) +
    geom_bar(stat = "identity", position = "fill") +
    scale_fill_manual(values = color_palette) + # Apply the custom palette
    labs(
        title = "Proportional Programming Language Usage by Industry",
        x = "Programming Language",
        y = "Proportion"
    ) +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))


# q3
data <- read.csv("kaggle_survey_2020_responses.csv", stringsAsFactors = FALSE)
data <- data[-1, ]
head(data)

# Define columns representing advanced ML tools
advanced_ml_cols <- c("Q17_Part_6", "Q17_Part_7", "Q17_Part_8", "Q17_Part_9", "Q17_Part_10")

# Convert empty strings and whitespace to NA for the relevant columns
data <- data %>%
    mutate(across(all_of(advanced_ml_cols), ~ ifelse(. == "" | grepl("^\\s*$", .), NA, .)))

# Create a column to flag advanced ML users
data <- data %>%
    mutate(Advanced_User = rowSums(!is.na(select(., all_of(advanced_ml_cols)))) > 0)

# Verify the flagging
print(head(select(data, Advanced_User, all_of(advanced_ml_cols))))

# Check count of TRUE and FALSE in Advanced_User
print(table(data$Advanced_User))

library(dplyr)
library(tidyr)


# Define cloud platform columns
cloud_cols <- grep("^Q26_A_Part", names(data), value = TRUE)

# Replace empty strings with NA in cloud platform columns
data <- data %>%
    mutate(across(all_of(cloud_cols), ~ ifelse(. == "" | grepl("^\\s*$", .), NA, .)))

# Reshape data to long format for cloud platforms
cloud_data <- data %>%
    select(Advanced_User, all_of(cloud_cols)) %>%
    pivot_longer(
        cols = all_of(cloud_cols),
        names_to = "Cloud_Platform",
        values_to = "Used"
    ) %>%
    rename(
        Cloud_Platform = Cloud_Platform
    )

# Filter for respondents who selected a cloud platform (non-NA Used values)
cloud_data <- cloud_data %>%
    filter(!is.na(Used))

# Verify reshaped data
print(head(cloud_data))

# Count the number of Advanced_User respondents in the filtered data
advanced_user_counts <- cloud_data %>%
    count(Advanced_User)

# Print the results
print(advanced_user_counts)

# Group by Cloud_Platform and calculate metrics
cloud_summary <- cloud_data %>%
    group_by(Cloud_Platform) %>%
    summarise(
        Total_Users = n(), # Total number of users per platform
        Advanced_ML_Users = sum(Advanced_User, na.rm = TRUE) # Count of advanced ML users
    ) %>%
    mutate(
        Proportion = Advanced_ML_Users / Total_Users # Calculate proportion of advanced ML users
    ) %>%
    arrange(desc(Proportion)) # Sort by proportion in descending order

# Verify results
print(cloud_summary)

# Define cloud platform mapping
cloud_platform_mapping <- c(
    "Q26_A_Part_1" = "Amazon Web Services (AWS)",
    "Q26_A_Part_2" = "Microsoft Azure",
    "Q26_A_Part_3" = "Google Cloud Platform (GCP)",
    "Q26_A_Part_4" = "IBM Cloud / Red Hat",
    "Q26_A_Part_5" = "Oracle Cloud",
    "Q26_A_Part_6" = "SAP Cloud",
    "Q26_A_Part_7" = "Salesforce Cloud",
    "Q26_A_Part_8" = "VMware Cloud",
    "Q26_A_Part_9" = "Alibaba Cloud",
    "Q26_A_Part_10" = "Tencent Cloud",
    "Q26_A_Part_11" = "None",
    "Q26_A_OTHER" = "Other"
)

# Apply the mapping to rename Cloud_Platform codes to readable names
cloud_data$Cloud_Platform <- recode(cloud_data$Cloud_Platform, !!!cloud_platform_mapping)

# Create a bar graph of advanced ML users by cloud platform with renamed platforms
ggplot(cloud_data, aes(x = Cloud_Platform, fill = as.factor(Advanced_User))) +
    geom_bar(position = "fill") + # position="fill" stacks the bars to show proportions
    labs(
        title = "Proportion of Advanced ML Users by Cloud Platform",
        x = "Cloud Platform",
        y = "Proportion of Users",
        fill = "Advanced ML User"
    ) +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) # Rotate x-axis labels for readability


# q4

library(tidyverse)
library(scales)
# Include Q24 (compensation data) in experience_data
experience_data <- data %>%
    select(Q15, Q24, starts_with("Q37")) %>% # Include Q24 explicitly
    pivot_longer(
        cols = starts_with("Q37"),
        names_to = "Resource",
        values_to = "Used"
    ) %>%
    filter(!is.na(Used) & Used != "") %>% # Remove rows where the resource was not selected or is empty
    mutate(Q24 = ifelse(Q24 == "", "Unknown", Q24)) # Replace empty strings in Q24 with "Unknown"

# Verify the resulting data
head(experience_data)

# Define the ordered experience levels
ordered_experience_levels <- c(
    "Under 1 year", "1-2 years", "2-3 years", "3-4 years", "4-5 years",
    "5-10 years", "10-20 years", "20 or more years", "I do not use machine learning methods"
)

# Apply the ordering to Q15
experience_data <- experience_data %>%
    mutate(Q15 = factor(Q15, levels = ordered_experience_levels)) # Set factor levels for proper ordering

# Verify the ordering
levels(experience_data$Q15)

resource_mapping <- c(
    "Q37_Part_1" = "Coursera",
    "Q37_Part_2" = "edX",
    "Q37_Part_3" = "Kaggle Learn Courses",
    "Q37_Part_4" = "DataCamp",
    "Q37_Part_5" = "Fast.ai",
    "Q37_Part_6" = "Udacity",
    "Q37_Part_7" = "Udemy",
    "Q37_Part_8" = "LinkedIn Learning",
    "Q37_Part_9" = "Cloud-certification programs",
    "Q37_Part_10" = "University Courses",
    "Q37_Part_11" = "None",
    "Q37_OTHER" = "Other"
)

# Apply the mapping to the Resource column
experience_data <- experience_data %>%
    mutate(Resource = recode(Resource, !!!resource_mapping)) # Recode to human-readable names

# Verify the mapping
unique(experience_data$Resource)

# Calculate proportions of experience levels for each learning resource
experience_proportions <- experience_data %>%
    filter(!is.na(Q15)) %>% # Ensure no missing experience levels
    group_by(Resource, Q15) %>% # Group by resource and experience level
    summarise(Count = n(), .groups = "drop") %>% # Count occurrences
    group_by(Resource) %>% # Regroup by resource
    mutate(Proportion = Count / sum(Count)) %>% # Calculate proportions
    arrange(Resource, Q15) # Sort experience levels

# Verify the sorted proportions
head(experience_proportions)

# Create the plot for Q15 with sorted experience levels
ggplot(experience_proportions, aes(x = Resource, y = Proportion, fill = Q15)) +
    geom_bar(stat = "identity", position = "dodge") +
    scale_fill_manual(
        values = c(
            "Under 1 year" = "pink", "1-2 years" = "blue", "2-3 years" = "skyblue",
            "3-4 years" = "lightblue", "4-5 years" = "cyan", "5-10 years" = "green",
            "10-20 years" = "yellow", "20 or more years" = "orange",
            "I do not use machine learning methods" = "red"
        ),
        name = "Experience Level"
    ) +
    labs(
        title = "Proportion of Experience Levels by Learning Resource",
        x = "Learning Resource",
        y = "Proportion",
        fill = "Experience Level"
    ) +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Include Q24 (compensation data) in experience_data
experience_data <- data %>%
    select(Q15, Q24, starts_with("Q37")) %>% # Include Q24 explicitly
    pivot_longer(
        cols = starts_with("Q37"),
        names_to = "Resource",
        values_to = "Used"
    ) %>%
    filter(!is.na(Used) & Used != "") %>% # Remove rows where the resource was not selected or is empty
    mutate(Q24 = ifelse(Q24 == "", "Unknown", Q24)) # Replace empty strings in Q24 with "Unknown"

# Verify the resulting data
head(experience_data)

# Replace empty strings in Q24 with "Unknown"
compensation_data <- experience_data %>%
    mutate(Q24 = ifelse(Q24 == "", "Unknown", Q24))

# Verify the replacement
unique(compensation_data$Q24)

# Define the ordered compensation ranges
ordered_compensation_ranges <- c(
    "$0-999", "1,000-1,999", "2,000-2,999", "3,000-3,999", "4,000-4,999", "5,000-7,499",
    "7,500-9,999", "10,000-14,999", "15,000-19,999", "20,000-24,999", "25,000-29,999",
    "30,000-39,999", "40,000-49,999", "50,000-59,999", "60,000-69,999", "70,000-79,999",
    "80,000-89,999", "90,000-99,999", "100,000-124,999", "125,000-149,999", "150,000-199,999",
    "200,000-249,999", "250,000-299,999", "300,000-500,000", "> $500,000"
)

# Apply the ordering to Q24
compensation_data <- compensation_data %>%
    mutate(Q24 = factor(Q24, levels = ordered_compensation_ranges))

# Verify that Q24 is ordered correctly
levels(compensation_data$Q24)

# Define the mapping
resource_mapping <- c(
    "Q37_Part_1" = "Coursera",
    "Q37_Part_2" = "edX",
    "Q37_Part_3" = "Kaggle Learn Courses",
    "Q37_Part_4" = "DataCamp",
    "Q37_Part_5" = "Fast.ai",
    "Q37_Part_6" = "Udacity",
    "Q37_Part_7" = "Udemy",
    "Q37_Part_8" = "LinkedIn Learning",
    "Q37_Part_9" = "Cloud-certification programs",
    "Q37_Part_10" = "University Courses",
    "Q37_Part_11" = "None",
    "Q37_OTHER" = "Other"
)

# Apply the mapping
compensation_data <- compensation_data %>%
    mutate(Resource = recode(Resource, !!!resource_mapping))

# Verify the mapping
unique(compensation_data$Resource)

# Calculate proportions of compensation levels for each learning resource
compensation_proportions <- compensation_data %>%
    filter(!is.na(Q24)) %>% # Ensure no missing compensation levels
    group_by(Resource, Q24) %>% # Group by resource and compensation level
    summarise(Count = n(), .groups = "drop") %>% # Count occurrences
    group_by(Resource) %>% # Regroup by resource
    mutate(Proportion = Count / sum(Count)) %>% # Calculate proportions
    arrange(Resource, Q24) # Sort compensation levels

# Verify the proportions
head(compensation_proportions)

# Create the plot
ggplot(compensation_proportions, aes(x = Resource, y = Proportion, fill = Q24)) +
    geom_bar(stat = "identity", position = "dodge") +
    scale_fill_manual(
        values = colorRampPalette(c("blue", "green", "yellow", "red"))(length(ordered_compensation_ranges)),
        name = "Compensation Range"
    ) +
    labs(
        title = "Proportion of Compensation Ranges by Learning Resource",
        x = "Learning Resource",
        y = "Proportion",
        fill = "Compensation Range"
    ) +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))

# additional analysis
library(tidyverse)
library(cluster)
library(factoextra)
library(dplyr)
library(ggplot2)
library(scales)
data <- read.csv("E:/Classes/DSC 190/FinalProject/kaggle_survey_2020_responses.csv")

# Select relevant columns
columns <- c("Q15", "Q4", "Q24")
data <- data[, columns]

# Map Q15 (Experience) to numeric values
experience_mapping <- c(
    "Under 1 year" = 0.5,
    "1-2 years" = 1.5,
    "2-3 years" = 2.5,
    "3-4 years" = 3.5,
    "4-5 years" = 4.5,
    "5-10 years" = 7.5,
    "10-20 years" = 15,
    "20 or more years" = 20
)
data$Q15 <- as.numeric(recode(data$Q15, !!!experience_mapping))

# Map Q24 (Compensation) to numeric values
compensation_mapping <- c(
    "$0-999" = 500, "1,000-1,999" = 1500, "2,000-2,999" = 2500, "3,000-3,999" = 3500,
    "4,000-4,999" = 4500, "5,000-7,499" = 6250, "7,500-9,999" = 8750, "10,000-14,999" = 12500,
    "15,000-19,999" = 17500, "20,000-24,999" = 22500, "25,000-29,999" = 27500,
    "30,000-39,999" = 35000, "40,000-49,999" = 45000, "50,000-59,999" = 55000,
    "60,000-69,999" = 65000, "70,000-79,999" = 75000, "80,000-89,999" = 85000,
    "90,000-99,999" = 95000, "100,000-124,999" = 112500, "125,000-149,999" = 137500,
    "150,000-199,999" = 175000, "200,000-249,999" = 225000, "250,000-299,999" = 275000,
    "300,000-500,000" = 400000, "> $500,000" = 500000
)
data$Q24 <- as.numeric(recode(data$Q24, !!!compensation_mapping))

# Handle categorical variables
data$Q4 <- as.numeric(as.factor(data$Q4))

# Replace NA values with 0
data[is.na(data)] <- 0

# Preview the cleaned data
head(data)

# Standardize the features
data_scaled <- scale(data[, c("Q15", "Q24", "Q4")])

# Perform K-Means clustering
set.seed(42)
kmeans_model <- kmeans(data_scaled, centers = 4, nstart = 25)
data$Cluster <- as.factor(kmeans_model$cluster)

# Print cluster centers
print(kmeans_model$centers)

# Perform PCA
pca <- prcomp(data_scaled, center = TRUE, scale. = TRUE)
pca_data <- as.data.frame(pca$x[, 1:2])
pca_data$Cluster <- data$Cluster

# Visualize PCA with clusters
ggplot(pca_data, aes(x = PC1, y = PC2, color = Cluster)) +
    geom_point(size = 2, alpha = 0.8) +
    labs(
        title = "Career Pathway Clusters",
        x = "PCA Component 1",
        y = "PCA Component 2",
        color = "Cluster"
    ) +
    theme_minimal()

# Group data by cluster and calculate mean values
cluster_summary <- data %>%
    group_by(Cluster) %>%
    summarise(
        Avg_Experience = mean(Q15, na.rm = TRUE),
        Avg_Compensation = mean(Q24, na.rm = TRUE),
        Avg_Education = mean(Q4, na.rm = TRUE),
    )

# Print cluster summary
print(cluster_summary)

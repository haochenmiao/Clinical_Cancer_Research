# Load necessary packages
library(jsonlite)
library(dplyr)
library(tidyr)
library(purrr)
library(reshape2)
library(ggplot2)
library(ggmosaic)

# Set working directory
setwd("/Users/haochenmiao/Documents/Data_Sciecne_Projects/R_clinical_cancer")

# Load JSON data
json_data_clinical <- fromJSON("FM-AD Clinical Project 2024-09-07.json")
json_data_specimen <- fromJSON("FM-AD Biospecimen 2024-09-08.json")

# Check structure of both datasets
str(json_data_clinical)
str(json_data_specimen)

# Merge datasets on 'case_id'
merged_data <- left_join(json_data_clinical, json_data_specimen, by = "case_id")
str(merged_data)

# Clean data by replacing "not reported" with NA
clinical_data_clean <- json_data_clinical %>%
  mutate(across(where(is.character), ~na_if(.x, "not reported")))

biospecimen_data_clean <- json_data_specimen %>%
  mutate(across(where(is.character), ~na_if(.x, "not reported")))

# Unnest clinical data
clinical_data_clean <- clinical_data_clean %>%
  unnest_wider(diagnoses, names_repair = "minimal") %>%
  unnest_wider(demographic, names_repair = "minimal")

# Rename columns to ensure unique names using make.unique
colnames(clinical_data_clean) <- make.unique(colnames(clinical_data_clean))

clinical_data_clean <- clinical_data_clean %>%
  mutate(age_at_diagnosis_years = age_at_diagnosis / 365.25)

clinical_data_clean$gender <- as.factor(clinical_data_clean$gender)

# Extract numeric part of the morphology code
clinical_data_clean$morphology_numeric <- as.numeric(sub("/.*", "", clinical_data_clean$morphology))

# Remove NA values resulting from conversion
clinical_data_clean <- clinical_data_clean[!is.na(clinical_data_clean$morphology_numeric), ]

clinical_data_clean$classification_of_tumor <- as.factor(clinical_data_clean$classification_of_tumor)


# Aggregate data
ethnicity_counts <- clinical_data_clean %>%
  group_by(ethnicity) %>%
  summarise(count = n())

# Due to potentially many categories, select the top 10 most common primary diagnoses
top_diagnoses <- clinical_data_clean %>%
  count(primary_diagnosis) %>%
  top_n(10, n) %>%
  pull(primary_diagnosis)

# Filter the dataset
filtered_data <- clinical_data_clean %>%
  filter(primary_diagnosis %in% top_diagnoses)

# Due to potentially many categories, select the top 10 tissues
top_tissues <- clinical_data_clean %>%
  count(tissue_or_organ_of_origin) %>%
  top_n(10, n) %>%
  pull(tissue_or_organ_of_origin)

# Filter the dataset
filtered_data_tissue <- clinical_data_clean %>%
  filter(tissue_or_organ_of_origin %in% top_tissues)

# Filter out 'Not Reported' tumor grades
grade_filtered <- clinical_data_clean %>%
  filter(tumor_grade != "Not Reported")

# Count occurrences
site_counts <- clinical_data_clean %>%
  count(site_of_resection_or_biopsy) %>%
  arrange(desc(n))

# Select top 10 sites
top_sites <- site_counts %>%
  top_n(10, n)

# Check the column names after renaming to verify no duplicates
colnames(clinical_data_clean)


# Check the structure of cleaned data
str(clinical_data_clean)

# Create a contingency table
count_table <- table(clinical_data_clean$tumor_grade, clinical_data_clean$primary_diagnosis)

# Convert the table to a data frame
count_df <- as.data.frame(count_table)

# Rename columns for clarity
colnames(count_df) <- c("Tumor_Grade", "Primary_Diagnosis", "Count")

# Count occurrences
site_counts <- clinical_data_clean %>%
  count(site_of_resection_or_biopsy) %>%
  arrange(desc(n))

# Select top 10 sites
top_sites <- site_counts %>%
  top_n(10, n)



# Data visualization
ggplot(clinical_data_clean, aes(x = primary_diagnosis)) +
  geom_bar() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(title = "Distribution of Tumor Types", x = "Primary Diagnosis", y = "Count")

ggplot(clinical_data_clean, aes(x = gender, y = age_at_diagnosis_years)) +
  geom_boxplot() +
  labs(title = "Age at Diagnosis by Gender", x = "Gender", y = "Age at Diagnosis")

ggplot(clinical_data_clean, aes(x = primary_diagnosis, fill = gender)) +
  geom_bar(position = "stack") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(title = "Distribution of Tumor Types by Gender", x = "Primary Diagnosis", y = "Count")

ggplot(clinical_data_clean, aes(x = age_at_diagnosis_years)) +
  geom_histogram(binwidth = 1000, fill = "steelblue", color = "black") +
  labs(
    title = "Distribution of Age at Diagnosis",
    x = "Age at Diagnosis (years)",
    y = "Number of Patients"
  ) +
  theme_minimal()

ggplot(clinical_data_clean, aes(x = tumor_grade, y = age_at_diagnosis_years)) +
  geom_boxplot(fill = "tomato") +
  labs(
    title = "Age at Diagnosis by Tumor Grade",
    x = "Tumor Grade",
    y = "Age at Diagnosis (years)"
  ) +
  theme_minimal()


ggplot(clinical_data_clean, aes(x = gender, y = age_at_diagnosis_years, fill = gender)) +
  geom_violin(trim = FALSE) +
  labs(
    title = "Age at Diagnosis Distribution by Gender",
    x = "Gender",
    y = "Age at Diagnosis (years)"
  ) +
  theme_minimal() +
  theme(legend.position = "none")


ggplot(clinical_data_clean, aes(x = age_at_diagnosis_years, color = gender)) +
  geom_density(size = 1) +
  labs(
    title = "Density Plot of Age at Diagnosis by Gender",
    x = "Age at Diagnosis (years)",
    y = "Density"
  ) +
  theme_minimal()

ggplot(count_df, aes(x = Primary_Diagnosis, y = Tumor_Grade, fill = Count)) +
  geom_tile() +
  labs(
    title = "Heatmap of Tumor Grade vs. Primary Diagnosis",
    x = "Primary Diagnosis",
    y = "Tumor Grade"
  ) +
  scale_fill_gradient(low = "white", high = "darkred") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))


ggplot(clinical_data_clean, aes(x = morphology_numeric, y = age_at_diagnosis_years)) +
  geom_point(alpha = 0.6, color = "purple") +
  labs(
    title = "Age at Diagnosis vs. Morphology Code",
    x = "Morphology Code (Numeric Part)",
    y = "Age at Diagnosis (years)"
  ) +
  theme_minimal()


ggplot(clinical_data_clean, aes(x = age_at_diagnosis_years)) +
  geom_histogram(binwidth = 1000, fill = "darkcyan", color = "black") +
  labs(
    title = "Age at Diagnosis by Classification of Tumor",
    x = "Age at Diagnosis (years)",
    y = "Number of Patients"
  ) +
  facet_wrap(~ classification_of_tumor) +
  theme_minimal()

ggplot(clinical_data_clean, aes(x = gender, fill = tumor_grade)) +
  geom_bar(position = "stack") +
  labs(
    title = "Tumor Grade Distribution by Gender",
    x = "Gender",
    y = "Number of Patients",
    fill = "Tumor Grade"
  ) +
  theme_minimal()

ggplot(ethnicity_counts, aes(x = "", y = count, fill = ethnicity)) +
  geom_col(width = 1, color = "black") +
  coord_polar(theta = "y") +
  labs(
    title = "Ethnicity Distribution",
    x = NULL,
    y = NULL,
    fill = "Ethnicity"
  ) +
  theme_void() +
  theme(legend.position = "right")


ggplot(data = filtered_data) +
  geom_mosaic(aes(
    x = product(primary_diagnosis),
    fill = gender,
    weight = 1
  )) +
  labs(
    title = "Mosaic Plot of Primary Diagnosis and Gender",
    x = "Primary Diagnosis",
    y = "Proportion",
    fill = "Gender"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))


ggplot(filtered_data_tissue, aes(x = tissue_or_organ_of_origin, fill = classification_of_tumor)) +
  geom_bar() +
  labs(
    title = "Tumor Classification by Tissue of Origin",
    x = "Tissue or Organ of Origin",
    y = "Number of Patients",
    fill = "Tumor Classification"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


# Create the density plot
ggplot(grade_filtered, aes(x = age_at_diagnosis_years, color = tumor_grade)) +
  geom_density(size = 1) +
  labs(
    title = "Density Plot of Age at Diagnosis by Tumor Grade",
    x = "Age at Diagnosis (years)",
    y = "Density",
    color = "Tumor Grade"
  ) +
  theme_minimal()

ggplot(top_sites, aes(x = reorder(site_of_resection_or_biopsy, -n), y = n)) +
  geom_bar(stat = "identity", fill = "darkorange") +
  labs(
    title = "Top 10 Sites of Resection or Biopsy",
    x = "Site of Resection or Biopsy",
    y = "Number of Patients"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
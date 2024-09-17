# Load necessary libraries
library(jsonlite)
library(ggplot2)
library(dplyr)
library(tidyr)
library(purrr)

# Load the JSON data
biospecimen_data <- fromJSON("FM-AD Biospecimen 2024-09-08.json")

# Display the structure of the data before unnesting
str(biospecimen_data)

# Unnesting 'samples', 'portions', 'slides', and 'analytes' with disambiguation
biospecimen_clean <- biospecimen_data %>%
  unnest(samples, names_sep = "_") %>%
  unnest(samples_portions, names_sep = "_") %>%
  unnest(samples_portions_slides, names_sep = "_", keep_empty = TRUE) %>%
  unnest(samples_portions_analytes, names_sep = "_", keep_empty = TRUE) %>%
  unnest(samples_portions_analytes_aliquots, names_sep = "_", keep_empty = TRUE)

# Checking the structure after unnesting
str(biospecimen_clean)

# Optional: View the first few rows of the cleaned data to ensure correctness
head(biospecimen_clean)

colnames(biospecimen_clean)

# Aggregate data by tissue type
tissue_type_counts <- biospecimen_clean %>%
  count(samples_tissue_type) %>%
  arrange(desc(n))

# Visualizations

# Create the bar plot of tumor descriptors
ggplot(biospecimen_clean, aes(x = samples_tumor_descriptor)) +
  geom_bar(fill = "skyblue", color = "black") +
  labs(
    title = "Distribution of Tumor Descriptors",
    x = "Tumor Descriptor",
    y = "Count"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Create the pie chart
ggplot(tissue_type_counts, aes(x = "", y = n, fill = samples_tissue_type)) +
  geom_col(width = 1) +
  coord_polar(theta = "y") +
  labs(
    title = "Tissue Type Distribution",
    fill = "Tissue Type"
  ) +
  theme_void() +
  theme(legend.position = "right")

# Create the boxplot of percent tumor nuclei by tumor descriptor
ggplot(biospecimen_clean_filtered, aes(x = samples_tumor_descriptor, y = samples_portions_slides_percent_tumor_nuclei)) +
  geom_boxplot(fill = "tomato", color = "black") +
  labs(
    title = "Percent Tumor Nuclei by Tumor Descriptor",
    x = "Tumor Descriptor",
    y = "Percent Tumor Nuclei"
  ) +
  theme_minimal()


# Create the density plot of percent tumor nuclei
ggplot(biospecimen_clean, aes(x = samples_portions_slides_percent_tumor_nuclei)) +
  geom_density(fill = "lightgreen", color = "darkgreen") +
  labs(
    title = "Density Plot of Percent Tumor Nuclei",
    x = "Percent Tumor Nuclei",
    y = "Density"
  ) +
  theme_minimal()

# Create the violin plot
ggplot(biospecimen_clean, aes(x = samples_sample_type, y = samples_portions_slides_percent_tumor_nuclei, fill = samples_sample_type)) +
  geom_violin(trim = FALSE) +
  labs(
    title = "Percent Tumor Nuclei by Sample Type",
    x = "Sample Type",
    y = "Percent Tumor Nuclei"
  ) +
  theme_minimal() +
  theme(legend.position = "none")

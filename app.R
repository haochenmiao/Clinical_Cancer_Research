# Load necessary libraries
library(shiny)
library(ggplot2)
library(dplyr)
library(tidyr)
library(purrr)
library(DT)
library(shinydashboard)
library(treemap)
library(lubridate)

# Load JSON data for clinical and biospecimen datasets
json_data_clinical <- fromJSON("FM-AD Clinical Project 2024-09-07.json")
json_data_specimen <- fromJSON("FM-AD Biospecimen 2024-09-08.json")

# Check for duplicate column names and resolve them
colnames(json_data_clinical) <- make.unique(colnames(json_data_clinical))
colnames(json_data_specimen) <- make.unique(colnames(json_data_specimen))

# Clean and process the clinical data
clinical_data_clean <- json_data_clinical %>%
  mutate(across(where(is.character), ~na_if(.x, "not reported"))) %>%
  unnest_wider(diagnoses, names_repair = "minimal") %>%
  unnest_wider(demographic, names_repair = "minimal")

# Ensure unique column names after unnesting
colnames(clinical_data_clean) <- make.unique(colnames(clinical_data_clean))

clinical_data_clean <- clinical_data_clean %>%
  mutate(age_at_diagnosis_years = age_at_diagnosis / 365.25)

# Clean and process the biospecimen data
biospecimen_clean <- json_data_specimen %>%
  unnest(samples, names_sep = "_") %>%
  unnest(samples_portions, names_sep = "_") %>%
  unnest(samples_portions_slides, names_sep = "_", keep_empty = TRUE) %>%
  unnest(samples_portions_analytes, names_sep = "_", keep_empty = TRUE) %>%
  unnest(samples_portions_analytes_aliquots, names_sep = "_", keep_empty = TRUE)

# Ensure unique column names for biospecimen data
colnames(biospecimen_clean) <- make.unique(colnames(biospecimen_clean))

# Create ethnicity counts for the pie chart
ethnicity_counts <- clinical_data_clean %>%
  group_by(ethnicity) %>%
  summarise(count = n())

# Define UI layout
ui <- dashboardPage(
  dashboardHeader(title = "Cancer Data Analysis"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Overview", tabName = "overview", icon = icon("dashboard")),
      menuItem("Clinical Data", tabName = "clinical", icon = icon("hospital")),
      menuItem("Biospecimen Data", tabName = "biospecimen", icon = icon("flask"))
    )
  ),
  dashboardBody(
    tabItems(
      # Overview Tab
      tabItem(
        tabName = "overview",
        fluidRow(
          box(title = "Welcome to Cancer Data Analysis", width = 12,
              "This Shiny app provides an interactive view into the clinical and biospecimen data from the FM-AD project.")
        )
      ),
      # Clinical Data Tab
      tabItem(
        tabName = "clinical",
        fluidRow(
          box(title = "Distribution of Tumor Types", status = "primary", width = 6,
              plotOutput("tumor_distribution")),
          box(title = "Age at Diagnosis by Gender", status = "primary", width = 6,
              plotOutput("age_diagnosis_gender"))
        ),
        fluidRow(
          box(title = "Distribution of Tumor Types by Gender", status = "primary", width = 6,
              plotOutput("tumor_gender")),
          box(title = "Ethnicity Distribution", status = "primary", width = 6,
              plotOutput("ethnicity_distribution"))
        ),
        fluidRow(
          box(title = "Data Table - Clinical", status = "primary", width = 12,
              dataTableOutput("clinical_data_table"))
        )
      ),
      # Biospecimen Data Tab
      tabItem(
        tabName = "biospecimen",
        fluidRow(
          box(title = "Distribution of Tumor Descriptors", status = "primary", width = 6,
              plotOutput("tumor_descriptor")),
          box(title = "Tissue Type Distribution", status = "primary", width = 6,
              plotOutput("tissue_distribution"))
        ),
        fluidRow(
          box(title = "Percent Tumor Nuclei by Tumor Descriptor", status = "primary", width = 6,
              plotOutput("tumor_nuclei_descriptor")),
          box(title = "Density of Percent Tumor Nuclei", status = "primary", width = 6,
              plotOutput("density_tumor_nuclei"))
        ),
        fluidRow(
          box(title = "Data Table - Biospecimen", status = "primary", width = 12,
              dataTableOutput("biospecimen_data_table"))
        )
      )
    )
  )
)

# Define server logic
server <- function(input, output) {
  # Clinical Plots
  output$tumor_distribution <- renderPlot({
    ggplot(clinical_data_clean, aes(x = primary_diagnosis)) +
      geom_bar(fill = "skyblue") +
      theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
      labs(title = "Distribution of Tumor Types", x = "Primary Diagnosis", y = "Count")
  })
  
  output$age_diagnosis_gender <- renderPlot({
    ggplot(clinical_data_clean, aes(x = gender, y = age_at_diagnosis_years)) +
      geom_boxplot(fill = "coral") +
      labs(title = "Age at Diagnosis by Gender", x = "Gender", y = "Age at Diagnosis (Years)")
  })
  
  output$tumor_gender <- renderPlot({
    ggplot(clinical_data_clean, aes(x = primary_diagnosis, fill = gender)) +
      geom_bar(position = "stack") +
      theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
      labs(title = "Distribution of Tumor Types by Gender", x = "Primary Diagnosis", y = "Count")
  })
  
  output$ethnicity_distribution <- renderPlot({
    ggplot(ethnicity_counts, aes(x = "", y = count, fill = ethnicity)) +
      geom_col(width = 1) +
      coord_polar(theta = "y") +
      labs(title = "Ethnicity Distribution", x = NULL, y = NULL) +
      theme_void()
  })
  
  output$clinical_data_table <- renderDataTable({
    datatable(clinical_data_clean, options = list(scrollX = TRUE))
  })
  
  # Biospecimen Plots
  output$tumor_descriptor <- renderPlot({
    ggplot(biospecimen_clean, aes(x = samples_tumor_descriptor)) +
      geom_bar(fill = "lightblue", color = "black") +
      labs(title = "Distribution of Tumor Descriptors", x = "Tumor Descriptor", y = "Count") +
      theme_minimal()
  })
  
  output$tissue_distribution <- renderPlot({
    tissue_type_counts <- biospecimen_clean %>%
      count(samples_tissue_type) %>%
      arrange(desc(n))
    
    ggplot(tissue_type_counts, aes(x = "", y = n, fill = samples_tissue_type)) +
      geom_col(width = 1) +
      coord_polar(theta = "y") +
      labs(title = "Tissue Type Distribution", fill = "Tissue Type") +
      theme_void()
  })
  
  output$tumor_nuclei_descriptor <- renderPlot({
    ggplot(biospecimen_clean, aes(x = samples_tumor_descriptor, y = samples_portions_slides_percent_tumor_nuclei)) +
      geom_boxplot(fill = "tomato") +
      labs(title = "Percent Tumor Nuclei by Tumor Descriptor", x = "Tumor Descriptor", y = "Percent Tumor Nuclei") +
      theme_minimal()
  })
  
  output$density_tumor_nuclei <- renderPlot({
    ggplot(biospecimen_clean, aes(x = samples_portions_slides_percent_tumor_nuclei)) +
      geom_density(fill = "lightgreen") +
      labs(title = "Density of Percent Tumor Nuclei", x = "Percent Tumor Nuclei", y = "Density") +
      theme_minimal()
  })
  
  output$biospecimen_data_table <- renderDataTable({
    datatable(biospecimen_clean, options = list(scrollX = TRUE))
  })
}

# Run the application
shinyApp(ui = ui, server = server)

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
library(plotly)  # For interactive plots
library(highcharter)  # Advanced visualizations
library(dygraphs)  # For time series

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
              plotlyOutput("tumor_distribution")),
          box(title = "Age at Diagnosis by Gender", status = "primary", width = 6,
              plotlyOutput("age_diagnosis_gender"))
        ),
        fluidRow(
          box(title = "Distribution of Tumor Types by Gender", status = "primary", width = 6,
              plotlyOutput("tumor_gender")),
          box(title = "Age at Diagnosis by Tumor Classification", status = "primary", width = 6,
              plotlyOutput("age_histogram_classification"))
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
              plotlyOutput("tumor_descriptor")),
          box(title = "Percent Tumor Nuclei by Sample Type", status = "primary", width = 6,
              plotlyOutput("tumor_nuclei_violin"))
        ),
        fluidRow(
          box(title = "Percent Tumor Nuclei by Tumor Descriptor", status = "primary", width = 6,
              plotlyOutput("tumor_nuclei_descriptor")),
          box(title = "Density of Percent Tumor Nuclei", status = "primary", width = 6,
              plotlyOutput("density_tumor_nuclei"))
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
  output$tumor_distribution <- renderPlotly({
    p <- ggplot(clinical_data_clean, aes(x = primary_diagnosis)) +
      geom_bar(fill = "skyblue") +
      theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
      labs(title = "Distribution of Tumor Types", x = "Primary Diagnosis", y = "Count")
    ggplotly(p)
  })
  
  output$age_diagnosis_gender <- renderPlotly({
    p <- ggplot(clinical_data_clean, aes(x = gender, y = age_at_diagnosis_years)) +
      geom_boxplot(fill = "coral") +
      labs(title = "Age at Diagnosis by Gender", x = "Gender", y = "Age at Diagnosis (Years)")
    ggplotly(p)
  })
  
  output$tumor_gender <- renderPlotly({
    p <- ggplot(clinical_data_clean, aes(x = primary_diagnosis, fill = gender)) +
      geom_bar(position = "stack") +
      theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
      labs(title = "Distribution of Tumor Types by Gender", x = "Primary Diagnosis", y = "Count")
    ggplotly(p)
  })
  
  output$age_histogram_classification <- renderPlotly({
    p <- ggplot(clinical_data_clean, aes(x = age_at_diagnosis_years, fill = classification_of_tumor)) +
      geom_histogram(binwidth = 5, color = "black", position = "dodge") +
      labs(
        title = "Age at Diagnosis by Tumor Classification",
        x = "Age at Diagnosis (Years)",
        y = "Number of Patients"
      ) +
      scale_fill_brewer(palette = "Set1") +
      theme_minimal()
    
    ggplotly(p)
  })
  
  output$clinical_data_table <- renderDataTable({
    datatable(clinical_data_clean, options = list(scrollX = TRUE))
  })
  
  # Biospecimen Plots
  output$tumor_descriptor <- renderPlotly({
    p <- ggplot(biospecimen_clean, aes(x = samples_tumor_descriptor)) +
      geom_bar(fill = "lightblue", color = "black") +
      labs(title = "Distribution of Tumor Descriptors", x = "Tumor Descriptor", y = "Count") +
      theme_minimal()
    ggplotly(p)
  })
  
  output$tumor_nuclei_violin <- renderPlotly({
    p <- ggplot(biospecimen_clean, aes(x = samples_sample_type, y = samples_portions_slides_percent_tumor_nuclei, fill = samples_sample_type)) +
      geom_violin(trim = FALSE) +
      labs(
        title = "Percent Tumor Nuclei by Sample Type",
        x = "Sample Type",
        y = "Percent Tumor Nuclei"
      ) +
      theme_minimal() +
      theme(legend.position = "none")
    
    ggplotly(p)
  })
  
  output$tumor_nuclei_descriptor <- renderPlotly({
    p <- ggplot(biospecimen_clean, aes(x = samples_tumor_descriptor, y = samples_portions_slides_percent_tumor_nuclei)) +
      geom_boxplot(fill = "tomato") +
      labs(title = "Percent Tumor Nuclei by Tumor Descriptor", x = "Tumor Descriptor", y = "Percent Tumor Nuclei") +
      theme_minimal()
    ggplotly(p)
  })
  
  output$density_tumor_nuclei <- renderPlotly({
    p <- ggplot(biospecimen_clean, aes(x = samples_portions_slides_percent_tumor_nuclei)) +
      geom_density(fill = "lightgreen") +
      labs(title = "Density of Percent Tumor Nuclei", x = "Percent Tumor Nuclei", y = "Density") +
      theme_minimal()
    ggplotly(p)
  })
  
  output$biospecimen_data_table <- renderDataTable({
    datatable(biospecimen_clean, options = list(scrollX = TRUE))
  })
}

# Run the application
shinyApp(ui = ui, server = server)

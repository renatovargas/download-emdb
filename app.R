# MAFAP Emissions Database (EMDB)
# Renato Vargas (FAO Consultant)

library(shiny)
library(readr)
library(openxlsx)

# Define dataset options
file_choices <- list(
  "Total Emissions (Long Format)" = "gwp_total_emissions_long.rds",
  "Total Emissions (Pivot Format)" = "gwp_total_emissions_pivot.rds",
  "Total Emissions (All Fields)" = "gwp_total_emissions.rds"
)

ui <- fluidPage(
  titlePanel("MAFAP Emissions Data Downloader"),
  
  fluidRow(
    column(12,
           selectInput("dataset", "Choose a dataset:", choices = file_choices),
           uiOutput("country_ui"),
           uiOutput("year_ui"),
           downloadButton("download", "Download Country Data (.xlsx)")
    )
  )
)

server <- function(input, output, session) {
  
  # Reactive dataset based on user selection
  selected_data <- reactive({
    req(input$dataset)
    df <- readRDS(file.path("data", input$dataset))
    return(df)
  })
  
  # Update country choices based on dataset
  output$country_ui <- renderUI({
    df <- selected_data()
    selectInput("country", "Select a country:", choices = sort(unique(df$Area)))
  })
  
  # Update year choices based on dataset
  output$year_ui <- renderUI({
    df <- selected_data()
    req(input$country)
    df_country <- df[df$Area == input$country, ]
    min_year <- min(df_country$Year, na.rm = TRUE)
    max_year <- max(df_country$Year, na.rm = TRUE)
    
    sliderInput("years", "Select Year Range:",
                min = min_year, max = max_year,
                value = c(min_year, max_year),
                sep = "")
  })
  
  # Filter dataset based on user selections
  filtered_data <- reactive({
    req(input$country, input$years)
    df <- selected_data()
    df <- df[df$Area == input$country & df$Year >= input$years[1] & df$Year <= input$years[2], ]
    return(df)
  })
  
  # Download handler for Excel file
  output$download <- downloadHandler(
    filename = function() {
      req(input$country, input$years)
      df_country <- selected_data()[selected_data()$Area == input$country, ]
      iso3 <- unique(df_country$ISO3)[1]  # Extract first unique ISO3 value
      first_year <- input$years[1]
      last_year <- input$years[2]
      paste0("emdb-", iso3, "-", first_year, "-", last_year, ".xlsx")
    },
    content = function(file) {
      write.xlsx(filtered_data(), file)
    }
  )
}

shinyApp(ui, server)

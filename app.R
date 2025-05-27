library(shiny)
library(ggplot2)  # For plotting
library(DT)       # For interactive tables
library(dplyr)    # For data manipulation
library(tidyr)    # For data tidying
library(corrplot) # For correlation plots
library(shinycssloaders) # For loading spinners

# Define UI
ui <- fluidPage(
  # Add custom CSS
  tags$head(
    tags$style(HTML("
            .well {
                background-color: #f8f9fa;
                border: 1px solid #dee2e6;
                border-radius: 5px;
                padding: 20px;
                margin-bottom: 20px;
            }
            .shiny-input-container {
                margin-bottom: 15px;
            }
            .stat-box {
                background-color: #fff;
                border: 1px solid #ddd;
                border-radius: 4px;
                padding: 15px;
                margin-bottom: 15px;
            }
        "))
  ),
  
  # Application title
  titlePanel("Interactive Statistical Analysis Demo"),
  
  # Sidebar layout
  sidebarLayout(
    # Sidebar panel for inputs
    sidebarPanel(
      width = 3,
      # Data source selection
      radioButtons("data_source", "Select Data Source:",
                   choices = list(
                     "Random Data" = "random",
                     "Upload CSV" = "upload",
                     "Tumor Dataset" = "local"
                   ),
                   selected = "local"
      ),
      
      # Conditional file input
      conditionalPanel(
        condition = "input.data_source == 'upload'",
        fileInput("file", "Upload a CSV file:")
      ),
      
      # Sample size input (only for random data)
      conditionalPanel(
        condition = "input.data_source == 'random'",
        numericInput("sample_size", "Sample size for random data:",
                     value = 1000, min = 100, max = 10000)
      ),
      
      # Column selection (for uploaded/local data)
      conditionalPanel(
        condition = "input.data_source != 'random'",
        uiOutput("column_selector")
      ),
      
      # Color selection
      selectInput("color", "Choose plot color:",
                  choices = c("Red", "Blue", "Green", "Yellow", "Purple")),
      
      # Number of bins
      sliderInput("bins", "Number of histogram bins:",
                  min = 1, max = 50, value = 30),
      
      # Display options
      checkboxInput("show_plot", "Show plots", value = TRUE),
      checkboxInput("show_stats", "Show statistics", value = TRUE),
      checkboxInput("show_correlation", "Show correlation", value = TRUE)
    ),
    
    # Main panel for outputs
    mainPanel(
      width = 9,
      # Tabset panel
      tabsetPanel(
        # Welcome tab
        tabPanel("Welcome",
                 h2("Welcome to the Statistical Analysis Demo!"),
                 p("This app demonstrates various statistical capabilities."),
                 verbatimTextOutput("data_info")
        ),
        
        # Plot tab
        tabPanel("Plots",
                 conditionalPanel(
                   condition = "input.show_plot == true",
                   fluidRow(
                     column(6, withSpinner(plotOutput("distPlot"))),
                     column(6, withSpinner(plotOutput("boxPlot")))
                   ),
                   fluidRow(
                     column(6, withSpinner(plotOutput("qqPlot"))),
                     column(6, withSpinner(plotOutput("corrPlot")))
                   )
                 )
        ),
        
        # Statistics tab
        tabPanel("Statistics",
                 conditionalPanel(
                   condition = "input.show_stats == true",
                   h3("Summary Statistics"),
                   verbatimTextOutput("summary_stats"),
                   h3("Distribution Parameters"),
                   verbatimTextOutput("dist_params"),
                   h3("Hypothesis Test"),
                   verbatimTextOutput("hypothesis_test")
                 )
        ),
        
        # Data tab
        tabPanel("Data",
                 h3("Data Preview"),
                 withSpinner(DTOutput("data_preview")),
                 verbatimTextOutput("error_message")
        ),
        
        # About tab
        tabPanel("About",
                 h3("About this App"),
                 p("This is a demonstration of statistical analysis capabilities including:"),
                 tags$ul(
                   tags$li("Multiple data sources (Random, Upload, Local)"),
                   tags$li("Descriptive statistics"),
                   tags$li("Distribution analysis"),
                   tags$li("Correlation analysis"),
                   tags$li("Hypothesis testing"),
                   tags$li("Multiple visualization types"),
                   tags$li("Interactive data exploration")
                 )
        )
      )
    )
  )
)

# Define server logic
server <- function(input, output) {
  # Reactive data source
  data_source <- reactive({
    tryCatch({
      switch(input$data_source,
             "random" = {
               set.seed(123)
               data.frame(
                 x = rnorm(input$sample_size),
                 y = rnorm(input$sample_size),
                 z = rnorm(input$sample_size)
               )
             },
             "upload" = {
               req(input$file)
               read.csv(input$file$datapath)
             },
             "local" = {
               read.csv("brain_tumor_dataset.csv")
             }
      )
    }, error = function(e) {
      NULL
    })
  })
  
  # Dynamic column selector
  output$column_selector <- renderUI({
    data <- data_source()
    if (!is.null(data)) {
      selectInput("selected_column", "Select Column for Analysis:",
                  choices = names(data))
    }
  })
  
  # Data information
  output$data_info <- renderPrint({
    data <- data_source()
    if (is.null(data)) {
      cat("Error: Unable to load data\n")
    } else {
      cat("Data Source:", input$data_source, "\n")
      cat("Number of rows:", nrow(data), "\n")
      cat("Number of columns:", ncol(data), "\n")
      cat("Column names:", paste(names(data), collapse = ", "), "\n")
    }
  })
  
  # Generate histogram
  output$distPlot <- renderPlot({
    data <- data_source()
    req(data)
    
    if (input$data_source == "random") {
      col_name <- "x"
    } else {
      req(input$selected_column)
      col_name <- input$selected_column
    }
    
    ggplot(data, aes_string(x = col_name)) +
      geom_histogram(breaks = seq(min(data[[col_name]]), 
                                  max(data[[col_name]]), 
                                  length.out = input$bins + 1),
                     fill = tolower(input$color), 
                     color = "black") +
      theme_minimal() +
      labs(title = paste("Distribution of", col_name),
           x = "Value",
           y = "Count")
  })
  
  # Generate box plot
  output$boxPlot <- renderPlot({
    data <- data_source()
    req(data)
    
    if (input$data_source == "random") {
      data_long <- pivot_longer(data, cols = everything(), 
                                names_to = "variable", 
                                values_to = "value")
    } else {
      req(input$selected_column)
      data_long <- data.frame(
        variable = input$selected_column,
        value = data[[input$selected_column]]
      )
    }
    
    ggplot(data_long, aes(x = variable, y = value, fill = variable)) +
      geom_boxplot() +
      theme_minimal() +
      labs(title = "Box Plot",
           x = "Variable",
           y = "Value")
  })
  
  # Generate Q-Q plot
  output$qqPlot <- renderPlot({
    data <- data_source()
    req(data)
    
    if (input$data_source == "random") {
      col_name <- "x"
    } else {
      req(input$selected_column)
      col_name <- input$selected_column
    }
    
    ggplot(data, aes_string(sample = col_name)) +
      stat_qq() +
      stat_qq_line() +
      theme_minimal() +
      labs(title = paste("Q-Q Plot of", col_name),
           x = "Theoretical Quantiles",
           y = "Sample Quantiles")
  })
  
  # Generate correlation plot
  output$corrPlot <- renderPlot({
    data <- data_source()
    req(data)
    
    if (input$data_source == "random") {
      cor_matrix <- cor(data)
    } else {
      # For uploaded/local data, use all numeric columns
      numeric_cols <- sapply(data, is.numeric)
      if (sum(numeric_cols) > 1) {
        cor_matrix <- cor(data[, numeric_cols])
      } else {
        return(NULL)
      }
    }
    
    corrplot(cor_matrix, method = "circle", type = "upper",
             tl.col = "black", tl.srt = 45)
  })
  
  # Summary statistics
  output$summary_stats <- renderPrint({
    data <- data_source()
    req(data)
    
    if (input$data_source == "random") {
      summary(data)
    } else {
      req(input$selected_column)
      summary(data[[input$selected_column]])
    }
  })
  
  # Distribution parameters
  output$dist_params <- renderPrint({
    data <- data_source()
    req(data)
    
    if (input$data_source == "random") {
      col_name <- "x"
    } else {
      req(input$selected_column)
      col_name <- input$selected_column
    }
    
    x <- data[[col_name]]
    cat("Mean:", mean(x), "\n")
    cat("Median:", median(x), "\n")
    cat("Standard Deviation:", sd(x), "\n")
    cat("Variance:", var(x), "\n")
    cat("Skewness:", moments::skewness(x), "\n")
    cat("Kurtosis:", moments::kurtosis(x), "\n")
  })
  
  # Hypothesis test
  output$hypothesis_test <- renderPrint({
    data <- data_source()
    req(data)
    
    if (input$data_source == "random") {
      col_name <- "x"
    } else {
      req(input$selected_column)
      col_name <- input$selected_column
    }
    
    x <- data[[col_name]]
    t_test <- t.test(x, mu = 0)
    cat("One Sample t-test (H0: mean = 0)\n")
    print(t_test)
  })
  
  # Data preview
  output$data_preview <- renderDT({
    data <- data_source()
    req(data)
    datatable(data, options = list(pageLength = 5))
  })
  
  # Error message
  output$error_message <- renderPrint({
    data <- data_source()
    if (is.null(data)) {
      cat("Error: Unable to load data. Please check your data source and try again.")
    }
  })
}

# Run the application
shinyApp(ui = ui, server = server) 

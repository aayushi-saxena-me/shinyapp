library(shiny)
library(ggplot2)  # For plotting
library(DT)       # For interactive tables
library(dplyr)    # For data manipulation
library(tidyr)    # For data tidying
library(corrplot) # For correlation plots

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
            # Text input
            textInput("name", "Enter your name:", "User"),
            
            # Numeric input
            numericInput("age", "Enter your age:", 25, min = 0, max = 120),
            
            # Select input
            selectInput("color", "Choose your favorite color:",
                       choices = c("Red", "Blue", "Green", "Yellow", "Purple")),
            
            # Slider input
            sliderInput("bins", "Number of histogram bins:",
                       min = 1, max = 50, value = 30),
            
            # Date input
            dateInput("date", "Select a date:", value = Sys.Date()),
            
            # Checkbox inputs
            checkboxInput("show_plot", "Show plot", value = TRUE),
            checkboxInput("show_stats", "Show statistics", value = TRUE),
            checkboxInput("show_correlation", "Show correlation", value = TRUE),
            
            # File input
            fileInput("file", "Upload a CSV file:"),
            
            # Sample size input
            numericInput("sample_size", "Sample size for random data:",
                        value = 1000, min = 100, max = 10000)
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
                    verbatimTextOutput("user_info")
                ),
                
                # Plot tab
                tabPanel("Plots",
                    conditionalPanel(
                        condition = "input.show_plot == true",
                        fluidRow(
                            column(6, plotOutput("distPlot")),
                            column(6, plotOutput("boxPlot"))
                        ),
                        fluidRow(
                            column(6, plotOutput("qqPlot")),
                            column(6, plotOutput("corrPlot"))
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
                    DTOutput("data_table")
                ),
                
                # About tab
                tabPanel("About",
                    h3("About this App"),
                    p("This is a demonstration of statistical analysis capabilities including:"),
                    tags$ul(
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
    # Generate random data
    generate_data <- reactive({
        set.seed(123)
        data.frame(
            x = rnorm(input$sample_size),
            y = rnorm(input$sample_size),
            z = rnorm(input$sample_size)
        )
    })
    
    # Reactive expression for user info
    output$user_info <- renderPrint({
        cat("Name:", input$name, "\n")
        cat("Age:", input$age, "\n")
        cat("Favorite Color:", input$color, "\n")
        cat("Selected Date:", as.character(input$date), "\n")
    })
    
    # Generate histogram
    output$distPlot <- renderPlot({
        data <- generate_data()
        ggplot(data, aes(x = x)) +
            geom_histogram(breaks = seq(min(data$x), max(data$x), length.out = input$bins + 1),
                          fill = tolower(input$color), color = "black") +
            theme_minimal() +
            labs(title = "Distribution of X",
                 x = "Value",
                 y = "Count")
    })
    
    # Generate box plot
    output$boxPlot <- renderPlot({
        data <- generate_data()
        data_long <- pivot_longer(data, cols = everything(), names_to = "variable", values_to = "value")
        ggplot(data_long, aes(x = variable, y = value, fill = variable)) +
            geom_boxplot() +
            theme_minimal() +
            labs(title = "Box Plots of Variables",
                 x = "Variable",
                 y = "Value")
    })
    
    # Generate Q-Q plot
    output$qqPlot <- renderPlot({
        data <- generate_data()
        ggplot(data, aes(sample = x)) +
            stat_qq() +
            stat_qq_line() +
            theme_minimal() +
            labs(title = "Q-Q Plot of X",
                 x = "Theoretical Quantiles",
                 y = "Sample Quantiles")
    })
    
    # Generate correlation plot
    output$corrPlot <- renderPlot({
        data <- generate_data()
        cor_matrix <- cor(data)
        corrplot(cor_matrix, method = "circle", type = "upper",
                tl.col = "black", tl.srt = 45)
    })
    
    # Summary statistics
    output$summary_stats <- renderPrint({
        data <- generate_data()
        summary(data)
    })
    
    # Distribution parameters
    output$dist_params <- renderPrint({
        data <- generate_data()
        cat("Mean:", mean(data$x), "\n")
        cat("Median:", median(data$x), "\n")
        cat("Standard Deviation:", sd(data$x), "\n")
        cat("Variance:", var(data$x), "\n")
        cat("Skewness:", moments::skewness(data$x), "\n")
        cat("Kurtosis:", moments::kurtosis(data$x), "\n")
    })
    
    # Hypothesis test
    output$hypothesis_test <- renderPrint({
        data <- generate_data()
        t_test <- t.test(data$x, mu = 0)
        cat("One Sample t-test (H0: mean = 0)\n")
        print(t_test)
    })
    
    # Handle file upload and display data
    output$data_table <- renderDT({
        req(input$file)
        tryCatch({
            data <- read.csv(input$file$datapath)
            datatable(data, options = list(pageLength = 5))
        }, error = function(e) {
            datatable(data.frame(Message = "Please upload a valid CSV file"))
        })
    })
}

# Run the application
shinyApp(ui = ui, server = server) 
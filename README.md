# Hello World Shiny App

A simple demonstration of a Shiny web application.

## Prerequisites

1. Install R from [CRAN](https://cran.r-project.org/)
2. Install RStudio from [Posit](https://posit.co/download/rstudio-desktop/)
3. Install required R packages:
   ```R
   install.packages("shiny")
   ```

## Project Structure
```
ShinyApp/
├── app.R           # Main application file
├── www/            # Static files directory
│   └── styles.css  # Custom CSS styles
└── README.md       # This file
```

## Running the App

1. Open RStudio
2. Open the `app.R` file
3. Click the "Run App" button in the top right corner of the editor
   OR
   Run this command in the R console:
   ```R
   shiny::runApp()
   ```

## Features
- Simple "Hello World" demonstration
- Basic styling with custom CSS
- Responsive layout using Shiny's fluidPage 
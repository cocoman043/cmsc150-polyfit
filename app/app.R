#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
source("polynomial_regression.R")
source("quadratic_spline_interpolation.R")

# Define UI for application that draws a histogram
ui <- fluidPage(
  titlePanel("Fitness"),
  navbarPage("",
             tabPanel("User Manual",
                      mainPanel(
                        h2("Welcome to the Fitness app!"),
                        p("This is a user manual for the Shiny app. The app is divided into 4 tabs, each of which is a different tool."),
                        p("The first tab is the user manual. It is a simple tab that contains this user manual."),
                        p("The second tab is the curve fitting tab. It contains two tools: polynomial regression and quadratic spline interpolation."),
                        p("The third tab is the nutrition optimizer tab. It contains a tool that optimizes the nutrition of a diet."),
                      )
             ),
             navbarMenu("Curve Fitting",
                      tabPanel("Polynomial Regression",
                               sidebarLayout(
                                 sidebarPanel(
                                   titlePanel("Polynomial Regression"),
                                   fileInput("file1", "Choose CSV File",
                                             multiple = FALSE,
                                             accept = c("text/csv",
                                                        "text/comma-separated-values,text/plain",
                                                        ".csv")),
                                   numericInput("degree", "Degree of polynomial:", 1, min = 1, max = 10),
                                   numericInput("target", "x to estimate:", 1),
                                   actionButton("submit", "Submit")
                                 ),
                                 mainPanel(
                                   tableOutput("contents"),
                                   verbatimTextOutput("result"),
                                 )
                               )
                      ),
                      tabPanel("Quadratic Spline Interpolation",
                               sidebarLayout(
                                 sidebarPanel(
                                   titlePanel("Quadratic Spline Interpolation"),
                                   fileInput("file1", "Choose CSV File",
                                             multiple = FALSE,
                                             accept = c("text/csv",
                                                        "text/comma-separated-values,text/plain",
                                                        ".csv")),
                                   numericInput("target", "x to estimate:", 1),
                                   actionButton("submit", "Submit")
                                 ),
                                 mainPanel(
                                   tableOutput("contents"),
                                   verbatimTextOutput("result"),
                                 )
                               )
                      )
             ),
             tabPanel("Nutrition Optimizer",
                      sidebarLayout(
                        sidebarPanel(
                          checkboxGroupInput("diet", "Choose diet:",
                                             choices = list("Vegetarian" = "Vegetarian",
                                                            "Vegan" = "Vegan",
                                                            "Keto" = "Keto",
                                                            "Paleo" = "Paleo",
                                                            "Mediterranean" = "Mediterranean",
                                                            "Atkins" = "Atkins",
                                                            "Zone" = "Zone",
                                                            "Raw" = "Raw",
                                                            "Dukan" = "Dukan",
                                                            "Ultra-low-fat" = "Ultra-low-fat",
                                                            "HCG" = "HCG",
                                                            "Alkaline" = "Alkaline",
                                                            "Weight Watchers" = "Weight Watchers",
                                                            "South Beach" = "South Beach",
                                                            "MIND" = "MIND",
                                                            "TLC" = "TLC",
                                                            "Flat Belly" = "Flat Belly",
                                                            "Fertility" = "Fertility",
                                                            "Mayo Clinic" = "Mayo Clinic",
                                                            "Ornish" = "Ornish",
                                                            "Biggest Loser" = "Biggest Loser",
                                                            "Jenny Craig" = "Jenny Craig",
                                                            "Nutrisystem" = "Nutrisystem",
                                                            "Volumetrics" = "Volumetrics",
                                                            "Abs" = "Abs",
                                                            "Bodybuilding" = "Bodybuilding",
                                                            "DASH" = "DASH",
                                                            "Engine 2" = "Engine 2",
                                                            "Fast" = "Fast",
                                                            "Flexitarian" = "Flexitarian",
                                                            "HMR" = "HMR",
                                                            "Master Cleanse" = "Master Cleanse",
                                                            "Medifast" = "Medifast",
                                                            "Optavia" = "Optavia",
                                                            "Spark Solution" = "Spark Solution",
                                                            "Supercharged Hormone" = "Supercharged Hormone",
                                                            "TLC" = "TLC",
                                                            "Whole30" = "Whole30",
                                                            "WW" = "WW",
                                                            "Zone" = "Zone"),
                                             selected = "Vegetarian"),
                        ),
                        mainPanel(
                          tableOutput("contents"),
                          verbatimTextOutput("result"),
                        )
                      )
             )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  output$contents <- renderTable({
    
    req(input$file1)
    
    # when reading semicolon separated files,
    # having a comma separator causes `read.csv` to error
    tryCatch(
      {
        df <- read.csv(input$file1$datapath)
      },
      error = function(e) {
        # return a safeError if a parsing error occurs
        stop(safeError(e))
      }
    )
    
    return(df)
  })
  
  output$result <- renderPrint("Nothing to show yet.")
}

# Run the application 
shinyApp(ui = ui, server = server)

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
source("systemsolver.r")

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
                                   fileInput("poly_reg_file", "Choose CSV File",
                                             multiple = FALSE,
                                             accept = c("text/csv",
                                                        "text/comma-separated-values,text/plain",
                                                        ".csv")),
                                   numericInput("degree", "Degree of polynomial:", 1, min = 1, max = 10),
                                   numericInput("target", "x to estimate:", 1),
                                   submitButton("Submit")
                                 ),
                                 mainPanel(
                                   tableOutput("poly_reg_result"),
                                   verbatimTextOutput("result"),
                                   verbatimTextOutput("poly_reg_function_string"),
                                   verbatimTextOutput("poly_reg_estimate")
                                 )
                               )
                      ),
                      tabPanel("Quadratic Spline Interpolation",
                               sidebarLayout(
                                 sidebarPanel(
                                   titlePanel("Quadratic Spline Interpolation"),
                                   fileInput("qsi_file", "Choose CSV File",
                                             multiple = FALSE,
                                             accept = c("text/csv",
                                                        "text/comma-separated-values,text/plain",
                                                        ".csv")),
                                   numericInput("target", "x to estimate:", 1),
                                   submitButton("Submit")
                                 ),
                                 mainPanel(
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
                        )
                      )
             )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  output$poly_reg_result <- renderTable({
    tryCatch(
      {
        df <- read.csv(input$poly_reg_file$datapath)
        result <- PolynomialRegression(input$degree,df);
      },
      error = function(e) {
        # return a safeError if a parsing error occurs
        stop(safeError(e))
      }
    )

    return(df)
  })
  
  output$poly_reg_function_string <- renderText({
    tryCatch(
      {
        df <- read.csv(input$poly_reg_file$datapath)
        result <- PolynomialRegression(input$degree,df);
      },
      error = function(e) {
        # return a safeError if a parsing error occurs
        stop(safeError(e))
      }
    )

    return(result$polynomial_string)
  })
  
  output$poly_reg_estimate <- renderText({
    tryCatch(
      {
        df <- read.csv(input$poly_reg_file$datapath)
        result <- PolynomialRegression(input$degree,df);
      },
      error = function(e) {
        # return a safeError if a parsing error occurs
        stop(safeError(e))
      }
    )

    return(result$polynomial_function(input$target))
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)

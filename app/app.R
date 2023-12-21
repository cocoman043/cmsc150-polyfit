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
source("simplex.R")

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
                                   numericInput("poly_reg_target", "x to estimate:", 1),
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
                                   numericInput("qsi_target", "x to estimate:", 1),
                                   submitButton("Submit")
                                 ),
                                 mainPanel(
                                   tableOutput("qsi_result"),
                                   verbatimTextOutput("qsi_function_string"),
                                   verbatimTextOutput("qsi_estimate"),
                                 )
                               )
                      )
             ),
             tabPanel("Nutrition Optimizer",
                      sidebarLayout(
                        sidebarPanel(
                          checkboxGroupInput("choices", "Choose foods:",
                                             choiceNames = table$Food,
                                             choiceValues = as.character(1:64),
                          ),
                          submitButton("Submit"),
                          actionButton("select_all", "Select All"),
                          actionButton("clear_all", "Clear All"),
                        ),
                        mainPanel(
                          tableOutput("nutrition_result"),
                        )
                      )
             )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  observe({
    if(input$select_all > 0) {
      updateCheckboxGroupInput(session, "choices", "Choose foods:", choiceNames = table$Food, choiceValues = as.character(1:64), selected = table$Food)
    }
    if(input$clear_all > 0) {
      updateCheckboxGroupInput(session, "choices", "Choose foods:", choiceNames = table$Food, choiceValues = as.character(1:64))
    }
  })
  
  output$poly_reg_result <- renderTable({
    tryCatch(
      {
        df <- read.csv(input$poly_reg_file$datapath, header = FALSE)
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
        df <- read.csv(input$poly_reg_file$datapath, header = FALSE)
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
        df <- read.csv(input$poly_reg_file$datapath, header = FALSE)
        result <- PolynomialRegression(input$degree,df);
      },
      error = function(e) {
        # return a safeError if a parsing error occurs
        stop(safeError(e))
      }
    )

    return(result$polynomial_function(input$poly_reg_target))
  })
  
  output$qsi_result <- renderTable({
    tryCatch(
      {
        df <- read.csv(input$qsi_file$datapath, header = FALSE)
        result <- quadratic_spline_interpolation(df);
      },
      error = function(e) {
        # return a safeError if a parsing error occurs
        stop(safeError(e))
      }
    )

    return(df)
  })
  
  output$qsi_function_string <- renderPrint({
    tryCatch(
      {
        df <- read.csv(input$qsi_file$datapath, header = FALSE)
        result <- quadratic_spline_interpolation(df);
      },
      error = function(e) {
        # return a safeError if a parsing error occurs
        stop(safeError(e))
      }
    )

    return(result$function_strings)
  })
  
  output$qsi_estimate <- renderText({
    tryCatch(
      {
        index <- 1
        df <- read.csv(input$qsi_file$datapath, header = FALSE)
        while(input$qsi_target > df[index,1]){
          index <- index + 1
        }
        estimate <- quadratic_spline_interpolation(df)$functions[[index-1]](input$qsi_target);
        return(estimate)
      },
      error = function(e) {
        # return a safeError if a parsing error occurs
        stop(safeError(e))
      }
    )
  })
  
  output$nutrition_result <- renderTable({
    tryCatch(
      {
        result <- optimize(input$choices);
      },
      error = function(e) {
        # return a safeError if a parsing error occurs
        stop(safeError(e))
      }
    )

    return(result)
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
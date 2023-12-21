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
  navbarPage("",
             tabPanel("User Manual",
                      mainPanel(
                        HTML("
                        
  <h1>Fit’n’Fit</h1>

  <p>This application provides tools for curve fitting, specifically Polynomial Regression and Quadratic Spline Interpolation, as well as a feature to calculate the optimal serving amount of foods to satisfy required nutrition constraints.</p>

  <h2>Features</h2>

  <h3>1. Curve Fitting Tools</h3>

  <h4>a. Polynomial Regression</h4>
  <ul>
    <li>Upload a .csv file for your data;</li>
    <li>Choose the degree of the polynomial to fit your data accurately;</li>
    <li>Estimate values using the regression curve.</li>
  </ul>

  <h4>b. Quadratic Spline Interpolation</h4>
  <ul>
    <li>Upload a .csv file for your data;</li>
    <li>Utilize quadratic splines to interpolate between data points;</li>
    <li>Estimate values using the interpolation curve.</li>
  </ul>

  <h3>2. Nutrition Calculator</h3>
  <ul>
    <li>Choose your chosen food from the list of available foods.</li>
  </ul>

  <h2>Technologies Used</h2>

  <ul>
    <li>R</li>
    <li>Shiny</li>
  </ul>

  <p>Feel free to reach out if you have any questions or suggestions!</p>
")
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
                                   # submitButton("Submit")
                                 ),
                                 mainPanel(
                                   tabsetPanel(
                                     id = 1,
                                     tabPanel("Result",
                                              verbatimTextOutput("poly_reg_function_string"),
                                              verbatimTextOutput("poly_reg_estimate")
                                     ),
                                     tabPanel("Input Data",
                                              tableOutput("poly_reg_result"),
                                     ),
                                   )
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
                                   # submitButton("Submit")
                                 ),
                                 mainPanel(
                                   tabsetPanel(
                                     id = 2,
                                     tabPanel("Result",
                                              verbatimTextOutput("qsi_estimate"),
                                              verbatimTextOutput("qsi_function_string"),
                                     ),
                                     tabPanel("Input Data",
                                              tableOutput("qsi_result"),
                                     ),
                                   )
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
                                             selected = as.character(c(21, 11, 12, 22, 29, 48, 44, 56, 38, 52, 58, 4, 6, 23, 28, 36, 5, 61, 62, 35)),
                          ),
                          actionButton("select_all", "Select All"),
                          actionButton("clear_all", "Clear All"),
                          # submitButton("Submit")
                        ),
                        mainPanel(
                          tabsetPanel(
                            id = 0,
                            tabPanel("Recomended Servings",
                                     tableOutput("nutrition_result")
                                     ),
                            tabPanel("Basic Solutions",
                                     verbatimTextOutput("nutrition_solutions")
                                     ),
                          )
                        )
                      )
             )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {
  
  observeEvent(input$clear_all,{
    updateCheckboxGroupInput(session,"choices", selected = character(0))
  })
  observeEvent(input$select_all, {      
    updateCheckboxGroupInput(session,"choices", selected = as.character(1:64))
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
        if (input$degree > nrow(df)-1) {
          stop("Degree of polynomial is too high.")
        }
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
        if (input$poly_reg_target < min(df$V1) || input$poly_reg_target > max(df$V1)) {
          stop("Target value is out of range.")
        }
        if (input$degree > nrow(df)-1) {
          stop("Degree of polynomial is too high.")
        }
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
        if (input$qsi_target < min(df$V1) || input$qsi_target > max(df$V1)) {
          stop("Target value is out of range.")
        }
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

    return(result$result)
  })
  
  output$nutrition_solutions <- renderPrint({
    tryCatch(
      {
        result <- optimize(input$choices);
      },
      error = function(e) {
        # return a safeError if a parsing error occurs
        stop(safeError(e))
      }
    )

    return(result$solutions)
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
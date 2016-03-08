library(shiny)

shinyUI(fluidPage(
  
  fluidRow( 
    column(2,
                   helpText("First peak"),
                   br(),
                   uiOutput("gamma_1"),
                   uiOutput("sigma_1"),
                   uiOutput("x0_1"),
                   uiOutput("A_1")
  ),
    column(2,
           helpText("Second peak"),
           br(),
           uiOutput("gamma_2"),
           uiOutput("sigma_2"),
           uiOutput("x0_2"),
           uiOutput("A_2")
    ),
  column(8,
         plotOutput("plot", width = "80%", height = "600px")
  )
  )
))




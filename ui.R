library(shiny)
library(RcppFaddeeva)
shinyUI(fluidPage(
  
  fluidRow(  plotOutput("plot", width = "100%", height = "400px"),
             
             hr(),
             
             column(3, selectInput("number", label = h3("Peaks"), 
                                   choices = list("One Voigt" = 1, 
                                                  "Two Voigts" = 2, 
                                                  "Three Voigts" = 3), selected = 1),
                    fileInput('file1', 'Choose file to upload',
                              accept = c(
                                'text/csv',
                                'text/comma-separated-values',
                                'text/tab-separated-values',
                                'text/plain',
                                '.csv',
                                '.tsv'
                              )),
                    sliderInput("xlim", label = h3("range"), min = 300, 
                                max = 900, value = c(400, 800)),
                    checkboxInput("fit", "Show fit", FALSE)
                    ),
                    column(2, uiOutput("sigma")), 
                    column(2, uiOutput("gamma")),
                    column(2, uiOutput("x0")), 
                    column(2, uiOutput("amplitude"))
             )
  ))
  
  
  
  

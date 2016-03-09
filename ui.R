library(shiny)

shinyUI(fluidPage(
  
  fluidRow(  plotOutput("plot", width = "100%", height = "400px"),
             
             hr(),
             
             column(3, selectInput("number", label = h3("Peaks"), 
                                   choices = list("One Voigt" = 1, 
                                                  "Two Voigts" = 2, 
                                                  "Three Voigts" = 3), selected = 1)),
             column(2, uiOutput("sigma")), 
             column(2, uiOutput("gamma")),
             column(2, uiOutput("x0")), 
             column(2, uiOutput("amplitude"))
  )
))




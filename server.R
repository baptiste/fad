
source('./helper/fad.R')

# set mirror
options(repos=structure(c(CRAN="http://cran.rstudio.com")))

if (!("shiny" %in% names(installed.packages()[,"Package"]))) {install.packages("shiny")}
suppressMessages(library(shiny, quietly = TRUE))

if (!("RcppFaddeeva" %in% names(installed.packages()[,"Package"]))) {install.packages("RcppFaddeeva")}
suppressMessages(library(RcppFaddeeva, quietly = TRUE))

if (!("openintro" %in% names(installed.packages()[,"Package"]))) {install.packages("openintro")}
suppressMessages(library(openintro, quietly = TRUE))


shinyServer(function(input, output)
{ 
  
  ## first peak
  
  output$x0_1 = renderUI(
    {
      
      sliderInput("x0_1",
                  "x0",
                  value = 543,
                  min = 0,
                  max = 1000)
      
    })
  
  output$gamma_1 = renderUI(
  {
  
      sliderInput("gamma_1",
                  "Gamma",
                  value = 685,
                  min = 1,
                  max = 1000)
   
  })
    
  output$sigma_1 = renderUI(
  {
  
      sliderInput("sigma_1",
                  "Sigma",
                  value = 860,
                  min = 1,
                  max = 1000)
    
  })
  
  output$A_1 = renderUI(
    {
      
      sliderInput("A_1",
                  "Amplitude",
                  value = 0.7,
                  min = 0,
                  max = 1)
      
    })
  ## second peak
  
  output$x0_2 = renderUI(
    {
      
      sliderInput("x0_2",
                  "x0",
                  value = 590,
                  min = 0,
                  max = 1000)
      
    })
  
  output$gamma_2 = renderUI(
    {
      
      sliderInput("gamma_2",
                  "Gamma",
                  value = 20,
                  min = 1,
                  max = 1000)
      
    })
  
  output$sigma_2 = renderUI(
    {
      
      sliderInput("sigma_2",
                  "Sigma",
                  value = 600,
                  min = 1,
                  max = 1000)
      
    })
  
  output$A_2 = renderUI(
    {
      
      sliderInput("A_2",
                  "Amplitude",
                  value = 1,
                  min = 0,
                  max = 1)
      
    })
  
  ############
  # Plotting #
  ############
  
  output$plot = renderPlot(
  { 

   fad(gamma=c(input$gamma_1,input$gamma_2),
       sigma=c(input$sigma_1,input$sigma_2),
       x0=c(input$x0_1,input$x0_2), A=c(input$A_1, input$A_2))
   
  
  })

  ################
  # Calculations #
  ################

  output$area = renderText(
  {

  })
})

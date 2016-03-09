
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
  
  
  output$sigma <- renderUI({
    pvars <- seq_len(input$number)
    lapply(seq_along(pvars), function(i) {
      sliderInput(inputId = paste0("sigma_", pvars[i]),
                  "Sigma",
                  value = 200,
                  min = 0,
                  max = 2000)
    })
    
  })
  
  output$gamma <- renderUI({
    pvars <- seq_len(input$number)
    lapply(seq_along(pvars), function(i) {
      sliderInput(inputId = paste0("gamma_", pvars[i]),
                  "Gamma",
                  value = 200,
                  min = 0,
                  max = 2000)
    })
    
  })
  
  output$x0 <- renderUI({
    pvars <- seq_len(input$number)
    lapply(seq_along(pvars), function(i) {
      sliderInput(inputId = paste0("x0_", pvars[i]),
                  "x0",
                  value = 590,
                  min = 400,
                  max = 700)
    })
    
  })
  
  
  output$amplitude <- renderUI({
    pvars <- seq_len(input$number)
    lapply(seq_along(pvars), function(i) {
      
      sliderInput(inputId = paste0("A_", pvars[i]),
                  "Amplitude",
                  value = 1,
                  min = 0,
                  max = 1)
    })
    
  })
  
  ############
  # Plotting #
  ############
  
  output$plot = renderPlot(
    { 
      
      inFile <- input$file1
      
      if (is.null(inFile)) {
        d <- readRDS("./helper/cv.rds")
        } else {
        d <- read.csv(inFile$datapath)
      }
               
               
      n <- seq_len(input$number)
      sigmas <- sapply(paste0("sigma_", n), function(g) input[[g]])
      gammas <- sapply(paste0("gamma_", n), function(g) input[[g]])
      x0s <- sapply(paste0("x0_", n), function(g) input[[g]])
      As <- sapply(paste0("A_", n), function(g) input[[g]])

      d <- fad(gamma=gammas,
          sigma=sigmas,
          x0=x0s,
          A=As, dat=d)
      plot_fad(d, xlim= input$xlim)

      if(input$fit){
        p0 <- c(gammas, sigmas, x0s, As)
        pf <- optim(p0, cost, d=d)
        ym <- model(pf$par, d)
        lines(d[,1], ym/max(ym),lwd=2)
      }
      
    })
  
  
})

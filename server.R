
source('./helper/fad.R')
library(plotly)
# set mirror
options(repos=structure(c(CRAN="http://cran.rstudio.com")))

if (!("shiny" %in% names(installed.packages()[,"Package"]))) {install.packages("shiny")}
suppressMessages(library(shiny, quietly = TRUE))

if (!("RcppFaddeeva" %in% names(installed.packages()[,"Package"]))) {install.packages("RcppFaddeeva")}
suppressMessages(library(RcppFaddeeva, quietly = TRUE))

if (!("openintro" %in% names(installed.packages()[,"Package"]))) {install.packages("openintro")}
suppressMessages(library(openintro, quietly = TRUE))

if (!("plotly" %in% names(installed.packages()[,"Package"]))) {install.packages("plotly")}
suppressMessages(library(plotly, quietly = TRUE))

if (!("magrittr" %in% names(installed.packages()[,"Package"]))) {install.packages("magrittr")}
suppressMessages(library(magrittr, quietly = TRUE))

if (!("tidyr" %in% names(installed.packages()[,"Package"]))) {install.packages("tidyr")}
suppressMessages(library(tidyr, quietly = TRUE))

# default data
d <- readRDS("./helper/cv.rds")


shinyServer(function(input, output)
{ 
  
  defaults <- list(x0 = c(550, 590, 650),
                   sigma = c(150, 200, 200),
                   gamma = c(200,200,200),
                   amplitude = c(0.5,1,1))
                   
  output$sigma <- renderUI({
    pvars <- seq_len(input$number)
    lapply(seq_along(pvars), function(i) {
      sliderInput(inputId = paste0("sigma_", pvars[i]),
                  "Sigma",
                  value = defaults$sigma[i],
                  min = 0,
                  max = 2000)
    })
    
  })
  
  output$gamma <- renderUI({
    pvars <- seq_len(input$number)
    lapply(seq_along(pvars), function(i) {
      sliderInput(inputId = paste0("gamma_", pvars[i]),
                  "Gamma",
                  value = defaults$gamma[i],
                  min = 0,
                  max = 2000)
    })
    
  })
  
  output$x0 <- renderUI({
    pvars <- seq_len(input$number)
    lapply(seq_along(pvars), function(i) {
      sliderInput(inputId = paste0("x0_", pvars[i]),
                  "x0",
                  value = defaults$x0[i],
                  min = 400,
                  max = 700)
    })
    
  })
  
  
  output$amplitude <- renderUI({
    pvars <- seq_len(input$number)
    lapply(seq_along(pvars), function(i) {
      
      sliderInput(inputId = paste0("A_", pvars[i]),
                  "Amplitude",
                  value = defaults$amplitude[i],
                  min = 0,
                  max = 1)
    })
    
  })
  
  ############
  # Plotting #
  ############
  
  output$Plot1 = renderPlotly({
    
    n <- seq_len(input$number)
    sigmas <- sapply(paste0("sigma_", n), function(g) input[[g]])
    gammas <- sapply(paste0("gamma_", n), function(g) input[[g]])
    x0s <- sapply(paste0("x0_", n), function(g) input[[g]])
    As <- sapply(paste0("A_", n), function(g) input[[g]])
    # input file, otherwise default
    inFile <- input$file1
    if (!is.null(inFile)) {
      d <- read.csv(inFile$datapath)
    }
    tmp <- fad(gamma=gammas,
             sigma=sigmas,
             x0=x0s,
             A=As, dat=d)
    sp <- tidyr::gather(tmp, var, value, -wavelength)
    
    plot_ly(sp, x = ~wavelength, y = ~value, color = ~var) %>%
      add_lines() 
  })
  
  output$Plot2 = renderPlotly({
    
    n <- seq_len(input$number)
    sigmas <- sapply(paste0("sigma_", n), function(g) input[[g]])
    gammas <- sapply(paste0("gamma_", n), function(g) input[[g]])
    x0s <- sapply(paste0("x0_", n), function(g) input[[g]])
    As <- sapply(paste0("A_", n), function(g) input[[g]])
    # input file, otherwise default
    inFile <- input$file1
    if (!is.null(inFile)) {
      d <- read.csv(inFile$datapath)
    }
    tmp <- fad(gamma=gammas,
               sigma=sigmas,
               x0=x0s,
               A=As, dat=d)
    plot_ly(tmp, x = ~wavelength, y = ~data) %>%
      add_lines(name = ~"data")%>%
      add_lines(y = ~model, name = ~"model")
  })

  
  #     if(input$fit){
  #       p0 <- c(gammas, sigmas, x0s, As)
  #       pf <- optim(p0, cost, d=d)
  #       ym <- model(pf$par, d)
  #       lines(d[,1], ym/max(ym),lwd=2)
  #     }
  #     
  #   })
  
  
})

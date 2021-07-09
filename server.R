server <- function(input, output, session) {  
  
  # set up buttons ----------------------------------------------------------
  shinyjs::disable(id="simulate")
  
  # click through "next" buttons --------------------------------------------
  observeEvent(input$next1, {
    updateCollapse(
      session, id = "mainCollapse", 
      open = "Select Variables", 
      close = "Upload Data"
    )
  })
  
  observeEvent(input$next2, {
    updateCollapse(
      session, id = "mainCollapse", 
      open = "Select Sample and Effect Size", 
      close = "Select Variables"
    )
  })
  
  observeEvent(input$next3, {
    updateCollapse(
      session, id = "mainCollapse",
      open = "Select Simulation Parameters", 
      close = "Select Sample and Effect Size"
    )
  })
  
  observeEvent(input$next4, {
    updateCollapse(
      session, id = "mainCollapse",
      open = "Run Simulation",
      close = "Select Simulation Parameters"
    )
  })
  
  # grab data from file -----------------------------------------------------
  header <- eventReactive(input$header, {
    updatePickerInput(
      session = session,
      inputId = "predictorVars", 
      label = NULL,
      choices = NULL
    )
    
    updatePickerInput(
      session = session,
      inputId = "outcomeVar", 
      label = NULL,
      choices = NULL
    )
    
    if_else(input$header == "Yes", TRUE, FALSE)
  })
  
  dfFull <- eventReactive(input$file, {       
    req(input$file)
    df <- read.csv(input$file$datapath, header = header()) ## could update to allow for diff file formats 
    shinyjs::enable(id="next1")
    vars <- names(df)
    
    updatePickerInput(
      session = session,
      inputId = "predictorVars", 
      label = NULL,
      choices = vars, 
      selected = NULL
    )
    
    updatePickerInput(
      session = session,
      inputId = "outcomeVar", 
      label = NULL,
      choices = vars, 
      selected = NULL
    )
    
    df
  })
  
  observeEvent(input$predictorVars, {
    varsAvail <- setdiff(names(dfFull()), input$predictorVars)
    updatePickerInput(
      session = session,
      inputId = "outcomeVar", 
      label = NULL,
      choices = varsAvail
    )
  }, ignoreNULL = FALSE)
  
  observeEvent(input$outcomeVar, {
    varsAvail <- setdiff(names(dfFull()), input$outcomeVar)
    selected <- setdiff(input$predictorVars, input$outcomeVar)
    
    updatePickerInput(
      session = session,
      inputId = "predictorVars", 
      label = NULL,
      choices = varsAvail, 
      selected = selected
    )
  })
  
  observeEvent(c(input$predictorVars, input$outcomeVar), {
    if (length(input$predictorVars) >= 1 & length(input$outcomeVar) == 1) {
      shinyjs::hide("helpTextSim")
      shinyjs::enable("simulate")
    } else{
      shinyjs::show("helpTextSim")
      shinyjs::disable("simulate")
    }
  })
  
  
  df <- reactive({ 
    req(dfFull(), input$outcomeVar)
    subset(dfFull(), select = c(input$predictorVars, input$outcomeVar)) 
  })
  
  
  # display data selected --------------------------------------------------
  
  output$dataTable <-  DT::renderDataTable({ df() })
  
  
  # get sample and effect size(s) -N----------------------------------------
  N <- reactive({
    if(input$ssOrEs == "Sample Size"){
      ss1 <- as.numeric(input$ss[1])
      ss2 <- as.numeric(input$ss[2])
      seq(from = ss1, to = ss2, by = (ss2-ss1)/10)
    } else {
      as.numeric(input$ss)
    }
  })
  
  es <- reactive({
    if(input$ssOrEs == "Sample Size"){
      as.numeric(input$es)
    } else {
      es1 <- as.numeric(input$es[1])
      es2 <- as.numeric(input$es[2])
      seq(from = es1, to = es2, by = (es2-es1)/10)
    }
  })
  
  
  # simulate ----------------------------------------------------------------
  results <- eventReactive(input$simulate, {
    
    logOut <- if_else(input$logOutcome == "Yes", TRUE, FALSE)
    
    # simulate
    withProgress(message = "Running Power Simulation", value = 0, {
      results <- simulatePower(
        df(),
        input$alpha, 
        input$sims, 
        es(), 
        N(), 
        input$outcomeVar, 
        input$predictorVars, 
        logOutcome = logOut
      )
      
    })
    
    shinyjs::hide("helpTextOuputs")
    
    results
  })
  
  # show results ------------------------------------------------------------
  output$resultsTable <- DT::renderDataTable({ 
    req(length(results()) > 0 )
    results() %>%
      datatable() %>%
      formatRound(columns=colnames(results())[2], digits=3)
  })
  
  output$powerPlot <- renderPlotly({
    req(length(results()) > 0 )
    if(input$ssOrEs == "Sample Size"){
      x <- N()
      xlab <- "Sample Size"
      title <- paste("Power Simulations for Effect =", input$es)
    }
    else{
      x <- es()
      xlab <- "Effect Size"
      title <- paste("Power Simulations for Sample Size =", input$ss)
    }
    
    p <- results() %>% 
      mutate(text = paste0("Sample size: ", x, "\nPower: ", powers)) %>%
      ggplot(aes(x = x, y = powers)) +
      geom_point(aes(text = text), colour=pal["blue"]) + 
      geom_line(colour=pal["blue"]) + 
      theme(text = element_text(size=20))+
      geom_hline(
        yintercept = 0.8,
        colour = pal["orange"],
        linetype = "dashed",
        size = 1.2) +
      labs(x=xlab, y="Power", title=title) +
      theme_minimal() 
    
    plotly::ggplotly(p, tooltip="text") %>% 
      plotly::layout(height = "auto")
  })
}


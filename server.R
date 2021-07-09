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
    df <- read.csv(input$file$datapath, header = header()) 
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
  
  # update picker inputs -----------------------------------------------------
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
    if (length(input$outcomeVar) == 1) {
      shinyjs::hide("helpTextSim")
      shinyjs::enable("simulate")
    } else{
      shinyjs::show("helpTextSim")
      shinyjs::disable("simulate")
    }
  })

  # display data selected --------------------------------------------------
  df <- reactive({ 
    req(dfFull(), input$outcomeVar)
    subset(dfFull(), select = c(input$predictorVars, input$outcomeVar)) 
  })
  
  output$dataTable <-  reactable::renderReactable({  reactable(df()) })
  
  # get sample and effect size(s) -N----------------------------------------
  N <- reactive({
    if(input$ssOrEs == "Sample Size"){
      ss1 <- as.numeric(input$ss[1])
      ss2 <- as.numeric(input$ss[2])
      seq(from = ss1, to = ss2, by = (ss2-ss1)/10)
    } else {
      as.numeric(input$ss2)
    }
  })
  
  es <- reactive({
    if(input$ssOrEs == "Sample Size"){
      as.numeric(input$es)
    } else {
      es1 <- as.numeric(input$es2[1])
      es2 <- as.numeric(input$es2[2])
      seq(from = es1, to = es2, by = (es2-es1)/10)
    }
  })

  # simulate ----------------------------------------------------------------
  results <- eventReactive(input$simulate, {
        results <- simulatePower(
          df(),
          input$alpha, 
          input$sims, 
          es(), 
          N(), 
          trtFrac=0.5,
          input$outcomeVar, 
          input$predictorVars
          ) 
    
    shinyjs::hide("helpTextOuputs")
    
    results
  })
  
  # show results ------------------------------------------------------------
  output$resultsTable <- reactable::renderReactable({
    req(length(results()) > 0)
    if(isolate(input$ssOrEs) == "Sample Size"){
      results() %>%
        rename("Power" = "powers", "Sample size" = "N") %>%
        reactable()
    } else{
      results() %>%
        rename("Power" = "powers", "Effect size" = "effectSize") %>%
        reactable()
    }
    
  })

  output$powerPlot <- renderPlotly({
    req(length(results()) > 0 )
    
    if(isolate(input$ssOrEs) == "Sample Size"){
      x <- isolate(N())
      xlab <- "Sample Size"
      title <- paste("Power Simulations for Effect =", isolate(input$es))
      df <-  results() %>% 
        mutate(text = paste0("Sample size: ", x, "\nPower: ", powers)) 
    } else {
      x <- isolate(es())
      xlab <- "Effect Size"
      title <- paste("Power Simulations for Sample Size =", isolate(input$ss2))
      df <-  results() %>% 
        mutate(text = paste0("Effect size: ", x, "\nPower: ", powers)) 
    }
    
    p <- df %>%
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
    
    plotly::ggplotly(p, tooltip="text") %>% config(displayModeBar = F)
  })
}


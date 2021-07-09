library(shiny)
library(shinyWidgets)
library(shinyBS)
library(shinyjs)
library(bslib)
library(tidyverse)
library(DT)
library(plotly)

source("simulatePowerFunction.R")

pal = c(
    "white" = "#FFFFFF",
    "black" = "#000000",
    "pink" = "#da7fc7", 
    "blue" = "#00c2de",
    "green" = "#94d600", 
    "yellow" = "#ffd800", 
    "purple" = "#bc84cb", 
    "orange" = "#ff8500"
)

# TODO
# make lsit of variables reactive s.t. can't select same variable as predictor and outcome 
# add temp text before sim run
# move table below outputs 

## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## 
##                  ui                       ##
## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ##

ui <- fluidPage(
    useShinyjs(),
    includeCSS("www/styles.css"),
    theme = bslib::bs_theme(
        version=3, 
        bootswatch = "readable",
        bg = pal["white"], 
        fg = pal["black"], 
        primary = pal["blue"],
        success = pal["green"],
        info = pal["orange"],
        warning = pal["yellow"],
        danger = pal["pink"],
        ),

    # application title
    column(width=10, offset=1, 
        titlePanel("Power Simulations"),
        
        tags$p("This application runs power simulations based data and parameters you input below. 
                       The simulations are run using an OLS model. Before starting, make sure your dataset includes columns of
                       any variable transformations you want to include in your model and save the dataset in CSV format."),
        
        sidebarPanel(bsCollapse(
            id = "mainCollapse",
            multiple = FALSE,
            open = c("Upload Data"),
            bsCollapsePanel(
                title = "Upload Data", 
                
                tags$p(tags$strong("Upload your cleaned data in csv format")),
                fileInput(inputId = "file", label = NULL, accept = c(".csv")),
                
                tags$p(tags$strong("Does the file have a header?")),
                radioButtons(
                    inputId = "header", 
                    label = NULL, 
                    choices = c("Yes", "No")
                    ),
                actionButton('next1', "Next", class = "btn btn-info btn-block")
            ), 
            bsCollapsePanel(
                title = "Select Variables", 
                
                tags$p(tags$strong("Choose predictor variables you want to include in your model")),
    
                pickerInput(
                    inputId = "predictorVars", 
                    label = NULL,
                    choices = NULL, 
                    multiple = TRUE
                    ),
                
                tags$p(tags$strong("Choose the outcome variable you want in your model")),
                pickerInput(
                    inputId = "outcomeVar", 
                    label = NULL,
                    choices = NULL,
                    multiple = FALSE
                    ),  
                
                tags$p(tags$strong("Do you want the outcome variable to be log transformed?")),
                radioButtons(
                    inputId = "logOutcome", 
                    label = NULL, 
                    choices = c("Yes", "No"), 
                    selected = "No"
                    ), 
                actionButton('next2', "Next", class = "btn btn-info btn-block")
            ),  
            bsCollapsePanel(
                title = "Select Sample and Effect Size", 

                tags$p(tags$strong("Do you want to vary the sample size or effect size?")),
                radioButtons(
                    inputId = "ssOrEs", 
                    label = NULL, 
                    choices = c("Sample Size", "Effect Size")),
                
                # show if want to vary sample size 
                # i.e. select multiple sample sizes and one effect size
                conditionalPanel(
                    condition = "input.ssOrEs == 'Sample Size'",
                    tags$p(tags$strong("Select a range of sample sizes")),
                    sliderTextInput(
                        inputId = "ss",
                        label = NULL, 
                        choices = seq(0, 1000000, by=1000), 
                        selected = c(10000, 100000)
                    ), 
                    
                    tags$p(tags$strong("Select an effect size as a fraction of the 
                                       difference in means"), tags$br(), 
                           "(e.g. 0.05 = 5 percent difference in means)"),
                    sliderTextInput(
                        inputId = "es",
                        label = NULL, 
                        choices = seq(0.01, 0.5, by=0.01), 
                        selected = 0.05
                    )
                ),
                
                # show if want to vary effect size 
                # i.e. select multiple effect sizes and one sample size
                conditionalPanel(
                    condition = "input.ssOrEs == 'Effect Size'",
                    tags$p(tags$strong("Select a range of effect sizes as a fraction of the difference in means"), tags$br(), 
                           "(e.g. 0.05 = 5 percent difference in means)"),
                    sliderTextInput(
                        inputId = "es",
                        label = NULL, 
                        choices = seq(0.01, 0.5, by=0.01), 
                        selected = c(0.05, 0.15)
                    ), 
                    
                    tags$p(tags$strong("Select a sample size")),
                    sliderTextInput(
                        inputId = "es",
                        label = NULL, 
                        choices = seq(0, 1000000, by=1000), 
                        selected = 50000
                    ), 
                ), 
                actionButton('next3', "Next", class = "btn btn-info btn-block")
                
            ), 
            bsCollapsePanel(
                title = "Select Simulation Parameters", 
                
                tags$p(tags$strong("Select alpha value between 0.01 and 0.1 for significance level")),
                numericInput(inputId = "alpha", label = NULL, value = 0.05, min = 0.01, max = 0.1),
                
                tags$p(tags$strong("Enter number of repetitions to run per sample/effect size"), 
                       tags$br(), "(Value must be between 1 and 500)"),
                numericInput(inputId = "sims", label = NULL, value = 100, min = 1, max = 500),

                actionButton('next4', "Next", class = "btn btn-info btn-block")
            
            ), 
            bsCollapsePanel(
                title = "Run Simulation", 
                tags$div(
                    id="helpTextSim", 
                    helpText("Select input data and variables to run your simulation!")
                ), 
                actionButton(inputId = "simulate", label = "Run Power Simulation")
            )
        )),
                
            
        mainPanel(
            column(8, 
                   tags$p(tags$h3("Estimated Statistical Power")),
                   plotlyOutput("powerPlot", height = "10px")
                   ), 
            column(4, 
                   DT::dataTableOutput(outputId = "resultsTable")
                   ), 
            column(12, 
                   tags$div(
                       id="helpTextOuputs", 
                       helpText("Select your inputs and hit the 'run simulation' button to view estimated power!"))
                   ), 
            column(12, 
                   tags$p(tags$h3("Selected Data")),
                   DT::dataTableOutput(outputId = "dataTable")
                   )

        )
    )
)

## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## 
##                  server                   ##
## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ##

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

    observeEvent(c(input$predictorVars), {
        selectedVars <- c(input$predictorVars, input$outcomeVar)
        varsAvail <- setdiff(names(dfFull()), selectedVars)

        updatePickerInput(
            session = session,
            inputId = "outcomeVar", 
            label = NULL,
            choices = varsAvail, 
            selected = NULL
        )
        
        if (length(input$predictorVars) >= 1 & length(input$outcomeVar) == 1) {
            shinyjs::hide("helpTextSim")
            shinyjs::enable("simulate")
        } else{
            shinyjs::show("helpTextSim")
            shinyjs::disable("simulate")
        }
    })
    
    df <- reactive({ 
        req(dfFull(), input$predictorVars, input$outcomeVar)
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



shinyApp(ui, server)



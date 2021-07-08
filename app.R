library(shiny)
library(shinyWidgets)
library(shinyBS)
library(shinyjs)
library(DT)
library(dplyr)
library(shinythemes)
library(ggplot2)
source("simulatePowerFunction.R")


## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## 
##                  ui                       ##
## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ##

ui <- fluidPage(
    useShinyjs(),
    includeCSS("www/styles.css"),
    theme = shinytheme("cerulean"),
    
    # MODIFY CSS
   # tags$head(tags$style(HTML("hr {border-top: 2px solid #8b1c3f;}"))),

    # application title
    titlePanel("Power Simulations"),
   
    sidebarPanel(bsCollapse(
        id = "collapse_main",
        multiple = TRUE,
        open = c("Introduction"),
        bsCollapsePanel(
            title = "Introduction",
            # DESCRIPTION OF APP 
            # TODO: rephrase 
            tags$p("This application runs power simulations based data and parameters you input below. 
                   The simulations are run using an OLS model. Before starting, make sure your dataset includes columns of
                   any variable transformations you want to include in your model and save the dataset in CSV format."),
            actionButton('next0', "Next") # class = "btn btn-info btn-block"
        ), 
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
            actionButton('next1', "Next") # class = "btn btn-info btn-block"
        ), 
        bsCollapsePanel(
            title = "Select Variables", 
            
            tags$p(tags$strong("Choose predictor variables you want to include in your model")),
            selectizeInput(
                inputId = "predictorVars", 
                label = NULL,
                choices = NULL, 
                multiple = TRUE
                ),
            
            tags$p(tags$strong("Choose the outcome variable you want in your model")),
            selectizeInput(
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
            actionButton('next2', "Next") # class = "btn btn-info btn-block"
        ), 
        bsCollapsePanel(
            title = "Select Sample and Effect Size", 
            tags$p(tags$h4("Select Sample and Effect Size")),
            
            # INPUT: sample size vs effect size
            tags$p(tags$strong("Do you want to vary the sample size or effect size?")),
            radioButtons(
                inputId = "ssOrEs", 
                label = NULL, 
                choices = c("Sample Size", "Effect Size")),
            
            # show if want to vary sample size 
            # i.e. select multiple sample sizes and one effect size
            conditionalPanel(
                condition = "input.ssOrEs == 'Sample Size'",
                tags$p(tags$strong("Enter values between 1 and 1,000,000 to create a range of sample sizes to test")),
                numericInput(inputId = "ssMin", label = "Min", value = NULL, min = 1, max = 1000000),
                numericInput(inputId = "ssMax", label = "Max", value = NULL, min = 1, max = 1000000),
                numericInput(inputId = "ssBy", label = "By", value = NULL, min = 1, max = 1000000),
                
                tags$p(tags$strong("Input an effect size between 0.001 and 1.00 as a fraction of the 
                                   difference in means"), tags$br(), 
                       "(e.g. 0.05 = 5 percent difference in means)"),
                numericInput(inputId = "es", label = NULL, 
                             value = NULL, min = 0.001, max = 1.0)
            ),
            
            # show if want to vary effect size 
            # i.e. select multiple effect sizes and one sample size
            conditionalPanel(
                condition = "input.ssOrEs == 'Effect Size'",
                tags$p(tags$strong("Enter values between 0.001 and 1.00 to create a range of effect sizes 
                                   as a fraction of the difference in means"), tags$br(), 
                       "(e.g. 0.05 = 5 percent difference in means)"),
                numericInput(inputId = "esMin", label = "Min", value = NULL, min = 0.001, max = 1.0),
                numericInput(inputId = "esMax", label = "Max", value = NULL, min = 0.001, max = 1.0),
                numericInput(inputId = "esBy", label = "By", value = NULL,min = 0.001, max = 1.0),
                
                tags$p(tags$strong("Input a sample size between 1 and 1,000,000")),
                numericInput(inputId = "ss", label = NULL, 
                             value = NULL,min = 1, max = 1000000)
            ), 
            actionButton('next3', "Next") # class = "btn btn-info btn-block"
            
        ), 
        bsCollapsePanel(
            title = "Select Simulation Parameters", 
            
            # Horizontal line ----
            tags$hr(), 
            
            tags$p(tags$h4("Select Simulation Parameters")),
            
            # INPUT: alpha
            tags$p(tags$strong("Select alpha value between 0.01 and 0.1 for significance level")),
            numericInput(inputId = "alpha", 
                         label = NULL, 
                         value = 0.05, min = 0.01, max = 0.1),
            
            # INPUT: number of sims
            tags$p(tags$strong("Enter number of repetitions to run per sample/effect size"), 
                   tags$br(), "(Value must be between 1 and 500)"),
            numericInput(inputId = "sims", 
                         label = NULL, 
                         value = 100, min = 1, max = 500),
            
            # numeric input for seed 
            tags$p(tags$strong("Enter seed value"), tags$br(), 
                   "Remember your seed value if you wish to reproduce your results!"),
            numericInput(inputId = "seed", 
                         label = NULL, 
                         value = 123),
            
            # Horizontal line ----
            tags$hr(), 
            
            # run simulation
            actionButton(inputId = "simulate", label = "Run Power Simulation")
        )
    )),
            
        
    mainPanel(
        
        # table of data
        tags$p(tags$h3("Selected Data")),
        DT::dataTableOutput(outputId = "dataTable"),
        
        # table of power results
        tags$p(tags$h3("Power Results")),
        DT::dataTableOutput(outputId = "resultsTable"),
        
        # plot power results
        tags$p(tags$h3("Power Plot")),
        plotOutput("powerPlot")
    )
)

## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## 
##                  server                   ##
## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ##

server <- function(input, output, session) {  # session is used to updateSelectInput
    
    observeEvent(input$next0, {
        updateCollapse(
            session, id = "collapse_main", 
            open = "Upload Data", 
            close = "Introduction"
            )
    })
    
    observeEvent(input$next1, {
        updateCollapse(
            session, id = "collapse_main", 
            open = "Select Variables", 
            close = "Upload Data"
        )
    })
    
    observeEvent(input$next2, {
        updateCollapse(
            session, id = "collapse_main", 
            open = "Select Sample and Effect Size", 
            close = "Select Variables"
            )
    })
    
    observeEvent(input$next3, {
        updateCollapse(
            session, id = "collapse_main",
            open = "Select Simulation Parameters", 
            close = "Select Sample and Effect Size"
            )
    })
    
    # GET DATA
    header <- reactive({
        if(input$header == "Yes"){
            return(TRUE)
        } else {
            return(FALSE)
        }
    })
    
    datFull <- reactive({       
        inFile <- input$file
        req(inFile, header)
        df <- read.csv(inFile$datapath, header = header()) ## could update to allow for diff file formats 
        vars <- colnames(df)
        updateSelectizeInput(session, "predictorVars","Select Predictor Variables", choices = vars)
        updateSelectizeInput(session, "outcomeVar","Select Outcome Variable", choices = vars)
        df
    })
    
    # OUTPUT: datatable of uploaded data based on selected outcome and predictor vars
    output$dataTable <-  DT::renderDataTable({ 
        req(datFull(), input$predictorVars, input$outcomeVar)
        vars <- c(input$predictorVars, input$outcomeVar)
        subset(datFull(), select = vars) 
    })
    
    # SIMULATE 
    observeEvent(input$simulate, {
        # transform log outcome to logical
        logOut <- reactive({
            if(input$logOutcome == "Yes")
                return(TRUE)
            FALSE
        })
        
        # get sample and effect size(s)
        if(input$ssOrEs == "Sample Size"){
            N <- reactive(seq(from = input$ssMin, to = input$ssMax, by = input$ssBy))
            es <- reactive(input$es)
        }
        else{
            N <- reactive(input$ss)
            es <- reactive(seq(from = input$esMin, to = input$esMax, by = input$esBy))
        }
        
        # get dataframe
        df <-  reactive({ 
            df <- datFull()
            vars <- input$predictorVars
            vars <- c(vars, input$outcomeVar)
            df <- subset(df, select = vars) #subsetting takes place here
            df
        })
        
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
                        logOutcome = logOut(), 
                        input$seed
                        )
            
            # OUPUT: build table of power results
            output$resultsTable <-  DT::renderDataTable({ 
                results %>%
                    datatable() %>%
                    formatRound(columns=colnames(results)[2], digits=3)
            })
            
            # OUPUT: build plot of power results 
            h3(textOutput("Power Plot"))
            output$powerPlot <- renderPlot({
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
                ggplot(results, aes(x = x, y = powers)) +
                    geom_point() + geom_smooth() +
                    theme(text = element_text(size=20))+
                    geom_hline(yintercept = 0.8,
                               colour = "#8b1c3f",
                               linetype = "dashed",
                               size = 2) +
                    xlab(xlab)  +
                    ylab("Power") +
                    ggtitle(title)
            })
        })
    })
}



shinyApp(ui, server)



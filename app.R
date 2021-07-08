library(shiny)
library(shinyWidgets)
library(DT)
library(dplyr)
library(shinythemes)
library(ggplot2)
source("simulatePowerFunction.R")


## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## 
##                  ui                       ##
## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ##

ui <- fluidPage(
    includeCSS("www/styles.css"),
    theme = shinytheme("cerulean"),
    
    # MODIFY CSS
   # tags$head(tags$style(HTML("hr {border-top: 2px solid #8b1c3f;}"))),

    # application title
    titlePanel("Power Simulations"),
    
    sidebarPanel(
        # DESCRIPTION OF APP 
        tags$p("This application runs power simulations based data and parameters you input below. 
               The simulations are run using an OLS model. Before starting, make sure your dataset includes columns of
               any variable transformations you want to include in your model and save the dataset in CSV format."),
        
        # horizontal line ---
        tags$hr(), 
        
        # INPUT: file 
        tags$p(tags$h4("Upload Data")),
        tags$p(tags$strong("Choose CSV file to use"),tags$br(),
              "(Note: data must include any transformed variables needed)"),
   
        fileInput(inputId = "file", label = NULL,
                  accept = c(".csv")),
        
        # INPUT: checkbox for file header
        tags$p(tags$strong("Does the file have a header?")),
        radioButtons(inputId = "header", label = NULL, 
            choices = c("Yes", "No")),
        
        # horizontal line ----
        tags$hr(), 
        
        tags$p(tags$h4("Select Variables")),
        
        # INPUT: choose predictor variables
        tags$p(tags$strong("Choose predictor variables you want to include in your model"),tags$br(),
                            "(Reminder: any transformations must be in uploaded dataset)"),
        selectizeInput(inputId = "predictorVars", label = NULL,
                       choices = NULL, multiple = TRUE),
        
        # INPUT: choose outcome variable
        tags$p(tags$strong("Choose the outcome variable you want in your model")),
        selectizeInput(inputId = "outcomeVar", label = NULL,
                       choices = NULL,multiple = FALSE),  
        
        # INPUT: logOutcome?
        tags$p(tags$strong("Do you want the outcome variable to be log transformed?")),
        radioButtons(inputId = "logOutcome", label = NULL, 
                     choices = c("Yes", "No"), selected = "No"),
        
        # Horizontal line ----
        tags$hr(),
        
        tags$p(tags$h4("Select Sample and Effect Size(s)")),
        
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
                tags$p(tags$strong("Enter values between 1 and 100,000 to create a range of sample sizes to test")),
                numericInput(inputId = "ssMin", label = "Min", value = NULL, min = 1, max = 100000),
                numericInput(inputId = "ssMax", label = "Max", value = NULL, min = 1, max = 100000),
                numericInput(inputId = "ssBy", label = "By", value = NULL, min = 1, max = 100000),
                
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
                numericInput(inputId = "esMin", label = "Min", value = NULL, min = 0.0000001, max = 1.0),
                numericInput(inputId = "esMax", label = "Max", value = NULL, min = 0.0000001, max = 1.0),
                numericInput(inputId = "esBy", label = "By", value = NULL,min = 0.0000001, max = 1.0),
                
                tags$p(tags$strong("Input a sample size between 1 and 100,000")),
                numericInput(inputId = "ss", label = NULL, 
                             value = NULL,min = 1, max = 100000)
            ),
        
        # Horizontal line ----
        tags$hr(), 
        
        tags$p(tags$h4("Select Parameters")),
        
        # INPUT: alpha
        tags$p(tags$strong("Select alpha value between 0.01 and 0.1 for significance level"), tags$br(), 
                           "(Value must be between 0.01 and 0.1)"),
        numericInput(inputId = "alpha", 
                     label = NULL, 
                     value = 0.05, min = 0.01, max = 0.1),
        
        # INPUT: number of sims
        tags$p(tags$strong("Enter number of repitions to run per sample/effect size"), 
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
    ),
    
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
    
    # GET DATA
    datFull <- reactive({       
        inFile <- input$file
        req(inFile)
        header <- reactive({
            if(input$header == "Yes")
                return(TRUE)
            FALSE
        })
        df <- read.csv(inFile$datapath, header = header()) ## could update to allow for diff file formats 
        vars <- colnames(df)
        updateSelectizeInput(session, "predictorVars","Select Predictor Variables", choices = vars)
        updateSelectizeInput(session, "outcomeVar","Select Outcome Variable", choices = vars)
        df
    })
    
    # OUTPUT: datatable of uploaded data based on selected outcome and predictor vars
    output$dataTable <-  DT::renderDataTable({ 
            df <- datFull()
            vars <- input$predictorVars
            vars <- c(vars, input$outcomeVar)
            subset(df, select = vars) #subsetting takes place here
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



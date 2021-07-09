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
        danger = pal["pink"]
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
                
                tags$p("Upload your cleaned data in csv format"),
                fileInput(inputId = "file", label = NULL, accept = c(".csv")),
                
                tags$p("Does the file have a header?"),
                radioButtons(
                    inputId = "header", 
                    label = NULL, 
                    choices = c("Yes", "No")
                    ),
                actionButton('next1', "Next", class = "btn btn-info btn-block")
            ), 
            bsCollapsePanel(
                title = "Select Variables", 
                
                tags$p("Choose the outcome variable you want in your model"),
                pickerInput(
                    inputId = "outcomeVar", 
                    label = NULL,
                    choices = NULL,
                    multiple = FALSE
                ),  
                
                tags$p("Choose predictor variables you want to include in your model"),
                pickerInput(
                    inputId = "predictorVars", 
                    label = NULL,
                    choices = NULL, 
                    multiple = TRUE, 
                    options = list('actions-box'= TRUE)
                    ),

                actionButton('next2', "Next", class = "btn btn-info btn-block")
            ),  
            bsCollapsePanel(
                title = "Select Sample and Effect Size", 

                tags$p("Do you want to vary the sample size or effect size?"),
                radioButtons(
                    inputId = "ssOrEs", 
                    label = NULL, 
                    choices = c("Sample Size", "Effect Size")
                    ),
                
                # show if want to vary sample size 
                # i.e. select multiple sample sizes and one effect size
                conditionalPanel(
                    condition = "input.ssOrEs == 'Sample Size'",
                    tags$p("Select a range of sample sizes"),
                    sliderTextInput(
                        inputId = "ss",
                        label = NULL, 
                        choices = seq(1000, 1000000, by=1000), 
                        selected = c(10000, 50000)
                    ), 
                    
                    tags$p("Select an effect size as a fraction of the difference in means, e.g. 0.05 = 5 percent difference in means)"),
                    sliderTextInput(
                        inputId = "es",
                        label = NULL, 
                        choices = seq(0.01, 0.5, by=0.01), 
                        selected = 0.02
                    )
                ),

                # show if want to vary effect size 
                # i.e. select multiple effect sizes and one sample size
                conditionalPanel(
                    condition = "input.ssOrEs == 'Effect Size'",
                    tags$p("Select a range of effect sizes as a fraction of the difference in means, (e.g. 0.05 = 5 percent difference in means)"),
                    sliderTextInput(
                        inputId = "es2",
                        label = NULL, 
                        choices = seq(0.01, 0.5, by=0.01), 
                        selected = c(0.05, 0.15)
                    ), 
                    
                    tags$p("Select a sample size"),
                    sliderTextInput(
                        inputId = "ss2",
                        label = NULL, 
                        choices = seq(1000, 1000000, by=1000), 
                        selected = 50000
                    )
                ), 
                actionButton('next3', "Next", class = "btn btn-info btn-block")
            ), 
            bsCollapsePanel(
                title = "Select Simulation Parameters", 
                
                tags$p("Select alpha value between 0.01 and 0.1 for significance level"),
                numericInput(inputId = "alpha", label = NULL, value = 0.05, min = 0.01, max = 0.1),
                
                tags$p("Enter number of repetitions to run per sample/effect size, (Value must be between 1 and 500)"),
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
            column(12, 
                   tags$div(
                       id = "outputs", 
                       tags$p(tags$h3("Estimated Statistical Power")), 
                       tags$div(
                           id="helpTextOuputs", 
                           helpText("Select your inputs and hit the 'run simulation' button to view estimated power!")
                       ), 
                       column(8, plotlyOutput("powerPlot", height = "auto")), 
                       column(4, reactable::reactableOutput(outputId = "resultsTable"))
                   )
                   ), 
            column(12, 
                   tags$div(
                       id = "data",
                       tags$br(), 
                       tags$hr(),
                       tags$p(tags$h3("Selected Data")),
                       reactable::reactableOutput(outputId = "dataTable")
                   )
                   )
            )
    )
)
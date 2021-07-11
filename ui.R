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

    column(width=10, offset=1, 
        titlePanel("Simulating power"),
        tags$h5(
            style = "color: #ff8500; margin:margin:5px 15px 0px 15px",
            "
            This application uses simulations based on user-uploaded data 
            to estimate statistical power for randomized controlled trials (RCTs) 
            that will be analyzed using an OLS regression.
            "
        ), 
        tags$br(), 
        introPanels, 
        
        sidebarPanel(bsCollapse(
            id = "mainCollapse",
            multiple = FALSE,
            open = c("Upload Data"),
            bsCollapsePanel(
                title = "Upload Data", 
                materialSwitch(
                    inputId = "useSampleData",
                    label = "Use example data?", 
                    value = FALSE,
                    status = "primary"
                ),
                conditionalPanel(
                    condition = "input.useSampleData",
                    tags$h5("Sample data loaded!"), 
                    tags$hr(), 
                    tags$p(
                        "This example data is from ",
                        tags$a(href = "https://www.capitalbikeshare.com/", "Capital Bikeshare"), 
                        " via ", 
                        tags$a(href = "https://github.com/MicrosoftDocs/ml-basics", "Microsoft's Azure ml-basics repo"), 
                        ". The data includes the following variables:",
                        tags$ul(
                            tags$li(tags$b("dteday"), ": date on which the data was observed"), 
                            tags$li(tags$b("season"), ": a categorical variable indicating season (1:spring, 2:summer, 3:fall, 4:winter)"), 
                            tags$li(tags$b("yr"), ": the year the observation was made (year 0:2011, and year 1:2012)"), 
                            tags$li(tags$b("month"), ": the month the observation was made (1:January ... 12:December)"), 
                            tags$li(tags$b("holiday"), ": a binary indicator if the day was a public holiday"), 
                            tags$li(tags$b("weekday"), ": the day of week of the observation (0:Sunday ... 6:Saturday)"), 
                            tags$li(tags$b("workingday"), ": a bindary indicator indicating if the day was a weekend/holiday or a working day"), 
                            tags$li(tags$b("weathersit"), ": a categorical variable for the weather situation (1:clear, 2:mist/cloud, 3:light rain/snow, 4:heavy rain/hail/snow/fog)"), 
                            tags$li(tags$b("temp"), ": the temperature in Celsius (normalized)"), 
                            tags$li(tags$b("atemp"), ": the apparent temperature in Celsius (normalized)"), 
                            tags$li(tags$b("hum"), ": normalized humidity"), 
                            tags$li(tags$b("windspeed"), ": normalized windspeed"), 
                            tags$li(tags$b("rentals"), ": the number of Capital Bikeshare rentals"), 
                        )
                    )
                ), 
                conditionalPanel(
                    condition = "! input.useSampleData",
                    
                        tags$p("Upload your cleaned data in csv format"),
                        fileInput(inputId = "file", label = NULL, accept = c(".csv")),
                        
                        tags$p("Does the file have a header?"),
                        radioButtons(
                            inputId = "header", 
                            label = NULL, 
                            choices = c("Yes", "No")
                        ),
                        actionButton('next1', "Next", class = "btn btn-info btn-block")
                    ) 
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
                
                tags$p("Choose any predictor variables you want to include in your model"),
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
                title = "Select Experiment Parameters", 
                
                tags$p("Select the probability that each unit will be assigned to treatment"),
                sliderTextInput(
                    inputId = "trtFrac",
                    label = NULL, 
                    choices = seq(0,1,0.01), 
                    selected = c(0.5)
                ), 

                tags$hr(), 
                
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
                    tags$p("Enter a range of sample sizes"),
                    numericRangeInput(
                        inputId = "ss",
                        label = NULL, 
                        value = c(10000, 50000), 
                        separator = " - "
                    ), 
                    
                    tags$p("Select an effect size as a fraction of the difference 
                           in means between the treatment and control group, 
                           e.g. 0.05 implies the average outcome in the treatment group is 
                           5 percent higher than the control group; -0.05 implies
                           average outcome in the treatment group is 5 percent 
                           lower than the control group"),
                    numericInput(
                        inputId = "es",
                        label = NULL, 
                        value = 0.02
                    )
                ),

                # show if want to vary effect size 
                # i.e. select multiple effect sizes and one sample size
                conditionalPanel(
                    condition = "input.ssOrEs == 'Effect Size'",
                    tags$p("Enter a range of effect sizes as a fraction of the 
                           difference in means between the treatment and control group, 
                           e.g. 0.05 implies the average outcome in the treatment group is 
                           5 percent higher than the control group; -0.05 implies
                           average outcome in the treatment group is 5 percent 
                           lower than the control group"),
                    numericRangeInput(
                        inputId = "es2",
                        label = NULL, 
                        value = c(0.01, 0.15), 
                        separator = " - "
                    ), 
                    
                    tags$p("Select a sample size"),
                    numericInput(
                        inputId = "ss2",
                        label = NULL, 
                        min = 0, 
                        value = 1000
                    )
                ), 
                actionButton('next3', "Next", class = "btn btn-info btn-block")
            ), 
            bsCollapsePanel(
                title = "Select Simulation Parameters", 
                
                tags$p("Select statistical significance level between 0.01 and 0.1"),
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
                       column(8, plotlyOutput("powerPlot", height = "auto") %>% 
                                  shinycssloaders::withSpinner(color=pal["orange"])), 
                       column(4, reactable::reactableOutput(outputId = "resultsTable") %>% 
                                  shinycssloaders::withSpinner(color=pal["orange"]))
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
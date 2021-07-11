introPanels <- div(style="margin:5px 15px 0px 15px",
    bsCollapse(
      id = "textCollapse",
      multiple = TRUE,
      open = c(), 
      bsCollapsePanel(
        title = "A brief overview of simulating power", 
        tags$p(
          tags$b(
          "
          In an RCT, statistical power is the probability of
          identifying an effect of the treatment when the treatment
          had a real effect. 
          "
          )
        ), 
        tags$p(
          "
          As a toy example, say we design an RCT to evaluate the impact of 
          lower prices on the use of Washington D.C.'s Capital Bikeshare system 
          by randomly selecting days to apply a lower bike share price and 
          then using a regression to estimate the effect of the price change 
          on daily bike rentals. (Note: this probably isn't the optimal experimental
          design for this problem - this is just an example!). 
          The power of the study is the probability our analysis concludes 
          the program changed bike riding on the treatment group days 
          (the days with the discount applied) when the incentive really did 
          have an influence on daily bike share use. 
          There is  sample Capital Bikeshare data included with this app 
          (just toggle the 'Use example data' switch under 'Upload Data')
          if you want to explore running the simulation on this example dataset.
          "
        ), 
        tags$hr(),
        tags$p(
          tags$b("There are many factors that influence statistical power"), 
          "
          , including the study's sample size in each experimental group, 
          (e.g., how many days will we collect data for? how many days will we apply the discount?), 
          the expected effect size (e.g., how much will the price change impact
          our bike share use?), the statistical significance level (the probability 
          of concluding the treatment had no effect, when it really did), and 
          the analysis used to evaluate the RCT.
          "
        ), 
        tags$p(
          tags$b(
          "
          Estimating the statistical power before running an RCT
          can be used to estimate the necessary sample size to
          achieve sufficient power assuming the treatment effect size 
          or estimate the treatment effect size to achieve 
          sufficient power assuming the sample size. 
          "
          )
        ), 
        tags$hr(),
        tags$p(
          tags$b(
            "
            While analytical equations exist to estimate power under
            simple experimental designs, using a simulation to estimate
            your experiment's power can provide a more accurate 
            estimate of your experiment's statistical power.
            "), 
          "This app allows users to quickly simulate statistical power for 
          experiments that will be analyzed using an OLS regression."
        ), 
        tags$p(
          "
          In the Capital Bikeshare example, this regression analysis
          would involve regressing daily bike rental use on a binary indicator 
          of if the price change was applied or not and any other variables we 
          want to control for, such as the temperature or day of week. 
          "
        ), 
        tags$hr(),
        tags$p(
          tags$b(
          "Some additional resources on power analyses include: ", 
          tags$a(
            href='https://egap.org/resource/10-things-to-know-about-statistical-power/',
            "EGAP's  '10 Things to Know About Statistical Power'",
          ), " and ", 
          tags$a(
            href='https://stats.idre.ucla.edu/other/mult-pkg/seminars/intro-power/',
            "UCLA IDRE's 'Introduction to Power Analysis'"
          ))
        )
      ),
      bsCollapsePanel(
        title = "Using this app", 
        tags$p(
          "
          To run a simulation, use the panel on the left of the screen 
          to upload a CSV file with baseline data for your outcome
          of interest and (optionally) any additional predictor variables 
          you plan to include in your analysis. If you don't have baseline data, 
          you can create a simulated dataset to use, or just test of the app
          using the sample data by toggling the 'Use example data' switch under 'Upload data'. 
          If you plan to transform any of you variables (e.g., taking the log of 
          your outcome of interest), apply these transformations before uploading your data. 
          Then, select your experiment  and simulation parameters of interest
          and hit the 'Run simulation' button.
          "
        )
      ), 
      bsCollapsePanel(
        title = "Have a more complicated experimental design or analysis plan?", 
        tags$p(
          "
          If you have a more complicated experimental design or analysis
          plan than what can be simulated using this app, I'd 
          encourage you to similuate the power yourself! You can 
          use this app's code as a starting point: 
          ", 
          tags$a(
            href="https://github.com/marisalyn/simulatePower",
            "https://github.com/marisalyn/simulatePower"
          )
        )
      )
    )
)
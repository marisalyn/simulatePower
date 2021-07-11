introPanels <- div(style="margin:5px 15px 0px 15px",
    bsCollapse(
      id = "textCollapse",
      multiple = TRUE,
      open = c(), 
      bsCollapsePanel(
        title = "A brief overview of simulating power", 
        tags$p(
          "
          In an RCT, statistical power is the probability of
          identifying an effect of the treatment when the treatment
          had a real effect. 
          "
        ), 
        tags$hr(),
        tags$p(
          "
          As a simple example, say we design an RCT 
          to incentize use of Washington D.C.'s Capital 
          Bikeshare system by sending some percent of users of the
          app a discount for their next 10 bike rides. We want to 
          measure if the incentive changes use of the Captial Bikeshare
          system for those who receive the message (the treatment group) 
          relative to those who do not (the control group). 
          The power of the study is the proability our analysis
          concludes the program changed bike riding amongst the 
          treatment group when the incentive really did have an influence. 
          "
        ), 
        tags$br(), 
        tags$p(
          "
          There are many factors that influence power, including 
          the study's sample size (how many users will we include
          in the treatment and control groups?), the expected 
          effect size (how much will the treatment impact our outcome?), 
          the statistical significance level (probability of concluding
          the experiment had no effect, when it really did), and 
          the analysis used to evaluate the RCT.
          "
        ), 
        tags$p(
          "
          Estimating the statistical power before running an RCT
          can be used to help estimate the necessary sample size to
          achieve sufficient power assuming the treatment effect size, 
          or identifying the treatment effect size to achieve 
          sufficient power assuming the sample size. 
          "
        ), 
        tags$hr(),
        tags$p(
          "
          While analytical equations exist to estimate power under
          simple experimental designs, using a simulation to estimate
          your experiment's power can provide a more accurate 
          estimate of your experiment's statistical power. This app 
          allows users to quickly simulate statistical power for 
          experiments that will be analyzed using an OLS regression.
          "
        ), 
        tags$p(
          "Some additional resources on power analyses include: ", 
          tags$a(
            href='https://egap.org/resource/10-things-to-know-about-statistical-power/',
            "EGAP's  '10 Things to Know About Statistical Power'",
          ), " and ", 
          tags$a(
            href='https://stats.idre.ucla.edu/other/mult-pkg/seminars/intro-power/',
            "UCLA IDRE's 'Introduction to Power Analysis'"
          )
        )
      ),
      bsCollapsePanel(
        title = "Using this app", 
        tags$p(
          "
          To run a simulation, use the options panel on the left of the screen 
          to upload a CSV file with baseline data for your outcome
          of interest and (optionally) any additional predictor variables 
          you plan to include in your analysis. If you don't have baseline data, 
          you can create a simulated dataset to use. If you plan to transform 
          any of you variables (e.g., taking the log of your outcome of interest), 
          apply these transformations before uploading your data. 
          Then, select your experiment  and simulation parameters of interest
          and hit the 'Run simulation' button.
          "
        )
      ), 
      bsCollapsePanel(
        title = "Have a more complicated experimental design or analysis plan?", 
        tags$p(
          "
          If you have a more complicated experimental design of analysis
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
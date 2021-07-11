#' Functions to simulate statistical power of experiments 

simulatePower <- function(df,
                          alpha,
                          sims,
                          effectSize,
                          N,
                          trtFrac, 
                          outcome,
                          predictors, 
                          seed = 123) {
  
  #' Function for using a simulation to estimate the statistical power of a 
  #' randomized experiment that will be evaluated using an OLS regression to  
  #' estimate the average treatment effect. The simulation can be run either  
  #' across multiple sample sizes (holding the effect size constant)  
  #' or multiple effect sizes (holding N constant). 
  #' 
  #' Parameters:
  #' df: cleaned dataframe of historical data or simulated data
  #' alpha: significance level for experiment
  #' sims: number of simulations to run per sample/effect size combination
  #' effectSize: the effect size(s) to test
  #' N: the sample size(s) to test
  #'trtFrac: the probability an individual is assigned to the treatment group
  #' outcome: the outcome variable of interest,  provided in quotations, e.g. "y"
  #' predictors: the variables to include in the OLS regression as independent 
  #'     variables 
  #'
  #' Outputs: Dataframe of tested effect/sample sizes and associated power
  
  set.seed(seed)
  
  if (length(outcome) > 1){
    stop("Only 1 outcome predictor allowed!")
  }
  
  if (length(N) > 1 & length(effectSize) > 1) {
    stop(" \n Both N and effectSize are greater than length 1!")
  }
  
  if (length(N) > 1) {
    powers <- map(N, ~simulateOLSExperiment(df, alpha, sims, effectSize, ., trtFrac, outcome, predictors)) %>% 
      unlist()
    results <- as.data.frame(cbind(N, powers))
  } else if (length(effectSize) > 1) {
    powers <- map(effectSize, ~simulateOLSExperiment(df, alpha, sims, ., N, trtFrac, outcome, predictors))%>% 
      unlist()
    results <- as.data.frame(cbind(effectSize, powers))
  } else {
    power <- simulateOLSExperiment(df, alpha, sims, effectSize, N, trtFrac, outcome, predictors)%>% 
      unlist()
    results <- as.data.frame(cbind(N, effectSize, power))
  }
  
  return(results)
}

simulateOLSExperiment <- function(df, 
                                  alpha,
                                  sims, 
                                  effectSize,
                                  N, 
                                  trtFrac,
                                  outcome, 
                                  predictors) {
  
  #' Helper function to run a simulation to estimate the statistical power of a 
  #' randomized experiment evaluated using an OLS regression to estimate 
  #' the average treatment effect. The simulation is run only for a single 
  #' effect size/sample size combination.
  #' 
  #' Parameters:
  #' df: cleaned dataframe of historical data or simulated data
  #' alpha: significance level for experiment
  #' sims: number of simulations to run per sample/effect size combination
  #' effectSize: the effect size(s) to test
  #' N: the sample size(s) to test
  #' trtFrac: the probability an individual is assigned to the treatment group
  #' outcome: the outcome variable of interest,  provided in quotations, e.g. "y"
  #' predictors: the variables to include in the OLS regression as independent 
  #'     variables 
  #'
  #' Outputs: estimated power for the given inputs
  
  if (length(outcome) > 1){
    stop("Only 1 outcome predictor allowed!")
  }
  
  if (length(N) > 1 | length(effectSize) > 1) {
    stop(paste0(
      "N and/or effectSize are greater than length 1!", 
      "Use 'simulatePower' to estimate power across multiple effect or sample sizes"
    ))
  }
  
  significantExperiments <- c()
  outcome_sym <- sym(outcome)
  for (i in 1:sims) {

    sampleDat <- sample_n(df, N, replace = TRUE) %>%
      mutate(
        trt = sample(c(1,0), size=N, replace=TRUE, prob=c(trtFrac, (1-trtFrac))),
        y = as.numeric(!!outcome_sym), 
        y = if_else(trt == 1, (1 + effectSize) * y, y), 
        trt = as.factor(trt)
      ) %>% 
      select(- !!outcome_sym)
    
    fit <- lm(y ~ ., data = sampleDat)
    p <- summary(fit)$coefficients["trt1", 4]  # p-val of trt

    significantExperiments <- c(significantExperiments, p <= alpha)
  }
  
  return(mean(significantExperiments))
}


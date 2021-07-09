## create function for simulating power using an OLS regression

## inputs:
# df: cleaned, subsetted dataframe to use for the simulation
# alpha: alpha value for significance level of experiments
# sims: number of simulations to run per provided sample/effect size combos
# effectSize: the effect size(s) to test
# N: the sample size(s) to test
#     [note only effect size or N may be more than one value]
# trtFrac: fraction of sample size assigned to treatment
# outcome: the outcome variable of interest, 
#     provided in quotations, e.g. "waterUse" 
#     [note: only one outcome variable is allowed]
# predictors: the variables to include as predictor variables

## outputs:
# dataframe of tested effect/sample sizes and associated power

simulatePower <-
  function(df,
           alpha,
           sims,
           effectSize,
           N,
           trtFrac, # TODO
           outcome,
           predictors, 
           shiny = TRUE, 
           seed = 123) {
    
    set.seed(seed)
    
    outcome_sym <- ensym(outcome)
    
    if (length(outcome) > 1)
      stop("Only 1 outcome predictor allowed!")

    if (length(N) > 1 & length(effectSize) > 1)
      stop(" \n Both N and effectSize are greater than length 1!")
    
    # reset rownames dataframe
    rownames(df) <- NULL
    
    # if testing multiple N
    if (length(N) > 1) {
      powers <- rep(NA, length(N))
      for (j in 1:length(N)) {
        n = N[j]
        
        # increment progress for shiny app
        if (shiny){
          incProgress(j/length(N), detail = paste("Simulating Sample Size:", n))
        } 
        
        significantExperiments <- rep(NA, sims)
        for (i in 1:sims) {
          sampleDat <- df %>% 
            sample_n(., n, replace = TRUE) %>%
            mutate(
              trt = sample(c(1,0), size=n, replace=TRUE, prob=c(trtFrac, (1-trtFrac))), 
              y = if_else(trt == 1, (1 - effectSize) * !!outcome_sym, as.numeric(!!outcome_sym)), 
              trt = as.factor(trt)
              ) %>% 
            select(- !!outcome_sym)
          

          fit <- lm(y ~ ., data = sampleDat)
          
          p <- summary(fit)$coefficients["trt1", 4]  # p-val of trt
          
          # 1 if experiment has significant trt
          significantExperiments[i] <- (p <= alpha) 
        }
        powers[j] <- mean(significantExperiments)
      }
      results <- as.data.frame(cbind(N, powers))
    }
    
    # if testing multiple effect sizes
    if (length(N) == 1) {
      powers <- rep(NA, length(effectSize))
      for (j in 1:length(effectSize)) {
        effect = effectSize[j]
        
        # increment progress for shiny app
        if (shiny){
          incProgress(j/length(effectSize), detail = paste("Simulating Effect Size:", effect))
        } 
        
        significantExperiments <- rep(NA, sims)
        for (i in 1:sims) {
          sampleDat <- df %>% 
            sample_n(., N, replace = TRUE) %>%
            mutate(
              trt = sample(c(1,0), size=N, replace=TRUE, prob=c(trtFrac, (1-trtFrac))), 
              y = if_else(trt == 1, (1 - effect) * !!outcome_sym, as.numeric(!!outcome_sym)), 
              trt = as.factor(trt)
            ) %>% 
            select(-!!outcome_sym)

          fit <- lm(y ~ ., data = sampleDat)
          
          p <- summary(fit)$coefficients["trt1",4]     # p-val of trt
          significantExperiments[i] <- (p <= alpha) # 1 if experiment has significant trt
        }
        powers[j] <- mean(significantExperiments)
      }
      results <- as.data.frame(cbind(effectSize, powers))
    }
    return(results)
  }





## create function for simulating power using an OLS regression

## inputs:
# df: cleaned, subsetted dataframe to use for the simulation
# alpha: alpha value for significance level of experiments
# sims: number of simulations to run per provided sample/effect size combos
# effectSize: the effect size(s) to test
# N: the sample size(s) to test
#     [note only effect size or N may be more than one value]
# outcome: the outcome variable of interest, 
#     provided in quotations, e.g. "waterUse" 
#     [note: only one outcome variable is allowed]
# predictors: the variables to include as predictor variables
# logOutcome: logical for whether or not to take the log of the outcome variable
#     [note: if input variables must be logged they must be provided in that
#      format in the provided dataframe]

## outputs:
# dataframe of tested effect/sample sizes and associated power


simulatePower <-
  function(df,
           alpha,
           sims,
           effectSize,
           N,
           outcome,
           predictors, 
           logOutcome = FALSE, 
           seed = 123) {
    
    set.seed(seed)
    
    # throw warning if outcome greater than 1
    if (length(outcome) > 1)
      stop("Only 1 outcome predictor allowed!")
  
    # throw warning if both effectSize and N greater than 1
    if (length(N) > 1 &
        length(effectSize) > 1)
      stop(" \n Both N and effectSize are greater than length 1!")
    
    # reset rownames dataframe
    rownames(df) <- NULL
    
    # if testing multiple N
    if (length(N) > 1) {
      powers <- rep(NA, length(N))
      for (j in 1:length(N)) {
        n = N[j]
        
        # increment progress for shiny app
        incProgress(j/length(N), detail = paste("Simulating Sample Size:", n))
        
        significantExperiments <- rep(NA, sims)
        for (i in 1:sims) {
          trt <- df[sample(nrow(df), size = 0.5 * n, replace = TRUE),]
          trt$trt <- 1
          cntrl <- df[sample(nrow(df), size = 0.5 * n, replace = TRUE),]
          cntrl$trt <- 0
          sampleDat <- rbind(trt, cntrl)
          sampleDat$y <- ifelse(sampleDat$trt == 1 ,
                                (1 - effectSize) * sampleDat[[outcome]],
                                sampleDat[[outcome]]) # estimate effect size
          modelDat <- sampleDat %>% select(-c(outcome))
          modelDat$trt <- as.factor(modelDat$trt)
          if(logOutcome == TRUE){
            fit <- lm(log(y) ~ ., data = modelDat)
          }
          else{
            fit <- lm(y ~ ., data = modelDat)
          }
          p <- summary(fit)$coefficients["trt1", 4]  # p-val of trt
          significantExperiments[i] <-
            (p <= alpha) # 1 if experiment has significant trt
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
        incProgress(j/length(effectSize), detail = paste("Simulating Effect Size:", effect))
        
        significantExperiments <- rep(NA, sims)
        for (i in 1:sims) {
          trt <- df[sample(nrow(df), size = 0.5 * N, replace = TRUE),]
          trt$trt <- 1
          cntrl <-
            df[sample(nrow(df), size = 0.5 * N, replace = TRUE),]
          cntrl$trt <- 0
          sampleDat <- rbind(trt, cntrl)
          sampleDat$y <- ifelse(sampleDat$trt == 1 ,
                                (1 - effect) * sampleDat[[outcome]],
                                sampleDat[[outcome]]) # estimate effect size
          modelDat <- sampleDat %>% select(-c(outcome))
          modelDat$trt <- as.factor(modelDat$trt)
          if(logOutcome == TRUE){
            fit <- lm(log(y) ~ ., data = modelDat)
          }
          else{
            fit <- lm(y ~ ., data = modelDat)
          }
          p <- summary(fit)$coefficients["trt1",4]     # p-val of trt
          significantExperiments[i] <- (p <= alpha) # 1 if experiment has significant trt
        }
        powers[j] <- mean(significantExperiments)
      }
      results <- as.data.frame(cbind(effectSize, powers))
    }
    return(results)
  }



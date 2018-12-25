# simulatePower
Shiny app for power analysis simulations

### About 
This is a shiny app which runs power simulations based on user-uploaded data and parameters. The simulations are based an experimental design that assigns individuals to treatment with probability 1/2. The significance test is based on an OLS model of the form:

![equation](http://www.sciweavers.org/upload/Tex2Img_1545699117/render.png)

### User inputs  
The user inputs used in the power simulation function are: 

* Dataframe: a cleaned dataframe to use for the simulation
* Outcome variable: the variable in the dataframe uploaded to use as the outcome of interest
* logOutcome: a binary indicator of whether or not the outcome variable should be log transformed
* Predictor(s): the variable(s) to use as predictors in the model [note any transformations of predictor variables must be made before uploading the dataset]
* Effect size(s): the treatment effect size(s) to test [note only effect size or sample size may be more than one value]
* Sample size(s): the treatment sample size(s) to test [note only effect size or sample size may be more than one value]
* Alpha: alpha value for significance level of experiments
* Number of simulations: the number of simulations to run per provided sample/effect size combos

### Reactive Outputs
The app displays:

* A table of selected data
* A table of power vs. sample size or effect size (whichever is varied)
* A plot of power vs. sample size or effect size (whichever is varied)

### Packages required
Several packages are used in the app and can be installed using:
```R
install.packages("shiny")
install.packages("shinyWidgets")
install.packages("DT")
install.packages("dplyr")
install.packages("shinythemes")
install.packages("ggplot2")
```

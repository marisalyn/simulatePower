# simulatePower
Shiny app for power analysis simulations

### About 
This is a shiny app which runs power simulations based on user-uploaded data and parameters. The simulations are based an experimental design in which 50 perecent of the sample is assigned to treatment and 50 percent is assigned to control. A bootstrapping approach is used in each simulation to generate the treatment and control groups. In each rep, the function randomly samples (with replacement) N/2 rows and assigns them to treatment. It then samples an additional N/2 rows and assign them to control where N = the total sample size. The function assumes a homogeneous treatment effect and reduces the specified outcome variable by this amount. For example, if the effect is 5 percent, then the outcome variable for each individual in the treatment group is multiplied by 0.95. Then, an OLS model of the form: 

![equation](http://www.sciweavers.org/upload/Tex2Img_1545699117/render.png)

is run. The p-value for the treatment effect coefficient is extracted. If the p-value is less than the specified alpha (e.g. 0.05), the experiment is marked as significant. Then the number of significant experiments divided by the number of repitions is the power of the given sample/effect. 

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
* A plot of power vs. sample size or effect size (whichever is varied); each sample/effect size result is plotted as a point and a LOESS smoothed curve with confidence interval is plotted through the points


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

# simulatePower
Shiny app for power analysis simulations available here: https://marisalyn.shinyapps.io/simulatepower/

### About 

This is a shiny app which runs simulations for a power analysis based on user-uploaded baseline (or simulated) data and parameters. The simulations are based an experimental design in which some user-defined percent of the sample is assigned to treatment and the remaining observations are assigned to control control. A bootstrapping approach is used in each simulation to generate the treatment and control groups. In each rep, the function randomly samples rows (with replacement) and assigns them to treatment. It then samples  additional rows and assigns them to the control group. The function assumes a homogeneous treatment effect and either increases or decreases the user-specified outcome variable by the user-specified average effect. For example, if the effect is a 5 percent increase, then the outcome variable for each individual in the treatment group is multiplied by 1.05. Then, an OLS model of the treatment and any user-specified predictor variables on the user-specified outcome is run. The p-value for the treatment effect coefficient is extracted. If the p-value is less than the user-specified statistical significance value (e.g. 0.05), the experiment is marked as significant. Then the number of significant experiments divided by the number of renditions is the power of the given sample/effect. 

The app displays:

* A table of selected data
* A table of power vs. sample size or effect size (whichever is varied)
* A plot of power vs. sample size or effect size (whichever is varied); each sample/effect size result is plotted as a point and a LOESS smoothed curve with confidence interval is plotted through the points


### Package management.

This app uses `renv` for dependency management. To learn more about `renv` see the docs here: 
https://rstudio.github.io/renv/articles/renv.html


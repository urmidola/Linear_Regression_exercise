 ## Exercise: least squares regression 
 ##   Use the /states.rds/ data set. Fit a model predicting energy consumed
 ##   per capita (energy) from the percentage of residents living in
 ##   metropolitan areas (metro). Be sure to
 ##   1. Examine/plot the data before fitting the model
 ##   2. Print and interpret the model `summary'
 ##   3. `plot' the model to look for deviations from modeling assumptions 
 ##   Select one or more additional predictors to add to your model and
 ##   repeat steps 1-3. Is this model significantly better than the model
##   with /metro/ as the only predictor?
  
data1 <- subset(na.omit(states.data), select = c("metro", "energy"))
plot(data1)
summary(data1)
  metro            energy     
  Min.   : 20.40   Min.   :200.0  
  1st Qu.: 47.92   1st Qu.:287.0  
  Median : 67.55   Median :320.0  
  Mean   : 64.31   Mean   :343.6  
  3rd Qu.: 81.62   3rd Qu.:362.5  
  Max.   :100.00   Max.   :786.0  
 lm.mod1 <- lm(energy ~ metro, data = na.omit(states.data))
 summary(lm.mod1)
  
  Call:
    lm(formula = energy ~ metro, data = na.omit(states.data))
  
  Residuals:
    Min      1Q  Median      3Q     Max 
  -179.17  -54.21  -21.64   15.07  448.02 
  
  Coefficients:
    Estimate Std. Error t value Pr(>|t|)    
  (Intercept) 449.8382    50.4472   8.917 1.37e-11 ***
    metro        -1.6526     0.7428  -2.225    0.031 *  
    ---
    Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
  
  Residual standard error: 112.3 on 46 degrees of freedom
  Multiple R-squared:  0.09714,	Adjusted R-squared:  0.07751 
  F-statistic: 4.949 on 1 and 46 DF,  p-value: 0.03105
  
  
  
lm.mod2 <- lm(energy ~ metro + toxic, data = na.omit(states.data))
summary(lm.mod2)
  
  Call:
    lm(formula = energy ~ metro + toxic, data = na.omit(states.data))
  
  Residuals:
    Min      1Q  Median      3Q     Max 
  -205.31  -48.05  -12.73   27.60  376.41 
  
  Coefficients:
    Estimate Std. Error t value Pr(>|t|)    
  (Intercept) 353.4806    46.2250   7.647 1.13e-09 ***
    metro        -1.1037     0.6229  -1.772   0.0832 .  
  toxic         3.4804     0.7299   4.768 1.98e-05 ***
    ---
    Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
  
  Residual standard error: 92.57 on 45 degrees of freedom
  Multiple R-squared:  0.4002,	Adjusted R-squared:  0.3735 
  F-statistic: 15.01 on 2 and 45 DF,  p-value: 1.013e-05
  
  
  
  
lm.mod3 <- lm(energy ~ metro + toxic + green, data = na.omit(states.data))
summary(lm.mod3)
  
  Call:
    lm(formula = energy ~ metro + toxic + green, data = na.omit(states.data))
  
  Residuals:
    Min       1Q   Median       3Q      Max 
  -179.311  -31.415   -4.114   17.108  191.943 
  
  Coefficients:
    Estimate Std. Error t value Pr(>|t|)    
  (Intercept) 160.5506    37.4912   4.282 9.87e-05 ***
    metro         0.2437     0.4273   0.570    0.571    
  toxic         2.6691     0.4730   5.643 1.13e-06 ***
    green         4.7992     0.5819   8.247 1.79e-10 ***
    ---
    Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
  
  Residual standard error: 58.67 on 44 degrees of freedom
  Multiple R-squared:  0.7644,	Adjusted R-squared:  0.7483 
  F-statistic: 47.58 on 3 and 44 DF, p-value: 7.305e-14
  
  ## The P-value for the analysis of variance F-test (P = 7.305e-14) 
  ##suggests that the model containing  toxic and green is more useful
  ##in predicting energy consumed per capita than not taking into account 
  ##the two predictors. 
  
  
  anova(lm.mod1, lm.mod2, lm.mod3)
  Analysis of Variance Table
  
  Model 1: energy ~ metro
  Model 2: energy ~ metro + toxic
  Model 3: energy ~ metro + toxic + green
  Res.Df    RSS Df Sum of Sq      F    Pr(>F)    
  1     46 580411                                  
  2     45 385607  1    194805 56.591 1.973e-09 ***
    3     44 151462  1    234145 68.020 1.785e-10 ***
    ---
    Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
  
  
 SSE = sum(lm.mod1$residuals^2)
 SSE
  [1] 580411.5
 SSE = sum(lm.mod2$residuals^2)
 SSE
  [1] 385606.7
SSE = sum(lm.mod3$residuals^2)
SSE
  [1] 151461.9
  
  
  
  
  ## Exercise: interactions and factors
  ##   Use the states data set.
  ##   1. Add on to the regression equation that you created in exercise 1 by
  ##      generating an interaction term and testing the interaction
  ##   2. Try adding region to the model. Are there significant differences
  ##      across the four regions?
  
  library(foreign)
  states.data1 <- read.dta("dataSets/states.dta")
  
  
  lm.mod10 <- lm(energy ~ metro + toxic*pop + green*pop, data = na.omit(states.data1))
  summary(lm.mod10)
  
  Call:
    lm(formula = energy ~ metro + toxic * pop + green * pop, data = na.omit(states.data))
  
  Residuals:
    Min      1Q  Median      3Q     Max 
  -89.390 -27.555  -5.321  25.052 113.056 
  
  Coefficients:
    Estimate Std. Error t value Pr(>|t|)    
  (Intercept)  2.022e+02  3.088e+01   6.550 7.16e-08 ***
    metro       -2.464e-01  4.027e-01  -0.612 0.544083    
  toxic       -1.644e-02  6.979e-01  -0.024 0.981320    
  pop         -1.383e-06  4.343e-06  -0.319 0.751677    
  green        4.973e+00  5.286e-01   9.408 8.52e-12 ***
    toxic:pop    8.210e-07  2.110e-07   3.891 0.000359 ***
    pop:green   -1.916e-07  2.916e-07  -0.657 0.514929    
  ---
    Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
  
  Residual standard error: 46.32 on 41 degrees of freedom
  Multiple R-squared:  0.8632,	Adjusted R-squared:  0.8431 
  F-statistic:  43.1 on 6 and 41 DF,  p-value: 3.589e-16
  
   SSE
  [1] 87972.8
  
  
  
   lm.mod11 <- lm(energy ~ metro + region + toxic*pop + green*pop, data = na.omit(states.data1))
   summary(lm.mod11)
  
  Call:
    lm(formula = energy ~ metro + region + toxic * pop + green * 
         pop, data = na.omit(states.data1))
  
  Residuals:
    Min     1Q Median     3Q    Max 
  -89.77 -21.40  -4.42  26.82  97.75 
  
  Coefficients:
    Estimate Std. Error t value Pr(>|t|)    
  (Intercept)    2.363e+02  3.679e+01   6.423 1.50e-07 ***
    metro         -3.054e-01  4.092e-01  -0.746 0.460001    
  regionN. East -3.194e+01  2.269e+01  -1.408 0.167307    
  regionSouth   -2.319e+01  2.064e+01  -1.124 0.268130    
  regionMidwest -3.758e+01  2.120e+01  -1.773 0.084258 .  
  toxic         -4.179e-01  7.375e-01  -0.567 0.574298    
  pop           -2.583e-06  4.489e-06  -0.575 0.568377    
  green          4.837e+00  5.303e-01   9.122 4.12e-11 ***
    toxic:pop      8.946e-07  2.262e-07   3.955 0.000322 ***
    pop:green     -1.541e-07  3.026e-07  -0.509 0.613487    
  ---
    Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
  
  Residual standard error: 46 on 38 degrees of freedom
  Multiple R-squared:  0.8749,	Adjusted R-squared:  0.8453 
  F-statistic: 29.53 on 9 and 38 DF,  p-value: 1.709e-14
  
   SSE
  [1] 80406.87
  
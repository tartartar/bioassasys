library(drc)
haemolysis <- read.csv("haemolysis_FEB_normalised_0_to_1.csv", header = TRUE)
# Fitting a four-parameter log-logistic model
haemolysis.LL.4.twocurves <- drm(Haemolysis~Concentration, Species, data=haemolysis, fct = LL.4(names = c("b (Slope)", "c (Lower limit)", "d (Upper limit)", "e (EC50)")))
# Fitting a two-parameter log-logistic model
haemolysis.LL.2.twocurves <- drm(Haemolysis~Concentration, Species, data=haemolysis, fct = LL.2(names = c("b (Slope)", "e (EC50)")))
# Fitting Weibull models, two and four parameters
haemolysis.W.2.twocurves <- drm(Haemolysis ~ Concentration, Species, data = haemolysis, fct = W1.2(names = c("b (Slope)", "e")))
haemolysis.W.4.twocurves <- drm(Haemolysis ~ Concentration, Species, data = haemolysis, fct = W1.4(names = c("b (Slope)", "c (Lower limit)", "d (Upper)", "e")))
# Fitting a two-parameter log-logistic model with only one curve for all data
haemolysis.LL.2.onecurve <- drm(Haemolysis ~ Concentration, data = haemolysis, fct = LL.2(names = c("b (Slope)", "e (EC50)")))

# (AIC) Akaike Information Criterion: a lower value indicates a better fit to the data.
AIC(haemolysis.LL.4.twocurves, haemolysis.LL.2.twocurves, haemolysis.W.2.twocurves, haemolysis.W.4.twocurves)
#                           df       AIC
# haemolysis.LL.4.twocurves  9 -83.45740
# haemolysis.LL.2.twocurves  5 -90.35029
# haemolysis.W.2.twocurves   5 -86.84956
# haemolysis.W.4.twocurves   9 -81.60892

plot(haemolysis.LL.2.twocurves, main="LL2 two curves, all data", xlab="NetF concentration (log scale, µg/mL)", ylab="Haemolysis %",type="all")
plot(haemolysis.LL.2.onecurve, main="LL2 one curve, all data", xlab="NetF concentration (log scale, µg/mL)", ylab="Haemolysis %",type="all")

modelFit(haemolysis.LL.2.onecurve)
# Lack-of-fit test

#           ModelDf    RSS Df F value p value
# ANOVA          45 2.6896                   
# DRC model      52 2.7479  7  0.1393  0.9945
modelFit(haemolysis.LL.2.twocurves)
# Lack-of-fit test

#           ModelDf     RSS Df F value p value
# ANOVA          36 0.44487                   
# DRC model      50 0.49301 14  0.2782  0.9935

op <- par(mfrow = c(1, 2)) # plot two graphs in one row
plot(haemolysis.LL.2.onecurve, main="LL2 one curve, mean values", xlab="NetF concentration (log scale, µg/mL)", ylab="Haemolysis %")
plot(haemolysis.LL.2.onecurve, main="LL2 one curve, all data", xlab="NetF concentration (log scale, µg/mL)", ylab="Haemolysis %",type="all")
plot(haemolysis.LL.2.twocurves, main="LL2 two curves, mean values", xlab="NetF concentration (log scale, µg/mL)", ylab="Haemolysis %")
plot(haemolysis.LL.2.twocurves, main="LL2 two curves, all data", xlab="NetF concentration (log scale, µg/mL)", ylab="Haemolysis %",type="all")

summary(haemolysis.LL.2.onecurve)

# Model fitted: Log-logistic (ED50 as parameter) with lower limit at 0 and upper limit at 1 (2 parms)

# Parameter estimates:

#                       Estimate Std. Error  t-value p-value
# b (Slope):(Intercept) -1.25973    0.23277 -5.41202       0
# e (EC50):(Intercept)   2.70326    0.46503  5.81303       0

# Residual standard error:

# 0.2298806 (52 degrees of freedom)
summary(haemolysis.LL.2.twocurves)

# Model fitted: Log-logistic (ED50 as parameter) with lower limit at 0 and upper limit at 1 (2 parms)

# Parameter estimates:

#                 Estimate Std. Error  t-value p-value
# b (Slope):horse -1.85611    0.24614 -7.54082       0
# b (Slope):sheep -2.45647    0.36243 -6.77783       0
# e (EC50):horse   7.41137    0.64127 11.55734       0
# e (EC50):sheep   1.11220    0.08394 13.24990       0

# Residual standard error:

# 0.09929824 (50 degrees of freedom)

anova(haemolysis.LL.2.onecurve, haemolysis.LL.2.twocurves)

# 1st model
# fct:      LL.2(names = c("b (Slope)", "e (EC50)"))
# pmodels: 1 (for all parameters)
# 2nd model
# fct:      LL.2(names = c("b (Slope)", "e (EC50)"))
# pmodels: Species (for all parameters)

# ANOVA table

#           ModelDf     RSS Df F value p value
# 1st model      52 2.74794                   
# 2nd model      50 0.49301  2  114.35    0.00

EDcomp(haemolysis.LL.2.twocurves, c(50,50), interval="delta")

# Estimated ratios of effect doses
# (Delta method-based confidence interval(s))

#                   Estimate  Lower  Upper
# horse/sheep:50/50   6.6637 5.1270 8.2005

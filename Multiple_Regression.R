#Multiple Regression
library(ggplot2)
library(dplyr)
library (stringr)
library(data.table)
library(grid)
library(gridExtra)
library(corrplot)
library(scales)
library(qqplotr)
library(MASS)
library(DMwR)
library(car)
library(e1071)
library(caret)
library(caTools)
library(pROC)
library(tidyverse)
library(MVA)
library(GGally)
library(gvlma)
bank=read.csv("C:/Users/Shamali/Desktop/RutgersSpring/multivariat/project/Newfolder/bank.csv",row.names=1,fill=TRUE)
attach(bank)
# Performing multiple regression on bank dataset
fit <- lm(Deposit~Job+Education+Marital+Housing, data=bank)
#show the results
summary(fit)
#Summary has three sections. Section1: How well does the model fit the data (before Coefficients). Section2: Is the hypothesis supported? (until sifnif codes). Section3: How well does data fit the model (again).
# Useful Helper Functions
coefficients(fit)
?mtcars
ggpairs(data=bank, title="bank Data")
#library(FFally)
library(GGally)
#install.packages("GGally", lib="/Library/Frameworks/R.framework/Versions/3.5/Resources/library")
library(GGally)
ggpairs(data=mtcars, title="Cars Data")
confint(fit,level=0.95)
# Predicted Values
fitted(fit)
residuals(fit)
#Anova Table
anova(fit)
vcov(fit)
cov2cor(vcov(fit))
temp <- influence.measures(fit)
temp
View(temp)
#diagnostic plots
plot(fit)
# Assessing Outliers
outlierTest(fit)
qqPlot(fit, main="QQ Plot")
leveragePlots(fit) # leverage plots
# Influential Observations
# added variable plots
avPlots(fit)
# Cook's D plot
# identify D values > 4/(n-k-1)
cutoff <- 4/((nrow(bank)-length(fit$coefficients)-2))
plot(fit, which=4, cook.levels=cutoff)
# Influence Plot
influencePlot(fit, id.method="identify", main="Influence Plot", sub="Circle size is proportial to Cook's Distance" )
# Normality of Residuals
# qq plot for studentized resid
qqPlot(fit, main="QQ Plot")
# distribution of studentized residuals
library(MASS)
sresid <- studres(fit)
hist(sresid, freq=FALSE,
     main="Distribution of Studentized Residuals")
xfit<-seq(min(sresid),max(sresid),length=40)
yfit<-dnorm(xfit)
lines(xfit, yfit)
#Non-constant Error Variance
# Evaluate homoscedasticity
# non-constant error variance test
ncvTest(fit)
# plot studentized residuals vs. fitted values
spreadLevelPlot(fit)
#Multi-collinearity
# Evaluate Collinearity
vif(fit) # variance inflation factors
sqrt(vif(fit)) > 2 # problem?
#Nonlinearity
# component + residual plot
crPlots(fit)
# Ceres plots
ceresPlots(fit)
#Non-independence of Errors
# Test for Autocorrelated Errors
durbinWatsonTest(fit)
# Global test of model assumptions
library(gvlma)
install.packages("gvlma", lib="/Library/Frameworks/R.framework/Versions/3.5/Resources/library")
library(gvlma)
gvmodel <- gvlma(fit)
summary(gvmodel)
fit
summary(fit)
fit1 <- fit
fit2 <- lm(Deposit~Job+Education+Marital, data = bank)
# compare models
anova(fit1, fit2)
step <- stepAIC(fit, direction="both")
step$anova # display results
install.packages("leaps", lib="/Library/Frameworks/R.framework/Versions/3.5/Resources/library")
library(leaps)
leaps<-regsubsets(mpg~disp+hp+drat+wt+qsec,data=mtcars,nbest=10)
# view results
summary(leaps)
# plot a table of models showing variables in each model.
# models are ordered by the selection statistic.
plot(leaps)
plot(leaps,scale="r2")
subsets(leaps, statistic="rsq")
# All Subsets Regression
plot(leaps,scale="bic")
summary(leaps)
?regsubsets
summary(leaps)
View(leaps)
leaps
coef(leaps,1:5)
# Calculate Relative Importance for Each Predictor
install.packages("relaimpo", lib="/Library/Frameworks/R.framework/Versions/3.5/Resources/library")
library(relaimpo)
calc.relimp(fit,type=c("lmg","last","first","pratt"),
            rela=TRUE)

summary(fit)
predict.lm(fit, data.frame(wt =3.2 ,drat=3.9,hp=130,disp=150) )

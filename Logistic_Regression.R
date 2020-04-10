## Logistic Regression


##Calling library
library(ggplot2)
#install.packages("cowplot", lib="/Library/Frameworks/R.framework/Versions/3.5/Resources/library")
library(cowplot)

## Few packages for confusion matrix. Lets look at them one by one
#install.packages("regclass", lib="/Library/Frameworks/R.framework/Versions/3.5/Resources/library")
library(regclass)
#install.packages("caret", lib="/Library/Frameworks/R.framework/Versions/3.5/Resources/library")
library(caret)
#install.packages("e1071", lib="/Library/Frameworks/R.framework/Versions/3.5/Resources/library")
library(e1071)
#install.packages("pROC", lib="/Library/Frameworks/R.framework/Versions/3.5/Resources/library")
library(pROC)


#Loading Dataset
bank=read.csv("C:/Users/Deepal/Desktop/MVA/bank.csv",fill=TRUE)
View(bank)
attach(bank)
head(bank)
str(bank)

# This shows that we need to tell R which columns contain factors it also shows us that there are some missing values. There are "?"s in the dataset.
# These are in the "ca" and "thal" columns. First, convert "?"s to NAs...
bank[bank == "?"] <- NA

## For some logistic regression we'll create a very simple model that uses deposit to predict default
xtabs(~ deposit + default, data=bank)
#Customer who doesnot fall into default category are the one bank should target the most.

logistic_simple <- glm(deposit ~ default, data=bank, family="binomial")
summary(logistic_simple)


Nodeposit.log.odds <- log(5237 / 5757)
Nodeposit.log.odds

Yesdeposit.log.odds.ratio <- log((52 / 116) / (5237/5757))
Yesdeposit.log.odds.ratio

## Now calculate the overall "Pseudo R-squared" and its p-value
ll.null <- logistic_simple$null.deviance/-2
ll.proposed <- logistic_simple$deviance/-2
ll.null
ll.proposed

## McFadden's Pseudo R^2 = [ LL(Null) - LL(Proposed) ] / LL(Null)
(ll.null - ll.proposed) / ll.null

## chi-square value = 2*(LL(Proposed) - LL(Null))
## p-value = 1 - pchisq(chi-square value, df = 2-1)
1 - pchisq(2*(ll.proposed - ll.null), df=1)

##
predicted.data <- data.frame(probability.of.deposit=logistic_simple$fitted.values,default=bank$default)
head(predicted.data)

## We can plot the data...
ggplot(data=predicted.data, aes(x=default, y=probability.of.deposit)) +
  geom_point(aes(color=default), size=5) +
  xlab("default") +
  ylab("Predicted probability of getting deposited")

## Since there are only two probabilities (one for default and one for not default),
## we can use a table to summarize the predicted probabilities.
xtabs(~ probability.of.deposit + default, data=predicted.data)


## Now we will use all of the data available for prediction. This is not the best way to do this
logistic <- glm(deposit ~ ., data=bank, family="binomial")
summary(logistic)

## Now calculate the overall "Pseudo R-squared" and its p-value
ll.null <- logistic$null.deviance/-2
ll.proposed <- logistic$deviance/-2

## McFadden's Pseudo R^2 = [ LL(Null) - LL(Proposed) ] / LL(Null)
(ll.null - ll.proposed) / ll.null

## The p-value for the R^2
1 - pchisq(2*(ll.proposed - ll.null), df=(length(logistic$coefficients)-1))

## now we can plot the data
predicted.data <- data.frame(probability.of.deposit=logistic$fitted.values,deposit=bank$deposit)
predicted.data <- predicted.data[order(predicted.data$probability.of.deposit, decreasing=FALSE),]
predicted.data$rank <- 1:nrow(predicted.data)

## Lastly, we can plot the predicted probabilities for each sample having
ggplot(data=predicted.data, aes(x=rank, y=probability.of.deposit)) +
  geom_point(aes(color=deposit), alpha=1, shape=4, stroke=2) +
  xlab("Index") +
  ylab("Predicted probability of getting deposited")

#confusion matrix
confusion_matrix(logistic)
pdata <- predict(logistic,newdata=bank,type="response" )
head(pdata)
head(bank$deposit)
pdataF <- as.factor(ifelse(test=as.numeric(pdata>0.5) == 0, yes="Deposit", no="NotDeposit"))


roc(bank$deposit,logistic$fitted.values,plot=TRUE)
par(pty = "s")
roc(bank$deposit,logistic$fitted.values,plot=TRUE)

## NOTE: By default, roc() uses specificity on the x-axis and the values range
## from 1 to 0. This makes the graph look like what we would expect, but the
## x-axis itself might induce a headache. To use 1-specificity (i.e. the
## False Positive Rate) on the x-axis, set "legacy.axes" to TRUE.
roc(bank$deposit,logistic$fitted.values,plot=TRUE, legacy.axes=TRUE)
roc(bank$deposit,logistic$fitted.values,plot=TRUE, legacy.axes=TRUE, xlab="False Positive Percentage", ylab="True Postive Percentage")
roc(bank$deposit,logistic$fitted.values,plot=TRUE, legacy.axes=TRUE, xlab="False Positive Percentage", ylab="True Postive Percentage", col="#377eb8", lwd=4)
roc(bank$deposit,logistic$fitted.values,plot=TRUE, legacy.axes=TRUE, xlab="False Positive Percentage", ylab="True Postive Percentage", col="#377eb8", lwd=4)

## If we want to find out the optimal threshold we can store the
## data used to make the ROC graph in a variable...
roc.info <- roc(bank$deposit, logistic$fitted.values, legacy.axes=TRUE)
str(roc.info)
roc.df <- data.frame(tpp=roc.info$sensitivities*100, ## tpp = true positive percentage
                     fpp=(1 - roc.info$specificities)*100, ## fpp = false positive precentage
                     thresholds=roc.info$thresholds)
head(roc.df) ## head() will show us the values for the upper right-hand corner of the ROC graph, when the threshold is so low
tail(roc.df) ## tail() will show us the values for the lower left-hand corner

## now let's look at the thresholds between TPP 60% and 80%
#roc.df[roc.df$tpp > 60 & roc.df$tpp < 80,]
roc(bank$deposit,logistic$fitted.values,plot=TRUE, legacy.axes=TRUE, xlab="False Positive Percentage", ylab="True Postive Percentage", col="#377eb8", lwd=4, percent=TRUE)
roc(bank$deposit,logistic$fitted.values,plot=TRUE, legacy.axes=TRUE, xlab="False Positive Percentage", ylab="True Postive Percentage", col="#377eb8", lwd=4, percent=TRUE, print.auc=TRUE)
roc(bank$deposit,logistic$fitted.values,plot=TRUE, legacy.axes=TRUE, xlab="False Positive Percentage", ylab="True Postive Percentage", col="#377eb8", lwd=4, percent=TRUE, print.auc=TRUE, partial.auc=c(100, 90), auc.polygon = TRUE, auc.polygon.col = "#377eb822", print.auc.x=45)

# Lets do two roc plots to understand which model is better
roc(bank$deposit, logistic_simple$fitted.values, plot=TRUE, legacy.axes=TRUE, percent=TRUE, xlab="False Positive Percentage", ylab="True Postive Percentage", col="#377eb8", lwd=4, print.auc=TRUE)

# Lets add the other graph
plot.roc(bank$deposit, logistic$fitted.values, percent=TRUE, col="#4daf4a", lwd=4, print.auc=TRUE, add=TRUE, print.auc.y=40)
legend("bottomright", legend=c("Simple", "Non Simple"), col=c("#377eb8", "#4daf4a"), lwd=4) # Make it user friendly





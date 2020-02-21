library(data.table)
library(tidyverse)  # data manipulation
library(data.table) # fast file reading
library(gridExtra)  # arranging ggplot in grid
library(rmarkdown)
library(tinytex)
library(latexpdf)
library(latex2exp)



#Convert the data frame to data table
setDT(bank)
#Describe the columns and their data types
str(bank)
#By head we get to know first n rows to get grasp of the data
head(bank)
#Find NA In the data table.

table(is.na(bank))

#Find NA in Columns.
bank[is.na(age),NROW(age)]
bank[is.na(job),NROW(job)]
bank[is.na(education),NROW(education)]
grep('NA',bank)

#Find different elements in the column
unique(bank$job)
unique(bank$marital)
#Summary of dataset

summary(bank)

#bank1=bank.head(1000)
#bank1=tail(bank)
#pairs(bank)

#Take sample of 1000 from the dataset.
bank12=bank[sample(.N,1000)]

#check for duplicate rows
sum(duplicated(bank))

#check for rows which contain missing data
sum(!complete.cases(bank))

#Check for rows which have complete missing values in all columns
all.empty = rowSums(is.na(bank))==ncol(bank)
sum(all.empty)

#check for missing value by variable
sapply(bank, function(x) sum(is.na(x)))

#Remove rows with all columns missing value
bank.clean = bank[!all.empty,]

#Create New Column To Indicate Missing Detection
bank.clean$missing = !complete.cases(bank.clean)

#Missing Numeric Value Treatment

#Replace with Average
bank.clean$age[is.na(bank.clean$age)] = mean(bank$age, na.rm=T)
bank.clean$day[is.na(bank.clean$day)] = mean(bank$day, na.rm=T)
bank.clean$duration[is.na(bank.clean$duration)] = mean(bank$duration, na.rm=T)
bank.clean$previous[is.na(bank.clean$previous)] = mean(bank$previous, na.rm=T)
bank.clean$campaign[is.na(bank.clean$campaign)] = mean(bank$campaign, na.rm=T)

hist(bank.clean$balance)
hist(bank.clean$pdays)

bank.clean$pdays[is.na(bank.clean$pdays)] = as.numeric(names(sort(-table(bank$pdays)))[1])
bank.clean$balance[is.na(bank.clean$balance)] = as.numeric(names(sort(-table(bank$balance)))[1])

#EDA
summary(bank$age)

library(ggplot2)
gg = ggplot (bank) 
graph1 = gg + geom_histogram(aes(x=age),color="black", fill="white", binwidth = 5) +
  ggtitle('Age Distribution (red mean line)') +
  ylab('Count') +
  xlab('Age') +
  geom_vline(aes(xintercept = mean(age), color = "red")) +
  scale_x_continuous(breaks = seq(0,100,5)) +
  theme(legend.position = "none")

graph2 = gg + geom_boxplot(aes(x='', y=age)) +
  ggtitle('Age Boxplot') +
  ylab('Age')
graph2
library(gridExtra)
grid.arrange(graph1, graph2, ncol = 2)

#Age Distribution vs Marital Status That Subscribes Term Deposit

graph3 <- ggplot(bank, aes(x=age, fill=marital)) + 
  geom_histogram(binwidth = 2, alpha=0.7) +
  facet_grid(cols = vars(deposit)) +
  expand_limits(x=c(0,100)) +
  scale_x_continuous(breaks = seq(0,100,10)) +
  ggtitle("Age Distribution by Marital Status")

graph3

#Age Boxplot vs Education Level That Subscribes Term Deposit

graph4 <- ggplot(bank, aes(x='', y=age, fill=education)) +
  geom_boxplot() +
  facet_grid(cols = vars(deposit)) +
  coord_flip() +
  ggtitle("Age Boxplot by Education Level") +
  ylab("Age") +
  xlab("Education") +
  theme(legend.position = "bottom")
graph4

#Subscription Analysis

#Age vs Subscription
ggplot (bank, aes(x=age)) + geom_histogram() +
  facet_grid(cols=vars(deposit)) + 
  ggtitle('Age Distribution') + ylab('Count') + xlab('Age')

#Balance vs Subscription
ggplot (bank, aes(x=balance)) + geom_histogram() +
  facet_grid(cols=vars(deposit)) + 
  ggtitle('Balance Histogram') + ylab('Count') + xlab('Balance')

#Education vs Subscription
ggplot(data = bank, aes(x=education, fill=deposit)) +
  geom_bar() +
  ggtitle("Term Deposit Subscription based on Education Level") +
  xlab(" Education Level") +
  guides(fill=guide_legend(title="Subscription of Term Deposit"))


#Day vs Subscription

ggplot (bank, aes(x=day)) + geom_histogram() +
  facet_grid(cols=vars(deposit)) + 
  ggtitle('Day Histogram') + ylab('Count') + xlab('Balance')

#duration


graph5 <- ggplot(bank, aes(x=duration, fill = deposit)) +
  geom_histogram(binwidth = 2) +
  facet_grid(cols = vars(deposit)) +
  coord_cartesian(xlim = c(0,5000), ylim = c(0,400))
graph5

#Barplot of Duration by Age
ggplot(bank, aes(age, duration)) +
  geom_col() +
  facet_grid(cols = vars(deposit))

#Scatterplot of Duration s Balance

ggplot(bank, aes(x=duration, y=balance)) +
  facet_grid(cols = vars(deposit)) +
  geom_point(shape=1)




ggplot (bank12 ,aes (x=job)) + geom_bar(fill='blue')
ggplot (bank12 ,aes (x=age)) + geom_bar(fill = 'red')
ggplot (bank12 ,aes (x=marital)) + geom_bar(fill = 'yellow')
ggplot (bank12 ,aes (x=education)) + geom_bar(fill = 'black')
ggplot (bank12 ,aes (x=default)) + geom_bar(fill = 'pink')
ggplot (bank12 ,aes (x=housing)) + geom_bar(fill = 'orange')
ggplot (bank12 ,aes (x=loan)) + geom_bar(fill = 'yellow')
ggplot (bank12 ,aes (x=contact)) + geom_bar(fill = 'red')
ggplot (bank12 ,aes (x=month)) + geom_bar(fill = 'green')
ggplot (bank12 ,aes (x=deposit)) + geom_bar(fill = 'black')


ggplot (bank12 ,aes (y= default ,x=balance )) + geom_boxplot ( fill ='yellow ')

ggplot(bank12 ,aes(x=default ,y= balance))+geom_boxplot( color ='purple')
ggplot(bank12 ,aes(x=default ,y= age))+geom_point( color ='black')
ggplot(bank12 ,aes(x=default ,y= duration))+geom_boxplot( color ='blue')
ggplot(bank12 ,aes(x=default ,y= job))+geom_boxplot( color ='blue')

ggplot(bank12 ,aes(x=balance ,y= job))+geom_point( color ='blue')

#Correlation
setDF(bank)
corr_data<-data.frame(bank$default,bank$balance,bank$housing)
corr<-cor(corr_data)
corr


#T-test

setDT(bank)
with(data=bank,t.test(age[default=="1"],age[default=="0"],var.equal=TRUE))
with(data=bank,t.test(balance[default=="1"],balance[default=="0"],var.equal=TRUE))
with(data=bank,t.test(duration[default=="1"],duration[default=="0"],var.equal=TRUE))
with(data=bank,t.test(housing[default=="1"],housing[default=="0"],var.equal=TRUE))

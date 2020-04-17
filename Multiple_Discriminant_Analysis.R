#Multiple Discriminant Analysis

##Calling library
library(data.table)
library(data.table) # fast file reading
library(gridExtra)  # arranging ggplot in grid
library(rmarkdown)
library(tinytex)
library(latexpdf)
library(latex2exp)
library(MASS)
#install.packages("ROCR", lib="/Library/Frameworks/R.framework/Versions/3.5/Resources/library")
library(ROCR)


bank=read.csv("C:/Users/Deepal/Desktop/MVA/bank.csv")
#View(bank)

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


#Take sample of 1000 from the dataset.
bankdata=bank[sample(.N,1000)]
bank=bankdata
#View(bank)
dim(bank)

#we need to convert to matrix to facilitate distance measurement
bank.data <- as.matrix(bank[,c(0:16)])
dim(bank.data)
dim(bank)
bank_raw <- cbind(bank.data, as.numeric(bank$deposit)-1)
dim(bank_raw)
colnames(bank_raw)[17] <- "deposit"


#View(bank_raw)
# Lets cut the data into two parts
smp_size_raw <- floor(0.75 * nrow(bank_raw))
train_ind_raw <- sample(nrow(bank_raw), size = smp_size_raw)
train_raw.df <- as.data.frame(bank_raw[train_ind_raw, ])
test_raw.df <- as.data.frame(bank_raw[-train_ind_raw, ])
# We now have a training and a test set. Training is 75% and test is 25%

bank_raw.lda <- lda(formula = train_raw.df$deposit ~ default+loan+job+marital+age+education+housing, data = train_raw.df)
bank_raw.lda
summary(bank_raw.lda)
print(bank_raw.lda)
plot(bank_raw.lda)
bank_raw.lda.predict <- lda(formula = train_raw.df$deposit ~ default+loan+job+marital+education+housing, data = train_raw.df)
bank_raw.lda.predict$class
bank_raw.lda.predict$x

# Get the deposit as a dataframe.
bank_raw.lda.predict.deposit <- as.data.frame(bank_raw.lda.predict$deposit)
dim(bank_raw.lda.predict.deposit)

# As LDA predict gives output as NULL, we can't further apply LDA on our dataset.


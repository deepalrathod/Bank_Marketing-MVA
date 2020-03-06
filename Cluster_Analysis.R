#bankk$default=factor(c("yes","no"))

#bankk$loan=factor(c("yes","no"))
#bankk$deposit=factor(c("yes","no"))
#bankk$job=factor(c("retired","unemployed","management","admin.","services","blue-collar","technician","housemaid","self-employed","entrepreneur","student"))

#abc=bankk[,c(1,5,6,9:17)]
library(reprex)
bankk=data.frame(as.numeric(as.factor(bankk$job)),
                 as.numeric(as.factor(bankk$marital)),
                 as.numeric(as.factor(bankk$education)),
                 as.numeric(as.factor(bankk$housing)),
                 as.numeric(as.factor(bankk$loan)),
                 as.numeric(as.factor(bankk$default)),
                 as.numeric(as.factor(bankk$deposit)))
colnames(bankk) <- c("Job", "Marital", "Education", "Housing", "Loan","Default","Deposit")

bank=bankk

#Bank=cbind(abc,bank)
#bank1=bankk[,!(names(bankk) %in% abc)]
#Bank <- bankk[sample(nrow(bankk),500),]
library(data.table)
setDT(bank)


#bankk[,"default" :=as.numeric(default)]
#bankk[,"loan" :=as.numeric(loan)]
#bankk[,"deposit" :=as.numeric(deposit)]
#bankk[,"job" :=as.numeric(job)]
unique(bank$Job)

bank=bank[sample(.N,50)]
bank

set.seed(123)
bank <- kmeans(bank, centers=5)

#plot(Job,Deposit)

install.packages("cluster", lib="/Library/Frameworks/R.framework/Versions/3.5/Resources/library")
library(cluster)


#bankk$default=factor(c("yes","no"))
#bankk$loan=factor(c("yes","no"))
#bankk$deposit=factor(c("yes","no"))
#library(data.table)
#setDT(bankk)



#bankk[,"default" :=as.numeric(default)]
#bankk[,"loan" :=as.numeric(loan)]
#bankk[,"deposit" :=as.numeric(deposit)]
#bank=bankk[sample(.N,100)]

# Bank
#bank<- bank[ ,c(12:17)]
#bank123=bank[,c(12:17)]
#bank=abc

bank=read.csv("C:/Users/Shamali/Desktop/RutgersSpring/multivariat/project/Newfolder/bank.csv",row.names=1,fill=TRUE)
matstd.can <- scale(bank)

# Creating a (Euclidean) distance matrix of the standardized data 
dist.bank <- dist(matstd.can, method="euclidean")

# Invoking hclust command (cluster analysis by single linkage method)      
clusbank.nn <- hclust(dist.bank, method = "single") 
plot(dist.bank)

# Plotting vertical dendrogram      
# create extra margin room in the dendrogram, on the bottom (Canine species' labels)

# New Example

#Euroemp <- read.csv("~/Desktop/MVA/R Exercise/K25945 - R code and data sets/Chapter 9/Euroemp.csv")
#bank <- Euroemp

attach(bank)
dim(bank)
# Hirerarchic cluster analysis, Nearest-neighbor

# Standardizing the data with scale()
matstd.bank <- scale(bank)
# Creating a (Euclidean) distance matrix of the standardized data
dist.bank <- dist(matstd.bank, method="euclidean")
plot(dist.bank)
# Invoking hclust command (cluster analysis by single linkage method)
clusbank.nn <- hclust(dist.bank, method = "single")


#Plotting

# Create extra margin room in the dendrogram, on the bottom (Countries labels)
par(mar=c(8, 4, 4, 2) + 0.1)
# Object "clusbank.nn" is converted into a object of class "dendrogram"
# in order to allow better flexibility in the (vertical) dendrogram plotting.

plot(as.dendrogram(clusbank.nn))
#boxplot(elev~clusbank.nn)
#,ylab="Distance between countries",ylim=c(0,6),
#main="Dendrogram. People banked in nine industry groups \n  from European countries")

#Horizontal Dendrogram

dev.new()
plot(as.dendrogram(clusbank.nn))



### Let's get Lazy

# We will use agnes function as it allows us to select option for data standardization, the distance measure and clustering algorithm in one single function

??agnes
(agn.bank <- agnes(bank, metric="euclidean", stand=TRUE, method = "single"))

View(agn.bank)

#  Description of cluster merging
agn.bank$merge

#Dendogram
plot(as.dendrogram(agn.bank))
#, xlab= "Distance between Countries",xlim=c(8,0),
#horiz = TRUE,main="Dendrogram \n Bankment in nine industry groups in European countries")

#Interactive Plots

plot(agn.bank,ask=TRUE)
plot(agn.bank, which.plots=2)


#K-Means Clustering
bank111=na.omit(bank)
#Euroemp <- read.csv("~/Desktop/MVA/R Exercise/K25945 - R code and data sets/Chapter 9/Euroemp.csv", header=TRUE,row.names=1)
attach(bank)
# Standardizing the data with scale()
matstd.bank <- scale(bank111)
# K-means, k=2, 3, 4, 5, 6
# Centers (k's) are numbers thus, 10 random sets are chosen

kmeans2.bank <- kmeans(na.omit(matstd.bank),2) # this helps in omitting NA 
kmeans2.bank

#(kmeans2.bank <- kmeans(matstd.bank,2,nstart = 5))
# Computing the percentage of variation accounted for. Two clusters
perc.var.2 <- round(100*(1 - kmeans2.bank$betweenss/kmeans2.bank$totss),1)
names(perc.var.2) <- "Perc. 2 clus"
perc.var.2

# Computing the percentage of variation accounted for. Three clusters
kmeans3.bank <- kmeans(na.omit(matstd.bank),2) # this helps in omitting NA 

#(kmeans3.bank <- kmeans(matstd.bank,3,nstart = 10))
perc.var.3 <- round(100*(1 - kmeans3.bank$betweenss/kmeans3.bank$totss),1)
names(perc.var.3) <- "Perc. 3 clus"
perc.var.3

# Computing the percentage of variation accounted for. Four clusters
kmeans4.bank <- kmeans(na.omit(matstd.bank),2) # this helps in omitting NA 

#(kmeans4.bank <- kmeans(matstd.bank,4,nstart = 10))
perc.var.4 <- round(100*(1 - kmeans4.bank$betweenss/kmeans4.bank$totss),1)
names(perc.var.4) <- "Perc. 4 clus"
perc.var.4

# Computing the percentage of variation accounted for. Five clusters
#(kmeans5.bank <- kmeans(matstd.bank,5,nstart = 10))

#kmeans5.bank <- kmeans(na.omit(matstd.bank),2) # this helps in omitting NA 

#perc.var.5 <- round(100*(1 - kmeans5.bank$betweenss/kmeans5.bank$totss),1)
#names(perc.var.5) <- "Perc. 5 clus"
#perc.var.5

#(kmeans6.bank <- kmeans(matstd.bank,6,nstart = 10))
#kmeans6.bank <- kmeans(na.omit(matstd.bank),2) # this helps in omitting NA 

# Computing the percentage of variation accounted for. Six clusters
#perc.var.6 <- round(100*(1 - kmeans6.bank$betweenss/kmeans6.bank$totss),1)
#names(perc.var.6) <- "Perc. 6 clus"
#perc.var.6
#
# Saving four k-means clusters in a list
is.na(kmeans4.bank)
clus1 <- matrix(names(kmeans4.bank$cluster[kmeans4.bank$cluster == 1]), 
                ncol=1, nrow=length(kmeans4.bank$cluster[kmeans4.bank$cluster == 1]))

colnames(clus1) <- "Cluster 1"
#clus1 <- matrix(names(kmeans4.bank$cluster[kmeans4.bank$cluster == 1])),ncol=1, nrow=length(kmeans4.bank$cluster[kmeans4.bank$cluster == 1]))
clus2 <- matrix(names(kmeans4.bank$cluster[kmeans4.bank$cluster == 2]), 
                ncol=1, nrow=length(kmeans4.bank$cluster[kmeans4.bank$cluster == 2]))
colnames(clus2) <- "Cluster 2"
clus3 <- matrix(names(kmeans4.bank$cluster[kmeans4.bank$cluster == 3]), 
                ncol=1, nrow=length(kmeans4.bank$cluster[kmeans4.bank$cluster == 3]))
colnames(clus3) <- "Cluster 3"
clus4 <- matrix(names(kmeans4.bank$cluster[kmeans4.bank$cluster == 4]), 
                ncol=1, nrow=length(kmeans4.bank$cluster[kmeans4.bank$cluster == 4]))
colnames(clus4) <- "Cluster 4"
list(clus1,clus2,clus3,clus4)

detach(bank)

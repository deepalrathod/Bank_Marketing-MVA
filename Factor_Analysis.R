bank=read.csv("C:/Users/Shamali/Desktop/RutgersSpring/multivariat/project/Newfolder/bank.csv",row.names=1,fill=TRUE)
attach(bank)
corrm.bank <- cor(bank)
corrm.bank
#high negative number is good #they will get grouped together for sure
plot(corrm.bank)
bank_pca <- prcomp(bank, scale=TRUE)#pca
summary(bank_pca)#see sCREe diagram
plot(bank_pca)
# A table containing eigenvalues and %'s accounted, follows. Eigenvalues are the sdev^2
(eigen_bank <- round(bank_pca$sdev^2,2))
names(eigen_bank) <- paste("PC",1:7,sep="")
eigen_bank
sumlambdas <- sum(eigen_bank)
sumlambdas
cumvar_bank <- cumsum(propvar)
propvar <- round(eigen_bank/sumlambdas,2)
propvar
cumvar_bank <- cumsum(propvar)
cumvar_bank
matlambdas <- rbind(eigen_bank,propvar,cumvar_bank)
matlambdas
rownames(matlambdas) <- c("Eigenvalues","Prop. variance","Cum. prop. variance")
rownames(matlambdas)
eigvec.bank<- bank_pca$rotation
print(bank_pca)#pc1=0.17*marital
# Taking the first four PCs to generate linear combinations for all the variables with four factors
(eigen_bank <- bank_pca$sdev^2)
names(eigen_bank) <- paste("PC",1:7,sep="")
eigen_bank
#SCREE DIAGRAM
plot(eigen_bank, xlab = "Component number", ylab = "Component variance", type = "l", main = "Scree diagram")
#FROM SCREE DIAGRAM WE UNDERSTOOD THAT WE NEED YTO MAKE 4 FACTORS
pcafactors.bank <- eigvec.bank[,1:4]#according to ske
pcafactors.bank
# Multiplying each column of the eigenvector's matrix by the square-root of the corresponding eigenvalue in order to get the factor loadings
unrot.fact.bank <- sweep(pcafactors.bank,MARGIN=2,bank_pca$sdev[1:4],`*`)
unrot.fact.bank #factors education housing and default can come together in pc1 as they have high correlation
# Computing communalities is the common variance
communalities.bank<- rowSums(unrot.fact.bank^2)#square of that factor
communalities.bank#1-this will be its unique variance #what the common variance is
# Performing the varimax rotation. The default in the varimax function is norm=TRUE thus, Kaiser normalization is carried out
rot.fact.bank <- varimax(unrot.fact.bank)
View(unrot.fact.bank)
rot.fact.bank
# The print method of varimax omits loadings less than abs(0.1). In order to display all the loadings, it is necessary to ask explicitly the contents of the object $loadings
fact.load.bank <- rot.fact.bank$loadings[1:7,1:4]
fact.load.bank
# Computing the rotated factor scores for the 30 European Countries. Notice that signs are reversed for factors F2 (PC2), F3 (PC3) and F4 (PC4)
scale.bank <- scale(bank)
scale.bank
as.matrix(scale.bank)%*%fact.load.bank%*%solve(t(fact.load.bank)%*%fact.load.bank)
#new values


#simple way of doing the whole process
##Since we have 4 columns that we are considering for factor analysis , we are checking
#how will the variance be distributed across 4 factors and if we really need 4 factors
#for our analysis.
library(psych)
install.packages("psych", lib="/Library/Frameworks/R.framework/Versions/3.5/Resources/library")
library(psych)
fit.pc <- principal(bank, nfactors=4, rotate="varimax")

fit.pc
round(fit.pc$values, 4)
fit.pc$loadings
# Loadings with more digits
for (i in c(1,3,2,4)) { print(fit.pc$loadings[[1,i]])}
# Communalities
fit.pc$communality
# Rotated factor scores, Notice the columns ordering: RC1, RC3, RC2 and RC4
fit.pc$scores
# Play with FA utilities

#factor rotation only in 4 lines
fa.parallel(bank) # See factor recommendation
fa.plot(fit.pc) # See Correlations within Factors
fa.diagram(fit.pc) # Visualize the relationship #to decide which rc to keep look for communalities #so adding it it should be high communalities
vss(bank) # See Factor recommendations for a simple structure

#From Compnent analysis we come to that there are 3 Factors which combine multiple columns. 
#Loan which tends to RC3 we won't convert it into RC3 as it makes no sense to convert it into RC3. vss(bank) # See Factor recommendations for a simple structure 


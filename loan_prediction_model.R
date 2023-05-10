df <- read.csv(file="D:/University/HK222/Probability and Statistics/LoanPrediction/loan_data.csv", na.strings=c("", "NA"), header=TRUE)
loan_prediction_data <- read.csv(file="D:/University/HK222/Probability and Statistics/LoanPrediction/loan_prediction_data.csv", na.strings=c("", "NA"), header=TRUE)
library(plyr) #Run install.packages(plyr) in the console
library(dplyr)
library(mice) #Run install.packages(mice) in the console
library(VIM) #Run install.packages(VIM) in the console
library(ggplot2)

#****************************************************
#Checking the Data
## Do we find something wrong in the data?
## Ambiguous or wrongful variables?
summary(df)
any(is.na(df)) #Return true if exists N/A
sum(is.na(df)) #Total number of N/A in the dataset
colSums(is.na(df)) #list numbers of N/A by column
#=======================================
## Missing data chart
sapply(df, function(x) sum(is.na(x))) #Sum all the missing data and watch for the GLOBAL ENVIRONMENT (mice_plot: List of 7)
mice_plot <- aggr(df, col=c('navyblue','red'),
                  numbers=TRUE, sortVars=TRUE,
                  labels=names(df), cex.axis=.7,
                  gap=3, ylab=c("Missing data","Pattern"))
# Checking the distribution of the data (Watch for outliers)
## Start with the numerical vars
par(mfrow=c(3,2))
#-----------Loan Amount--------------#
hist(df$LoanAmount, 
     main="Histogram for LoanAmount", 
     xlab="Loan Amount", 
     border="blue", 
     col="maroon",
     las=1, 
     breaks=20, prob = TRUE)
lines(density(df$LoanAmount, na.rm = TRUE), col='black', lwd=3)
boxplot(df$LoanAmount, col='maroon',xlab = 'LoanAmount', main = 'Box Plot for Loan Amount')
#-----------Applicant Income--------------#
hist(df$ApplicantIncome, 
     main="Histogram for Applicant Income", 
     xlab="Income", 
     border="blue", 
     col="maroon",
     las=1, 
     breaks=50, prob = TRUE)
lines(density(df$ApplicantIncome), col='black', lwd=3)
boxplot(df$ApplicantIncome, col='maroon',xlab = 'CoapplicantIncome', main = 'Box Plot for Applicant Income')

#-----------Co-Applicant Income--------------#
hist(df$CoapplicantIncome, 
     main="Histogram for Co-Applicant Income", 
     xlab="Income", 
     border="blue", 
     col="maroon",
     las=1, 
     breaks=50, prob = TRUE)
lines(density(df$CoapplicantIncome), col='black', lwd=3)
boxplot(df$CoapplicantIncome, col='maroon',xlab = 'CoapplicantIncome', main = 'Box Plot for CoapplicantIncome')
dev.off()
#=======================================
#####Categorical Missing Data handling
df$Dependents <- revalue(df$Dependents, c('3+'='3'))
val <- unique(df$Gender[!is.na(df$Gender)])
my_mode <- val[which.max(tabulate(match(df$Gender, val)))]
df$Gender[is.na(df$Gender)] = my_mode

val <- unique(df$Married[!is.na(df$Married)])
my_mode <- val[which.max(tabulate(match(df$Married, val)))]
df$Married[is.na(df$Married)] = my_mode
#-------------------------------------------#
val <- unique(df$Self_Employed[!is.na(df$Self_Employed)])
my_mode <- val[which.max(tabulate(match(df$Self_Employed, val)))]
df$Self_Employed[is.na(df$Self_Employed)] = my_mode
#-------------------------------------------#
val <- unique(df$Credit_History[!is.na(df$Credit_History)])
my_mode <- val[which.max(tabulate(match(df$Credit_History, val)))]
df$Credit_History[is.na(df$Credit_History)] = my_mode

#####Numerical Missing Data handling
vec_loan <- df$LoanAmount
vec_loan[is.na(vec_loan)] = median(vec_loan, na.rm = TRUE) #Impute LoanAmount by Median
df$LoanAmount <- vec_loan
#--------------------Loan_Amount_Term-----------------------#
vec_loanT <- df$Loan_Amount_Term
vec_loanT[is.na(vec_loanT)] = median(vec_loanT, na.rm = TRUE)
df$Loan_Amount_Term <- vec_loanT
#--------------------Dependents-----------------------#
vec_dep <- df$Dependents
vec_dep[is.na(vec_dep)] = median(vec_dep, na.rm = TRUE)
df$Dependents = vec_dep
any(is.na(df)) #Return true if exists N/A
sum(is.na(df)) #Total number of N/A in the dataset
colSums(is.na(df)) #list numbers of N/A by column

####Outlier Treatment##########
df$LogLoanAmount <- log(df$LoanAmount)
df$Income <- df$ApplicantIncome + df$CoapplicantIncome
df$ApplicantIncome <- NULL
df$CoapplicantIncome <- NULL
df$LogIncome <- log(df$Income)

par(mfrow=c(2,2))
hist(df$LogLoanAmount,
     main="Histogram for (Logarithmic) Loan Amount", 
     xlab="Loan Amount", 
     border="blue", 
     col="maroon",
     las=1, 
     breaks=20, prob = TRUE)
lines(density(df$LogLoanAmount), col='black', lwd=3)
boxplot(df$LogLoanAmount, col='maroon',xlab = 'Income', main = 'Box Plot for Applicant Income')

hist(df$LogIncome, 
     main="Histogram for Income", 
     xlab="Income", 
     border="blue", 
     col="maroon",
     las=1, 
     breaks=50, prob = TRUE)
lines(density(df$LogIncome), col='black', lwd=3)
boxplot(df$LogIncome, col='maroon',xlab = 'Income', main = 'Box Plot for Applicant Income')
dev.off()
#****************************************************
#*
# Examine if the applicant' Loan Amounts distribution is affected by their Education.
data(df, package="lattice")
ggplot(df, aes(x=LoanAmount, fill=Education)) +
  geom_density() +
  facet_grid(Education~.)

##categorical variables
par(mfrow=c(2,3))
counts <- table(df$Loan_Status, df$Gender)
barplot(counts, main="Loan Status by Gender",
        xlab="Gender", col=c("darkgrey","maroon"),
        legend = rownames(counts))
counts2 <- table(df$Loan_Status, df$Education)
barplot(counts2, main="Loan Status by Education",
        xlab="Education", col=c("darkgrey","maroon"),
        legend = rownames(counts2))
counts3 <- table(df$Loan_Status, df$Married)
barplot(counts3, main="Loan Status by Married",
        xlab="Married", col=c("darkgrey","maroon"),
        legend = rownames(counts3))
counts4 <- table(df$Loan_Status, df$Self_Employed)
barplot(counts4, main="Loan Status by Self Employed",
        xlab="Self_Employed", col=c("darkgrey","maroon"),
        legend = rownames(counts4))
counts5 <- table(df$Loan_Status, df$Property_Area)
barplot(counts5, main="Loan Status by Property_Area",
        xlab="Property_Area", col=c("darkgrey","maroon"),
        legend = rownames(counts5))
counts6 <- table(df$Loan_Status, df$Credit_History)
barplot(counts6, main="Loan Status by Credit_History",
        xlab="Credit_History", col=c("darkgrey","maroon"),
        legend = rownames(counts5))

dev.off()

par(mfrow=c(3,2))
barplot(table(df$Loan_Status), main="Loan Status")
barplot(table(df$Gender),main="Gender")
barplot(table(df$Married),main="Married")
barplot(table(df$Dependents),main="Dependents")
barplot(table(df$Education),main="Education")
barplot(table(df$Self_Employed),main="Self Employed")
dev.off()

par(mfrow=c(1,2))
boxplot(df$LogIncome,main="Income")
boxplot(df$LogLoanAmount,main="Loan Amount")
dev.off()

prop.table(table(df$Loan_Status))
prop.table(table(df$Gender))
prop.table(table(df$Married))
prop.table(table(df$Dependents))
prop.table(table(df$Education))
prop.table(table(df$Self_Employed))
prop.table(table(df$LogIncome))
prop.table(table(df$LogLoanAmount))

par(mfrow=c(6,2))
print(ggplot(df, aes(x=Loan_Status))+geom_bar()+facet_grid(.~Gender)+ggtitle("Loan Status by Gender of Applicant"))
print(ggplot(df, aes(x=Loan_Status))+geom_bar()+facet_grid(.~Married)+ggtitle("Loan Status by Marital Status of Applicant"))
print(ggplot(df, aes(x=Loan_Status))+geom_bar()+facet_grid(.~Dependents)+ggtitle("Loan Status by number of Dependents of Applicant"))
print(ggplot(df, aes(x=Loan_Status))+geom_bar()+facet_grid(.~Education)+ggtitle("Loan Status by Education of Applicant"))
print(ggplot(df, aes(x=Loan_Status))+geom_bar()+facet_grid(.~Self_Employed)+ggtitle("Loan Status by Employment status of Applicant"))
print(ggplot(df, aes(x=Loan_Status))+geom_bar()+facet_grid(.~Loan_Amount_Term)+ggtitle("Loan Status by terms  of loan"))
print(ggplot(df, aes(x=Loan_Status))+geom_bar()+facet_grid(.~Credit_History)+ggtitle("Loan Status by credit history of Applicant"))
print(ggplot(df, aes(x=Loan_Status))+geom_bar()+facet_grid(.~Property_Area)+ggtitle("Loan Status by property area"))
print(ggplot(df, aes(x=Loan_Status,y=LogIncome))+geom_boxplot()+ggtitle("Loan Status by income"))
print(ggplot(df, aes(x=Loan_Status,y=LogLoanAmount))+geom_boxplot()+ggtitle("Loan Status by Loan Amount"))

dev.off()
#****************************************************
########################
set.seed(75)
sample <- sample.int(n = nrow(df), size = floor(.70*nrow(df)), replace = F)
trainnew <- df[sample, ]
testnew  <- df[-sample, ]

trainnew$Loan_Status <- ifelse(trainnew$Loan_Status=="Y",1,0)
testnew$Loan_Status <- ifelse(testnew$Loan_Status=="Y",1,0)

summary(trainnew)
summary(testnew)
##################################
logistic1 <- glm(Loan_Status ~ Credit_History,data = trainnew, family = binomial)
summary(logistic1)

my_prediction_tr1 <- predict(logistic1, newdata = trainnew, type = "response")
table(trainnew$Loan_Status, my_prediction_tr1 > 0.5)
#---------------------------------
logistic_test1 <- glm (Loan_Status ~ Credit_History,data = testnew, family = binomial)
summary(logistic_test1)

my_prediction_te1 <- predict(logistic_test1, newdata = testnew, type = "response")
table(testnew$Loan_Status, my_prediction_te1 > 0.5)

#################################
logistic2 <- glm (Loan_Status ~ Credit_History+Education+Self_Employed+Property_Area+LogLoanAmount+
                    LogIncome,data = trainnew, family = binomial)
summary(logistic2)
my_prediction_tr2 <- predict(logistic2, newdata = trainnew, type = "response")
table(trainnew$Loan_Status, my_prediction_tr2 > 0.5)
#---------------------------------
logistic_test2 <- glm (Loan_Status ~ Credit_History+Education+Self_Employed+Property_Area+LogLoanAmount+
                         LogIncome,data = testnew, family = binomial)
summary(logistic_test2)
my_prediction_te2 <- predict(logistic_test2, newdata = testnew, type = "response")
table(testnew$Loan_Status, my_prediction_te2 > 0.5)


#### Decision Tree
library(rpart)
# grow tree 
dtree <- rpart(Loan_Status ~ Credit_History+Education+Self_Employed+Property_Area+LogLoanAmount+
                 LogIncome,method="class", data=trainnew,parms=list(split="information"))
dtree$cptable
plotcp(dtree)
dtree.pruned <- prune(dtree, cp=.02290076)
library(rpart.plot)
prp(dtree.pruned, type = 2, extra = 104,
    fallen.leaves = TRUE, main="Decision Tree")
dtree.pred <- predict(dtree.pruned, trainnew, type="class")
dtree.perf <- table(trainnew$Loan_Status, dtree.pred,
                    dnn=c("Actual", "Predicted"))
dtree.perf

dtree_test <- rpart(Loan_Status ~ Credit_History+Education+Self_Employed+Property_Area+LogLoanAmount+
                      LogIncome,method="class", data=testnew,parms=list(split="information"))
dtree_test$cptable
plotcp(dtree_test)
dtree_test.pruned <- prune(dtree_test, cp=.01639344)
prp(dtree_test.pruned, type = 2, extra = 104,
    fallen.leaves = TRUE, main="Decision Tree")
dtree_test.pred <- predict(dtree_test.pruned, testnew, type="class")
dtree_test.perf <- table(testnew$Loan_Status, dtree_test.pred,
                         dnn=c("Actual", "Predicted"))
dtree_test.perf

#### Random Forest
library(randomForest) 
set.seed(42) 
fit.forest <- randomForest(Loan_Status ~ Credit_History+Education+Self_Employed+Property_Area+LogLoanAmount+
                             LogIncome, data=trainnew,
                           na.action=na.roughfix,
                           importance=TRUE)
fit.forest
importance(fit.forest, type=2)
forest.pred <- predict(fit.forest, testnew)
forest.perf <- table(testnew$Loan_Status, forest.pred,
                     dnn=c("Actual", "Predicted"))
forest.perf
summary(df)

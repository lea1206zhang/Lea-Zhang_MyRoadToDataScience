Lea Zhang
2019-01-28

IBM HR Analytics
======================
------------------------------------------------------------------------

Contents:
-----------------------------------
[Abstract](https://github.com/lea1206zhang/Lea-Zhang_MyRoadToDataScience/blob/master/IBM%20HR%20Analytics.md#abstract)

[Part One Explotary Data Analysis](https://github.com/lea1206zhang/Lea-Zhang_MyRoadToDataScience/blob/master/IBM%20HR%20Analytics.md#part-one-explotary-data-analysis)

[Part Two Unsupervised Learning](https://github.com/lea1206zhang/Lea-Zhang_MyRoadToDataScience/blob/master/IBM%20HR%20Analytics.md#part-two-unsupervised-learning)

[Part Three Modeling](https://github.com/lea1206zhang/Lea-Zhang_MyRoadToDataScience/blob/master/IBM%20HR%20Analytics.md#part-three-modeling)

[Part Four Evaluation](https://github.com/lea1206zhang/Lea-Zhang_MyRoadToDataScience/blob/master/IBM%20HR%20Analytics.md#part-four-evaluation)

[Part Five Deployment](https://github.com/lea1206zhang/Lea-Zhang_MyRoadToDataScience/blob/master/IBM%20HR%20Analytics.md#part-five-deployment)

------------------------------------------------------------------------

# Abstract:
---------
Through the use of data science analysis techniques we where able to form a picture of 
what factors are important in determining employee attrition. Using these results IBM HR will
better understand what employees find important for job satisfaction. We provide a recommendation 
on how to implement a targeted plan to retain more employees at risk of leaving. These results 
can also be used in making future hiring decisions by preemptively avoiding groups of people with 
a higher risk of leaving. 

------------------------------------------------------------------------

# Part One Explotary Data Analysis
------------------------------------------------------

### Data Acquisition

The data is downloaded from the following link <https://www.kaggle.com/pavansubhasht/ibm-hr-analytics-attrition-dataset>

This dataset contains 1470 observations of 35 variables, as the table shown below. Basically, there are three aspects of data: 
1) the basic information about the employee, including age, education, number of company worked.
2) the basic information about the job, including income level, job level.
3) the attitude of the employee about the job, including job satisfaction, work life balance, relationship satisfaction.
And there’s no NULL value in the dataset.

``` r
#---------------------------------------------------------------------- View Data
setwd("~/Desktop/data project/IBM HR Analytics Employee Attrition ")
EmployeeAtt <- read.csv('EmployeeAttrition.csv')
install.packages('gridExtra')
library(ggplot2)
library(gridExtra)
summary(EmployeeAtt)
head(EmployeeAtt)
str(EmployeeAtt)
#----------------------------------------------------------------------------------------- Data Cleansing
EmployeeAtt<-unique(EmployeeAtt)
summary(EmployeeAtt)
head(EmployeeAtt)
### delete useless columns 
EmployeeAtt$EmployeeCount <- NULL
EmployeeAtt$Over18 <- NULL
EmployeeAtt$EmployeeNumber <- NULL
EmployeeAtt$StandardHours <- NULL
head(EmployeeAtt)
```
We looked at the independetn relationships betweeen attrition and separate attributes such as YearsAtIBM,
OverTime and MonthlyIncome.
```r
#----------------------------------------------------------------------------------------- Exploratory Data Analysis
ggplot(data = EmployeeAtt, aes(x=Age,y=MonthlyIncome,col=JobSatisfaction))+
  geom_point(alpha=0.5)
ggplot(data = EmployeeAtt, aes(x=Age,y=MonthlyIncome,col=Attrition))+
  geom_point(alpha=0.5,size=1)  
g1 <- ggplot(data = EmployeeAtt,aes(x=MonthlyIncome,fill=Attrition))+
  geom_density(alpha=0.6)
g2 <- ggplot(data = EmployeeAtt,aes(x=MonthlyRate,fill=Attrition))+
  geom_density(alpha=0.6)
g3 <- ggplot(data = EmployeeAtt,aes(x=DailyRate,fill=Attrition))+
  geom_density(alpha=0.6)
g4 <- ggplot(data = EmployeeAtt,aes(x=HourlyRate,fill=Attrition))+
  geom_density(alpha=0.6)
grid.arrange(g1, g2, g3, g4, ncol = 2, nrow = 2)
```
![](https://user-images.githubusercontent.com/37298254/51882358-a2f59180-234c-11e9-9490-493267b7c1c4.jpeg)
![](https://user-images.githubusercontent.com/37298254/51882362-a426be80-234c-11e9-99f3-ec7f52060962.jpeg)
![](https://user-images.githubusercontent.com/37298254/51882367-a5f08200-234c-11e9-8e6b-ee5dd8354ab1.jpeg)

Attrition increases as monthly income decreases.

```r
ggplot(EmployeeAtt,aes(x=Attrition,fill=OverTime))+
  geom_bar()
ggplot(EmployeeAtt,aes(x=Attrition,fill=as.factor(WorkLifeBalance)))+
  geom_bar(position='fill')
ggplot(EmployeeAtt,aes(x=YearsAtCompany,y=YearsSinceLastPromotion,col=OverTime))+
  geom_jitter(alpha=0.5)+
  geom_smooth(method = 'gam')+
  facet_wrap(~ Attrition)+ 
  ggtitle('Attrition')+
  theme(plot.title = element_text(hjust = 0.5))
  ```
![](https://user-images.githubusercontent.com/37298254/51882822-45fadb00-234e-11e9-87cf-c7fcc958b966.png)
![](https://user-images.githubusercontent.com/37298254/51882823-472c0800-234e-11e9-9ad9-c48135535540.png)
![](https://user-images.githubusercontent.com/37298254/51882825-4a26f880-234e-11e9-9889-dc76a5c74f0a.png)

People are not growing healthily in the organization. Those who have attrition and work overtime seem to be less likely to be promoted. Work overtime is an important factor leading to attrition. 

```r
ggplot(EmployeeAtt, aes(x=WorkLifeBalance, y=DistanceFromHome, 
                        group= WorkLifeBalance, fill=WorkLifeBalance)) +
  geom_boxplot(alpha=0.7)+
  theme(legend.position='none') +
  facet_wrap(~Attrition) +
  ggtitle('Attrition') +
  theme(plot.title = element_text(hjust = 0.5))  
 ```
 
![](https://user-images.githubusercontent.com/37298254/51882830-50b57000-234e-11e9-9e30-9bfcb9e753bb.png)

Those who rated their work-life balance relatively low were commuting a bit farther away in comparison with those who rated their work-life balance as very good.

------------------------------------------------------------------------

Part Two Unsupervised Learning
-----------------------------------
  
We ran out k-means clustering on all 23 numeric variables(scaled). The clusters are selected via Information Criterion where 
BIC selected 22 clusters and AIC selected 48. As both BIC and AIC selected a large number of clusters we decided to explore cluters comprised of fewer variables.

The below plots show four clusters: the red cluster contains employees with a low income and a short tenure at the company. 
The green cluster represents emplyees with a high income and short tenure. Dark blue are employees with a low income and a long tenure. Finally, light blue indicates employees with a high income and a long tenure. The size of the circle indicates the attrition rate. As intuition would tell us, employees with a lower income are more likely to leave the company.

```r
#--------------------------------- Unsupervised Learning - Clustering 
source("DataAnalyticsFunctions.R")
x <- EmployeeAtt
library("dplyr")
x_num <- select_if(x, is.numeric)
names(x_num)
summary(x_num)
x.scaled <- scale(x_num)

#choose number of clusters based on the fit above
kfit <- lapply(1:50, function(k) kmeans(x.scaled,k))
#Then "A" for AICc (default) or "B" for BIC or 'C' for HDIC
kaicc <- sapply(kfit,kIC)
kbic  <- sapply(kfit,kIC,"B")
##Now we plot them, first we plot AIC
par(mar=c(1,1,1,1))
par(mai=c(1,1,1,1))
plot(kaicc, xlab="K", ylab="IC", 
     ylim=range(c(kaicc,kbic)), # get them on same page
     type="l", lwd=2)
#Vertical line where AIC is minimized
abline(v=which.min(kaicc))
#Next we plot BIC
lines(kbic, col=4, lwd=2)
#Vertical line where BIC is minimized
abline(v=which.min(kbic),col=4)
#Insert labels
text(c(50,20),c(22000,26000),c("AIC","BIC"))
#both AIC and BIC choose very complicated models: AIC chooses 50 clusters, BIC chooses 19 clusters
#we use BIC
nineteen.clusters <- kmeans(x.scaled, 19, nstart = 10)
nineteen.clusters$centers
nineteen.clusters$size
#We can get the attrition rate for each cluster 
par(mar=c(5.1, 4.1, 4.1, 8.1), xpd=TRUE)
plot(tapply(as.numeric(EmployeeAtt$Attrition), nineteen.clusters$cluster, mean),ylab=NA, col='red')
mtext(side = 2, line = 3,'Attrition')
par(new=TRUE)
plot(tapply(EmployeeAtt$JobInvolvement, nineteen.clusters$cluster,mean),axes=F,xlab=NA,ylab=NA, col='green')
axis(side = 4)
mtext(side = 4, line = 3,'JobInvolvement')
#legend("topright",inset=c(0.5,0.5),legend=c("Attrition", 'JobInvolvement'), pch=c(1,1),col=c("red", "green"))
#axis(1,1:20)
#Summarize some variables on the clusters
lines(tapply(EmployeeAtt$JobInvolvement, nineteen.clusters$cluster,mean))
#find that people with lower involvment generally fall in cluster with higher attrition 
#########################have too many clusters and the size are relatively small, explore less variables
#create one column with 1 & 0 for attrition
EmployeeAtt$att <- ifelse(EmployeeAtt$Attrition == 'Yes', 1, 0)
sum(EmployeeAtt$att)/(nrow(EmployeeAtt)-sum(EmployeeAtt$att))
#####yes/no = 0.19 or 1:5
#Relationship satisfaction and monthly incom
relation <- EmployeeAtt[ ,c(23,17)]
rrelation <- scale(EmployeeAtt[ ,c(23,28)])
plot(relation, col = 4, xlab="Relationship Satisfaction Level", ylab="Monthly income")
plot(relation, col = 3-EmployeeAtt$att, xlab="Relationship Satisfaction Level", ylab="Monthly income")
##montly income seems more correlated with attrition 
#work life balance and monthly income
relation <- EmployeeAtt[ ,c(27,17)]
rrelation <- scale(EmployeeAtt[ ,c(23,28)])
plot(relation, col = 4, xlab="Work Life Balance", ylab="Monthly income")
plot(relation, col = 3-EmployeeAtt$att, xlab="Work Life Balance", ylab="Monthly income")
#attach(EmployeeAtt)
#identify(WorkLifeBalance,MonthlyIncome,JobInvolvement)
####interesting finding with relatively good work life balance(3) and high salary, 
###still a portion of people want to leave
###years at company
yac <- EmployeeAtt[ ,c(28, 17)]
#######
points(Ssimple_kmeans$centers, col = 1, pch = 24, cex = 1.5, lwd=1, bg = 2:5)
plot(yac, col = 4, xlab="Years at company", ylab="Monthly income")
plot(yac, col = 3-EmployeeAtt$att, xlab="Years at company", ylab="Monthly income")
#######
yyac <- scale(EmployeeAtt[ ,c(28, 17)])
yyac_kmeans <- kmeans(yyac,4,nstart=10)
colorcluster <- 1+yyac_kmeans$cluster
plot(yyac, col = 1, xlab="years at company", ylab="monthly income")
plot(yyac, col = colorcluster, xlab="years at company", ylab="monthly income")
points(yyac_kmeans$centers, col = 1, pch = 24, cex = 1.5, lwd=1, bg = 2:5)
####Cluster in the original units
colorcluster <- 1+yyac_kmeans$cluster
plot(yac, xlim=c(0,80), xlab="years at company", ylab="monthly income", col = colorcluster)
###This plotted in the original space
plot(yac, xlim=c(0,80), xlab="years at company", ylab="monthly income", col = colorcluster, main="Circles indicates installment rate (%)")
radius <- 2*sqrt(EmployeeAtt[,32]) ## installment rate in %
symbols(yac, circles=radius ,  xlim=c(0,80), xlab="years at company", ylab="monthly income", inches = FALSE, bg = colorcluster)

###The command
yyac_kmeans$centers
##displays the k centers (good to interpret)
yyac_kmeans$size
##displays the size of each cluster
###Summarize a variable on each cluster
tapply(EmployeeAtt[,32],yyac_kmeans$cluster,mean)
#attach(EmployeeAtt)
#identify(YearsAtCompany, MonthlyIncome, JobInvolvement)
```

![](https://user-images.githubusercontent.com/37298254/51883886-f10d9380-2352-11e9-8707-a6bcfd7f2a53.png)
  
------------------------------------------------------------------------

# Part Three Modeling
-----------------------------------

## Logistic Regression 
For logstic regression we initially inducted the model using all 30 variables. We next reduced the model by running backwards stepwise selection and then inducting a second model that only included the statistically significant variables from the backwards stepwise selection. In interpretation of our results we must be aware that we are looking at the conditional probability of an employee leaving given all other variables. The regression also produces the coefficients in log odds due to the structure of logistic regression modeling. The advantage to using the logistic model is in obtaining the weghted combination of our attributes and thus allowing us to determine the relative importnace of the different variables on the probability of retention.

```r
#Splitting data into Test and Train datasets suing sample function
data(EmployeeAtt)

## 75% of the sample size
smp_size <- floor(0.75 * nrow(EmployeeAtt))

## set the seed to make your partition reproducible
set.seed(123)
train_ind <- sample(seq_len(nrow(EmployeeAtt)), size = smp_size)

train <- EmployeeAtt[train_ind, ]
test <- EmployeeAtt[-train_ind, ]
train
#Model Selection
#FullModel, including every variables
fullmod = glm(Attrition ~. ,data=train,family=binomial)
summary(fullmod)
summary(fullmod)$coefficients[,4][summary(fullmod)$coefficients[,4]< 0.01] #p-values of this model

#NothingModel includes no variables, like random guessing
nothing <- glm(Attrition ~ 1,data=train,family=binomial)
summary(nothing)


#Backward Stepwise selection for Model fullmod
backwards = step(fullmod)
summary(backwards)

#Model Back2, all variables are significant, best model to predict from Train dataset
back2 = glm(Attrition ~ BusinessTravel + DistanceFromHome +  EnvironmentSatisfaction + Gender+ JobInvolvement + JobRole + JobSatisfaction+MaritalStatus+NumCompaniesWorked+OverTime +RelationshipSatisfaction+TotalWorkingYears+YearsAtCompany+YearsInCurrentRole+YearsSinceLastPromotion+YearsWithCurrManager,data=train,family=binomial)
summary(back2)

back2 = glm(Attrition ~ BusinessTravel + DistanceFromHome +  EnvironmentSatisfaction + Gender+ JobInvolvement + JobRole + JobSatisfaction+MaritalStatus+NumCompaniesWorked+OverTime +RelationshipSatisfaction+TotalWorkingYears+YearsAtCompany+YearsInCurrentRole+YearsSinceLastPromotion+YearsWithCurrManager,data=test,family=binomial)
summary(back2)
### Logistic Regression
threshold <- .6
TP <- sum((back2$fitted >= threshold)*back2$y)
FP <- sum((back2$fitted >= threshold)*(!back2$y))
FN <- sum((back2$fitted <  threshold)*back2$y)
TN <- sum((back2$fitted <  threshold)*(!back2$y))
TP
FP
FN
TN
(TP+TN)/(TP+FP+FN+TN)
LR.FPR <- FP / (FP + TN)
LR.TPR <- TP / (TP + FN)
points( c( LR.FPR ), c(LR.TPR))
text( c( LR.FPR ), c(LR.TPR+.05), labels=c("LR with .3"))

#finding the perfect threshold 
x <- c(0,0.05,0.1,0.15,0.2,0.25,0.3,0.35,0.4,0.45,0.55,0.6,0.65,0.7,0.75,0.8,0.85,0.9,0.95,1)
count <- 1
TPList <- list()
FPList <- list()
FNList <- list()
TNList <- list()

for (threshold in x) {
  a <- sum((back2$fitted >= threshold)*back2$y)
  TPList <- append(TPList, a)
  
  b <- sum((back2$fitted >= threshold)*(!back2$y))
  FPList <- append(FPList, b)
  c <- sum((back2$fitted <  threshold)*back2$y)
  FNList <- append(FNList, c)
  d <- sum((back2$fitted <  threshold)*(!back2$y))
  TNList <- append(TNList, d)
  
  LR.FPR <- FP / (FP + TN)
  LR.TPR <- TP / (TP + FN)
  points( c( LR.FPR ), c(LR.TPR))  
  count = count+1
}
print(count)
TPList
FPList
FNList
TNList

accuracyLi<- list()

for (i in 1:20) {
  Liu= (as.numeric(TPList[i])+as.numeric(TNList[i])) / (as.numeric(TPList[i]) +as.numeric(FPList[i]) +as.numeric(FNList[i]) +as.numeric(TNList[i]))
  accuracyLi <- append(accuracyLi, Liu)
}

accuracyLi
plot(x,accuracyLi,main = "Accuracry cutoffs for different Threshold",xlab = "Thresholds")

#K-Fold Cross Validation for LR
library(caret)
ctrl <- trainControl(method = "repeatedcv", number = 10, savePredictions = TRUE)

mod_fit <- train(Attrition ~ BusinessTravel + DistanceFromHome +  EnvironmentSatisfaction + Gender+ JobInvolvement + JobRole + JobSatisfaction+MaritalStatus+NumCompaniesWorked+OverTime +RelationshipSatisfaction+TotalWorkingYears+YearsAtCompany+YearsInCurrentRole+YearsSinceLastPromotion+YearsWithCurrManager
                 ,data=train, method="glm", family="binomial",
                 trControl = ctrl, tuneLength = 5)

pred = predict(mod_fit, newdata=test)
confusionMatrix(data=pred, test$Attrition)


```
## Support Vector Machine
For our initial model we induced a support vector machine with all 30 variables. We then induced a model on a reduced subset of the most significnat variables from our reduced logistic regression model. The reason we choose to use a SVM is the way in which the model allows a tolerance for mis-classification by finding the optimal balance between the margin separating the data and the hinge loss.

```r
#SVM model
library(e1071)
library(MASS)
fullSvm <-svm(Attrition ~., data=train,type="C-classification", kernel="linear")
summary(fullSvm)
svm_result <-svm(Attrition ~ BusinessTravel + DistanceFromHome +  EnvironmentSatisfaction + Gender+ JobInvolvement + JobRole + JobSatisfaction+MaritalStatus+NumCompaniesWorked+OverTime +RelationshipSatisfaction+TotalWorkingYears+YearsAtCompany+YearsInCurrentRole+YearsSinceLastPromotion+YearsWithCurrManager, data=train,type="C-classification", kernel="linear")
summary(svm_result)

svm_result$rho
head(test)
TP <- sum((predict(svm_result,test) == "Yes" )*(test$Attrition=="Yes"))
FP <- sum((predict(svm_result,test) == "Yes" )*(test$Attrition=="No"))
FN <- sum((predict(svm_result,test) == "No" )*(test$Attrition=="Yes"))
TN <- sum((predict(svm_result,test) == "No" )*(test$Attrition=="No"))
TP
FP
FN
TN
accuraySvm=(TP+TN) / (TP+FP+FN+TN)
accuraySvm
SVM.FPR <- FP / (FP + TN)
SVM.TPR <- TP / (TP + FN)

plot( c( 0, 1 ), c(0, 1), type="n", xlim=c(0,1), ylim=c(0,1), bty="n", xlab = "False positive rate", ylab="True positive rate")
lines(c(0,1),c(0,1), lty=2)
points( c( LR.FPR,Tr.FPR,SVM.FPR ), c(LR.TPR,Tr.TPR,SVM.TPR))
text( c( LR.FPR+0.04,Tr.FPR,SVM.FPR ), c(LR.TPR-.01,Tr.TPR-.05,SVM.TPR+.05), labels=c("LR","Tree","SVM"))

```

## Classification Tree
Our next supervised model was a classification tree. We retained the same variables as the reduced logistic model. The classification tree allows us to see the results generated from a relatively straightforward piecewise classification. The advantage from using this model in combination with a linear classifier model such as logistic regression or SVM is that we are unaware of the "shape" of our data and annot be certain if segmenting the data recursively with a tree is more accurate than creating a single decision surface through our entire data space. A tree is also much easier to visualize and explain to a non technical audience.

```r
#ClassificationTree
installpkg("tree")
library(tree)
installpkg("partykit")
library(partykit)

AttritionTree <- tree(Attrition ~ BusinessTravel + DistanceFromHome +  EnvironmentSatisfaction + Gender+ JobInvolvement + JobRole + JobSatisfaction+MaritalStatus+NumCompaniesWorked+OverTime +RelationshipSatisfaction+TotalWorkingYears+YearsAtCompany+YearsInCurrentRole+YearsSinceLastPromotion+YearsWithCurrManager, data=train) 
summary(AttritionTree)
plot(AttritionTree)
text(AttritionTree, label="yval")
plot(AttritionTree)
text(AttritionTree,label="yprob")
AttritionTree[[1]]$yprob[,2]
## Tree
TP <- sum((predict(AttritionTree,test,type="class") == "Yes" )*(test$Attrition=="Yes"))
FP <- sum((predict(AttritionTree,test,type="class") == "Yes" )*(test$Attrition=="No"))
FN <- sum((predict(AttritionTree,test,type="class") == "No" )*(test$Attrition=="Yes"))
TN <- sum((predict(AttritionTree,test,type="class") == "No" )*(test$Attrition=="No"))
TP
FP
FN
TN
accurayTree=(TP+TN) / (TP+FP+FN+TN)
accurayTree
Tr.FPR <- FP / (FP + TN)
Tr.TPR <- TP / (TP + FN)
```

## Logistic Regression with Interaction 
Next we decided to look at a logistic model including interaction variables. We inducted the initial model by including all possible interactions from the variables used in the reduced logistic model and the pruned the results by running a backwards stepwise selection on all of the significant variables. This model allows for the greatest model complexity by intrducing nonlinear features.

```r
#Interaction
library(glmnet)
`%ni%`<-Negate('%in%')
data(train)
x<-model.matrix(Attrition~.^2,data=train)
x=x[,-1]
x
typeof(train$Attrition)
glmnet1<-cv.glmnet(x=x,y=train$Attrition,type.measure='mse',nfolds=5,alpha=.5,family="binomial")
c<-coef(glmnet1,s='lambda.min',exact=TRUE)
inds<-which(c!=0)
variables<-row.names(c)[inds]
variables<-variables[variables %ni% '(Intercept)']
variables          

a<-glm(Attrition ~ X...Age*DailyRate + X...Age*JobSatisfaction + X...Age*TrainingTimesLastYear+ BusinessTravel*DistanceFromHome + BusinessTravel*EducationField + BusinessTravel*HourlyRate + BusinessTravel*JobInvolvement+ BusinessTravel*JobRole + BusinessTravel*MaritalStatus +BusinessTravel*OverTime +BusinessTravel*YearsSinceLastPromotion + DailyRate*JobInvolvement + DailyRate*JobRole + DailyRate*RelationshipSatisfaction + DailyRate*TotalWorkingYears+DailyRate*YearsInCurrentRole + DailyRate*YearsWithCurrManager + Department*JobInvolvement + Department*MaritalStatus +Department*MonthlyRate + Department*OverTime + Department*YearsInCurrentRole + DistanceFromHome*EducationField + DistanceFromHome*JobRole+ DistanceFromHome*MaritalStatus+ DistanceFromHome*OverTime +Education*JobSatisfaction + Education*YearsSinceLastPromotion +EducationField*EnvironmentSatisfaction       +EducationField*Gender +EducationField*JobInvolvement + EducationField*JobRole + EducationField*JobSatisfaction + EducationField*MaritalStatus + EducationField*OverTime+ EducationField*PerformanceRating +EnvironmentSatisfaction*JobLevel +EnvironmentSatisfaction*JobSatisfaction +EnvironmentSatisfaction*WorkLifeBalance +JobInvolvement*OverTime + JobInvolvement*TotalWorkingYears+JobLevel*TrainingTimesLastYear +JobRole*MaritalStatus +JobRole*OverTime +YearsAtCompany*YearsSinceLastPromotion + TrainingTimesLastYear*YearsInCurrentRole, data=train,family="binomial")
summary(a)
backwards1 = step(a)
summary(backwards1)
interactionModel<-glm(Attrition ~BusinessTravel + DistanceFromHome +  EnvironmentSatisfaction + Gender+ JobInvolvement + JobRole + JobSatisfaction+MaritalStatus+NumCompaniesWorked+OverTime +RelationshipSatisfaction+TotalWorkingYears+YearsAtCompany+YearsInCurrentRole+YearsSinceLastPromotion+YearsWithCurrManager+DailyRate:TotalWorkingYears+DistanceFromHome:JobRole+JobSatisfaction:Education + YearsSinceLastPromotion:Education  +EnvironmentSatisfaction:WorkLifeBalance +JobInvolvement:TotalWorkingYears +JobRole:OverTime+YearsSinceLastPromotion:YearsAtCompany, data=train,family="binomial")
summary(interactionModel)
  


#For Interac accuracy
threshold <- .6
interMod.TP <- sum((interactionModel$fitted >= threshold)*interactionModel$y)
interMod.FP <- sum((interactionModel$fitted >= threshold)*(!interactionModel$y))
interMod.FN <- sum((interactionModel$fitted <  threshold)*interactionModel$y)
interMod.TN <- sum((interactionModel$fitted <  threshold)*(!interactionModel$y))

interMod.FPR <- interMod.FP / (interMod.FP + interMod.TN)
interMod.TPR <- interMod.TP / (interMod.TP + interMod.FN)
```
------------------------------------------------------------------------

# Part Four Evaluation
-----------------------------------

In testing a range of cutoff calues from 0 to 1 in .05 increments using the OOS dataset we found that our logistic regression model achieved the highest accuracy witha cutoff value of .60. Intuitively this cutoff makes sense in a free-market economy where base employeee turnover is expected to be relatively high. Thus considering employees that are definitely more likely to leave than not as the group at risk will aovid wasting resources on higher compensation for retining employees that actually display a mornal (economy-wide) likelihood of leaving. We use the .60 threshold to evaluate ourlogistic regression model with and without interactions.

## Support Vector Machine
Sensitivity: .3226 Specificity:.9706 Accuracy:.8614

## Classifcation Tree
Sensitivity:.2419 Specificity:.9542 Accuracy:.8342

## Logistic Regression 
Sensitivity:.9542 Specificity:.4355 Accuracy:.8688

## Logistic Regression with Interaction
Sensitivity:.9314 Specificity:.4032 Accuracy:.8424

Therefore we select the Logistic Model without interactions for the purpose of determining attribute importance/relevance.

------------------------------------------------------------------------

# Part Five Deployment
-----------------------------------

* The main reason behind attrition is the reward-effort imbalance. From the exploratory analysis, monthly income and work overtime are two important factors that affect the attrition rate. Therefore, it is suggested that the company investigate about if the employees who work overtime or travel get proper compensation.  

* In the predictive model, one possible business implementation is that to make a dashboard, which utilizes the data generated, to update the possibility of attrition on a daily / monthly basis for every employee. As an indicator, the human resources department can make adjustment to improve the status. 
The dashboard can also be used to find the most useful plan to improve the job satisfaction as the result of each plan is measured quantitatively on a regular time basis.

* Considering the cost and benefit, the company could also only perform the prediction on existing high performance individuals. After evaluating the probability of these employees leaving the company and the cost of retaining theses employees, the company can maximize their profit by targeting the most profitable employees to make them stay at the company.

* But there are also some issues to think about when implementing the plan suggested above. For example, it is difficult to measure the profit of each high performance individual. And the company needs to think about approach to inform employees about the attrition rate analysis it conducted for employees.


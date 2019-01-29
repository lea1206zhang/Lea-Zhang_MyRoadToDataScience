Lea Zhang
2019-01-28

IBM HR Analytics
======================

------------------------------------------------------------------------

Abstract:
---------
Through the use of data science analysis techniques we where able to form a picture of 
what factors are important in determining employee attrition. Using these results IBM HR will
better understand what employees find important for job satisfaction. We provide a recommendation 
on how to implement a targeted plan to retain more employees at risk of leaving. These results 
can also be used in making future hiring decisions by preemptively avoiding groups of people with 
a higher risk of leaving. 

------------------------------------------------------------------------

Part One Data Understanding
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
### attrition increases as monthly income decreases
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
  ![](https://user-images.githubusercontent.com/37298254/51881874-d9321180-234a-11e9-8793-5e4e81c630c0.png)
  
  

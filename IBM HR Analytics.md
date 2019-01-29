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

Part One Explotary Data Analysis
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

Part two Unsupervised Learning
-----------------------------------
  
We ran out k-means clustering on all 23 numeric variables(scaled). The clusters are selected via Information Criterion where 
BIC selected 22 clusters and AIC selected 48. As both BIC and AIC selected a large number of clusters we decided to explore cluters comprised of fewer variables.

The below plots show four clusters: the red cluster contains employees with a low income and a short tenure at the company. 
The green cluster represents emplyees with a high income and short tenure. Dark blue are employees with a low income and a long tenure. Finally, light blue indicates employees with a high income and a long tenure. The size of the circle indicates the attrition rate. As intuition would tell us, employees with a lower income are more likely to leave the company.

![]()
  

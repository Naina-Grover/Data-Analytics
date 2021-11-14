#Group Assignment


######DATA PREPARATION######
library(readr)
library(tidyverse)
library(leaps)
library(ggplot2)
library(cowplot)
library(tree)

#read and view target data set
StudentPerfomance <- read_csv("C:/Users/Administrator/×ÀÃæ/DA inclass exercise files/StudentPerfomance.csv")
View(StudentPerfomance)

#check the dimensions of Default (how many rows, how many columns)
dim(StudentPerfomance)

#check the names of the columns in Default
names(StudentPerfomance)

#for less confusion, rename columns as common R format
colnames(StudentPerfomance)<-c("gender","race","parental_education","lunch","preparation","math_score","reading_score","writing_score","total_score")

#list all columns class
sapply(StudentPerfomance,class)

#covert character variables into factor ones
as.factor(StudentPerfomance$gender)
as.factor(StudentPerfomance$race)
as.factor(StudentPerfomance$parental_education)
as.factor(StudentPerfomance$lunch)
as.factor(StudentPerfomance$preparation)

#check how many missing values there are in the data frame Default
na_check<-is.na(StudentPerfomance)
sum(na_check)    #ANSWER:There is no "na" in the data set of StudentPerfomance

#convert categorical variables into numerical ones by dummy variables
Gender=ifelse(gender=="male",1,0)
Group_A=ifelse(race=="group A",1,0)
Group_B=ifelse(race=="group B",1,0)
Group_C=ifelse(race=="group C",1,0)
Group_D=ifelse(race=="group D",1,0)
Group_E=ifelse(race=="group E",1,0)
Associate_degree=ifelse(parental_education=="associate's degree",1,0)
Bachelor_degree=ifelse(parental_education=="bachelor's degree",1,0)
High_school=ifelse(parental_education=="high school",1,0)
Master_degree=ifelse(parental_education=="master's degree",1,0)
Some_college=ifelse(parental_education=="some college",1,0)
Some_high_school=ifelse(parental_education=="some high school",1,0)
Lunch=ifelse(lunch=="standard",1,0)
Preparation=ifelse(preparation=="completed",1,0)

#covert response variables into dummy variables
summary(StudentPerfomance)  #ANSWER:take the median value as classification standard
Math_score=ifelse(math_score>66,1,0)
Reading_score=ifelse(reading_score>70,1,0)
Writing_score=ifelse(writing_score>69,1,0)
Total_score=ifelse(total_score>205,1,0)

#build new data set with binary variables
SP<-data.frame(Gender,Group_A,Group_B,Group_C,Group_D,Group_E,
               Associate_degree,Bachelor_degree,High_school,Master_degree,Some_college,Some_high_school,
               Lunch,Preparation,Math_score,Reading_score,Writing_score,Total_score)
View(SP)



######CLASSIFICATION######

#run logistic regression with default as the response variable and all others as predictors
logreg.fit_T<-glm(Total_score~.-Math_score-Reading_score-Writing_score,data=SP,family=binomial)

logreg.fit_M<-glm(Math_score~.-Total_score-Reading_score-Writing_score,data=SP,family=binomial)
logreg.fit_R<-glm(Reading_score~.-Math_score-Total_score-Writing_score,data=SP,family=binomial)
logreg.fit_W<-glm(Writing_score~.-Math_score-Reading_score-Total_score,data=SP,family=binomial)
#without "+0", there would be warnings on some variables because they are dependent variables, we fix this problem by add "+0", 
    #after which, all coefficients show up except the intercept parameter.

#see the summary output of logistic regression
Logreg_summary_T<-summary(logreg.fit_T)

Logreg_summary_M<-summary(logreg.fit_M)
Logreg_summary_R<-summary(logreg.fit_R)
Logreg_summary_W<-summary(logreg.fit_W)

summary(logreg.fit_T)

#SUBSET SELECTION
reg_T<-regsubsets(Total_score~.-Math_score-Reading_score-Writing_score,data=SP,nvmax=14)

reg_M<-regsubsets(Math_score~.-Total_score-Reading_score-Writing_score,data=SP,nvmax=14)
reg_R<-regsubsets(Reading_score~.-Math_score-Total_score-Writing_score,data=SP,nvmax=14)
reg_W<-regsubsets(Writing_score~.-Math_score-Reading_score-Total_score,data=SP,nvmax=14)

####Problems: because of the dummy variables are in the same group before splitting, 
         ###R doesn't show the last variable which is dependent 

#see the summary output of model selection
reg_summary_T<-summary(reg_T)

reg_summary_M<-summary(reg_M)
reg_summary_R<-summary(reg_R)
reg_summary_W<-summary(reg_W)

#use different approaches: R^2, adjusted R^2, CP, BIC
#for Total_score
which.max(reg_summary_T$adjr2)
plot(reg_summary_T$adjr2,xlab="Number of Variables") #ANSWER:from the plot, we can see when number of variables is 10, it has the maximum value of adjr2
coef(reg_summary_T,10)  
which.min(reg_summary_T$cp)
plot(reg_summary_T$cp,xlab="Number of Variables") #ANSWER:from the plot, we can see when number of variables is 9, it has the minimum value of cp
coef(reg_summary_T,9)   
which.min(reg_summary_T$bic)
plot(reg_summary_T$bic,xlab="Number of Variables") #ANSWER:from the plot, we can see when number of variables is 7, it has the minimum value of bic
coef(reg_summary_T,7) 

#for Math_score
which.max(reg_summary_M$adjr2)
plot(reg_summary_M$adjr2,xlab="Number of Variables") #ANSWER:from the plot, we can see when number of variables is 10, it has the maximum value of adjr2
coef(reg_summary_M,10) 
which.min(reg_summary_M$cp)
plot(reg_summary_M$cp,xlab="Number of Variables") #ANSWER:from the plot, we can see when number of variables is 9, it has the minimum value of cp
coef(reg_summary_M,8) 
which.min(reg_summary_M$bic)
plot(reg_summary_M$bic,xlab="Number of Variables") #ANSWER:from the plot, we can see when number of variables is 7, it has the minimum value of bic
coef(reg_summary_M,5)   

#for Reading_score
which.max(reg_summary_R$adjr2)
plot(reg_summary_R$adjr2,xlab="Number of Variables") #ANSWER:from the plot, we can see when number of variables is 10, it has the maximum value of adjr2
coef(reg_summary_R,10)  # there will be NULL because the dependent variables
which.min(reg_summary_R$cp)
plot(reg_summary_R$cp,xlab="Number of Variables") #ANSWER:from the plot, we can see when number of variables is 9, it has the minimum value of cp
coef(reg_summary_R,9)  
which.min(reg_summary_R$bic)
plot(reg_summary_R$bic,xlab="Number of Variables") #ANSWER:from the plot, we can see when number of variables is 7, it has the minimum value of bic
coef(reg_summary_R,7)   

#for Writing_score
which.max(reg_summary_W$adjr2)
plot(reg_summary_W$adjr2,xlab="Number of Variables") #ANSWER:from the plot, we can see when number of variables is 10, it has the maximum value of adjr2
coef(reg_summary_W,10)  
which.min(reg_summary_W$cp)
plot(reg_summary_W$cp,xlab="Number of Variables") #ANSWER:from the plot, we can see when number of variables is 9, it has the minimum value of cp
coef(reg_summary_W,9) 
which.min(reg_summary_W$bic)
plot(reg_summary_W$bic,xlab="Number of Variables") #ANSWER:from the plot, we can see when number of variables is 7, it has the minimum value of bic
coef(reg_summary_W,7)   



#######DECISION TREE######## 
ggplot(StudentPerfomance,aes(x=Math_score))+geom_histogram(fill="blue")
summary(StudentPerfomance$Math_score)

## set cut point and split the data
StudentPerfomance$good_at_math<-ifelse(StudentPerfomance$Math_score>75,"Yes","No")
StudentPerfomance$good_at_math<-as.factor(StudentPerfomance$good_at_math)
summary(StudentPerfomance$good_at_math)
set.seed(2)
train_row<-sample(nrow(StudentPerfomance),nrow(StudentPerfomance)*0.8)
SP_train<-StudentPerfomance[train_row,]
View(SP_train)
SP_test<-StudentPerfomance[-train_row,]

## Modeling
tree_SP<-tree(good_at_math~gender+race+parental_education+lunch+preparation,data=SP_train)
summary(tree_SP)
plot(tree_SP)
text(tree_SP,pretty = 0,cex=0.6)

## Prune
CV<-cv.tree(tree_SP)
summary(CV)
plot(CV$size,CV$dev)

## Prediction
SP_test$tr_prob<-predict(tree_SP,newdata = SP_test)[,2]
SP_test$tr_result<-ifelse(SP_test$tr_prob>0.5,"Yes","No")
# creating confusion matrix
table(SP_test$tr_result,SP_test$good_at_math)
(139+5)/200


































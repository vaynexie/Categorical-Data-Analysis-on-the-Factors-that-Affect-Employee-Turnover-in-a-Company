turnover <- read.csv("~/Desktop/turnover.csv")
data_set<-turnover 
summary(data_set)

#1.data cleaning
#there is no null value 

#Renaming the irrelevant Variable name
library(plyr)
data_set<-rename(data_set, c("sales"="role"))
data_set<-rename(data_set, c("time_spend_company"="exp_in_company"))
names(data_set)[10]<-"salary"
head(data_set)

#2.Statistical Overview
dim(data_set)
str(data_set)

# Looks like about 76% of employees stayed and 24% of employees left. 
# NOTE: When performing cross validation, its important to maintain this turnover ratio
attrition<-as.factor(data_set$left)
summary(attrition)
perc_attrition_rate<-sum(data_set$left/length(data_set$left))*100
#percentage of attrition
print(perc_attrition_rate)

#3.Data visization
#a. Satisfaction - Evaluation - AverageMonthlyHours
par(mfrow=c(1,3))
hist(data_set$satisfaction_level, col="green")
hist(data_set$last_evaluation, col="red")
hist(data_set$average_montly_hours, col="blue")
#Satisfaction - There is a huge spike for employees with low satisfaction and high satisfaction.
#Evaluation - There is a bimodal distrubtion of employees for low evaluations (less than 0.6) and high evaluations (more than 0.8)
#AverageMonthlyHours - There is another bimodal distribution of employees with lower and higher average monthly hours (less than 150 hours & more than 250 hours)
#The evaluation and average monthly hour graphs both share a similar distribution.
#Employees with lower average monthly hours were evaluated less and vice versa.

#b. Salary V.S. Turnover
vis_1<-table(data_set$salary,data_set$left)
#print(vis_1)
d_vis_1<-as.data.frame(vis_1)
print(d_vis_1)
library(ggplot2)
p<-ggplot(d_vis_1, aes(x=Var1,y=Freq,fill=Var2)) +
  geom_bar(position="dodge",stat='identity') + coord_flip()

print(p)
#Majority of employees who left either had low or medium salary.
#Barely any employees left with high salary
#Employees with low to average salaries tend to leave the company.

#c. Department V.S. Turnover
vis_2<-table(data_set$role,data_set$left)
d_vis_2<-as.data.frame(vis_2)
d_vis_2<-subset(d_vis_2,Var2==1)
#print(d_vis_2)
library(ggplot2)
d_vis_2$Var1 <- factor(d_vis_2$Var1, levels = d_vis_2$Var1[order(-d_vis_2$Freq)])
p<-ggplot(d_vis_2, aes(x=Var1,y=Freq,fill=Var1)) +
  geom_bar(stat='identity') +theme(axis.text.x = element_text(angle = 90, hjust = 1))
print(p)
#The sales, technical, and support department were the top 3 departments to have employee turnover
#The management department had the smallest amount of turnover

#d. Turnover V.S. ProjectCount
vis_3<-table(data_set$number_project,data_set$left)
d_vis_3<-as.data.frame(vis_3)
#print(d_vis_1)
library(ggplot2)
p<-ggplot(d_vis_3, aes(x=Var1,y=Freq,fill=Var2)) +
  geom_bar(position="dodge",stat='identity') + coord_flip()

print(p)

#More than half of the employees with 2,6, and 7 projects left the company
#Majority of the employees who did not leave the company had 3,4, and 5 projects
#All of the employees with 7 projects left the company
#There is an increase in employee turnover rate as project count increases



#e. Turnover V.S. time spend in company
vis_3<-table(data_set$exp_in_company,data_set$left)
d_vis_3<-as.data.frame(vis_3)
#print(d_vis_1)
library(ggplot2)
p<-ggplot(d_vis_3, aes(x=Var1,y=Freq,fill=Var2)) +
  geom_bar(position="dodge",stat='identity') + coord_flip()

print(p)


#split the data into left_Data and stay_Data
left_data=subset(data_set,left==1)
stay_data=subset(data_set,left==0)

#f. Turnover V.S. AverageMonthlyHours
ggplot() + geom_density(aes(x=average_montly_hours), colour="red", data=left_data) + 
  geom_density(aes(x=average_montly_hours), colour="blue", data=stay_data)

#Another bi-modal distribution for employees that turnovered
#Employees who had less hours of work (~150hours or less) left the company more
#Employees who had too many hours of work (~250 or more) left the company
#Employees who left generally were underworked or overworked.

#g. Turnover V.S. Satisfaction
ggplot() + geom_density(aes(x=satisfaction_level), colour="red", data=left_data) + 
  geom_density(aes(x=satisfaction_level), colour="blue", data=stay_data)
#There is a tri-modal distribution for employees that turnovered
#Employees who had really low satisfaction levels (0.2 or less) left the company more
#Employees who had low satisfaction levels (0.3~0.5) left the company more
#Employees who had really high satisfaction levels (0.7 or more) left the company more


#4. T-test
#One-Sample T-Test (Measuring Satisfaction Level)¶
#A one-sample t-test checks whether a sample mean differs from the population mean. 
#Let's test to see whether the average satisfaction level of employees that 
#had a turnover differs from the entire employee population.

#Hypothesis Testing: 
#Is there significant difference in the means of satisfaction level 
#between employees who had a turnover and the entire employee population?
#Null Hypothesis: (H0: pTS = pES) The null hypothesis would be that 
#there is no difference in satisfaction level between employees who did turnover and the entire employee population.

emp_population_satisfaction <-mean(data_set$satisfaction_level)
left_pop<-subset(data_set,left==1)

emp_turnover_satisfaction <-mean(left_pop$satisfaction_level)

print( c('The mean for the employee population is: ', emp_population_satisfaction) )
print( c('The mean for the employees that had a turnover is: ' ,emp_turnover_satisfaction) )

t.test(left_pop$satisfaction_level,mu=emp_population_satisfaction) # Employee Population satisfaction mean


data_set=read.csv('/Users/amormio/Desktop/turnover.csv')
library(plyr)
data_set<-rename(data_set, c("sales"="role"))
data_set<-rename(data_set, c("time_spend_company"="exp_in_company"))
names(data_set)[10]<-"salary"
head(data_set)

library(arules)

data_set$number_project=discretize(data_set$number_project, method = "frequency", breaks = 3,labels =c("little","middle","many"))
summary(data_set$number_project)
#here we decide to conduct the discretization
data_set$exp_in_company=discretize(data_set$exp_in_company, method = "frequency", breaks = 3,labels =c("short","middle","long"))
summary(data_set$exp_in_company)
head(data_set)
data_set$Work_accident<-as.factor(data_set$Work_accident)
data_set$promotion_last_5years<-as.factor(data_set$promotion_last_5years)
data_set$role=as.factor(data_set$role)
data_set$salary=as.factor(data_set$salary)
data_set$exp_in_company<-as.factor(data_set$exp_in_company)
summary(data_set$exp_in_company)

library(InformationValue)
library(klaR)
model1=glm(left~., family=binomial, data=data_set)
summary(model1)
res=step(model1, list(lower = ~ 1, upper = formula(model1)), scale = 1, trace = T, 
         direction = "backward")
factor_vars=c("number_project","exp_in_company","Work_accident","promotion_last_5years","role","salary")
all_iv<-data.frame(VARS=factor_vars,IV=numeric(length(factor_vars)),
                  STRENGTH=character(length(factor_vars)),stringsAsFactors = F)

for(factor_var in factor_vars)
  {
all_iv[all_iv$VARS==factor_var,"IV"]<-InformationValue::IV(X=data_set[,factor_var],Y=data_set$left)
all_iv[all_iv$VARS==factor_var,"STRENGTH"]<-attr(InformationValue::IV(X=data_set[,factor_var],Y=data_set$left),"howgood")}
all_iv<-all_iv[order(-all_iv$IV),] 
all_iv##information Value
model2=glm(left~number_project+exp_in_company+satisfaction_level
           +last_evaluation+average_montly_hours, 
           family=binomial, data=data_set) 
summary(model2)
model3=glm(left~number_project+exp_in_company+Work_accident+salary
           +satisfaction_level+last_evaluation+average_montly_hours,
           family=binomial, data=data_set) 
summary(model3)
model4<-glm(left~number_project+exp_in_company+number_project*exp_in_company
            +last_evaluation+satisfaction_level+average_montly_hours, 
            family=binomial, data=data_set)
summary(model4)
p=read.csv('/Users/amormio/Desktop/position.csv')
q=as.matrix(p,'numeric')
trainset=read.csv('/Users/amormio/Desktop/trainset.csv')
test=read.csv('/Users/amormio/Desktop/testset.csv')
#train model
model5<-glm(left~number_project+exp_in_company+number_project*exp_in_company
            +last_evaluation+satisfaction_level+average_montly_hours, 
            family=binomial, data=trainset)
model5
library(gmodels)
library (Hmisc)
library (caTools)
library (ROCR)
library(ggplot2)
tdata=test[,-7]
tdata=test[,-7]
pred1=predict(model5,tdata,type='response')
par(mfrow=c(1,1))
test$prediction1<-ifelse(pred1>=0.5,1,0)
conf_mat1<-table(test$left,test$prediction1)
print(conf_mat1)
#accuracy<-(conf_mat[1,1]+conf_mat[2,2])/(conf_mat[1,1]+conf_mat[2,2]+conf_mat[1,2]+conf_mat[2,1])
#print(c("Accuracy:",accuracy))
precision1<-(conf_mat1[2,2])/(conf_mat1[1,2]+conf_mat1[2,2])
recall_1<-(conf_mat1[2,2])/(conf_mat1[2,1]+conf_mat1[2,2])
print(c("Precision1:",precision1))
print(c("Recall1:",recall_1))
colAUC(test$prediction1,test$left,plotROC=TRUE)

##resampling

library(ROSE)
retrain=read.csv('/Users/amormio/Desktop/retrainset.csv')
retrain$Work_accident<-as.factor(retrain$Work_accident)
retrain$promotion_last_5years<-as.factor(retrain$promotion_last_5years)
retrain$role<-as.factor(retrain$role)
retrain$salary<-as.factor(retrain$salary)
retrain$exp_in_company<-as.factor(retrain$exp_in_company)
#Logistic Regression
model1=glm(left~., family=binomial, data=retrain)
model2=glm(left~number_project+average_montly_hours+exp_in_company+satisfaction_level
           +last_evaluation, family=binomial,data=retrain) 
summary(model2)
model3=glm(left~number_project+average_montly_hours+exp_in_company+Work_accident
           +salary+satisfaction_level+last_evaluation, family=binomial, data=retrain) 


model4=glm(left~number_project+average_montly_hours+exp_in_company+
             number_project*exp_in_company+last_evaluation+satisfaction_level,
           family=binomial, data=retrain) 
summary(model4)
##prediction##
library(gmodels)
library (Hmisc)
library (caTools)
library (ROCR)
library(ggplot2)
tdata=test[,-7]
pred2<-predict(model4,tdata,type='response')
par(mfrow=c(1,1))
test$prediction2<-ifelse(pred2>=0.5,1,0)
conf_mat2<-table(test$left,test$prediction2)
print(conf_mat2)
#accuracy<-(conf_mat2[1,1]+conf_mat2[2,2])/(conf_mat2[1,1]+conf_mat2[2,2]+conf_mat2[1,2]+conf_mat2[2,1])
#print(c("Accuracy:",accuracy))
precision2<-(conf_mat2[2,2])/(conf_mat2[1,2]+conf_mat2[2,2])
recall2<-(conf_mat2[2,2])/(conf_mat2[2,1]+conf_mat2[2,2])
print(c("Precision2:",precision2))
print(c("Recall2:",recall2))
colAUC(test$prediction2,test$left, plotROC=TRUE)

table1<-cbind(data_set$number_project,data_set$exp_in_company,data_set$left)
table1<-as.data.frame(table1)
table<-data.frame(expand.grid(left=factor(c(1,0),levels=c(0,1)), number_project=factor(c("little","middle","many"),levels=c("little","middle","many")),exp_in_company=factor(c("short","middle","long"),levels=c("short", "middle","long"))))
a=as.data.frame(table(comb = do.call(paste, table1)))
count=c(0,303,62,631,20,221,22,1505,1527,2978,623,661,31,1383,887,2915, 339,831 )
table<-data.frame(table,count)

fit.e.n.l<-loglm(count~., data=table,fit=T, param=T)
anova(fit.e.n.l)
fit.en.el.ln<-update(fit.e.n.l, .~.^2, data=table, fit=T, param=T) 
anova(fit.en.el.ln)
fit.enl<-update(fit.e.n.l, .~.^3, data=table, fit=T, param=T) 
anova(fit.enl)
model<-glm(left~number_project+exp_in_company, data=table,family=binomial(),weight=count)                                                                                      
summary(model)                                                                                                                                 

set.seed(1)

logit_model<-glm(left~number_project+exp_in_company+number_project*exp_in_company+last_evaluation+satisfaction_level+average_montly_hours,  family=binomial, data=retrain)
fit.es.el.ls<-glm(left~number_project+exp_in_company, data=table,family=binomial(),weight=count)                                                                                         
tree<- prune(rpart(left ~., data = retrain), cp=0.05)
rf<- randomForest(left ~., data = retrain)

test$loglinear_model<-predict(fit.es.el.ls,test1)
test$logit_model<-predict(logit_model,test)
test$tree<-predict(tree,test)
test$rf<-predict(rf,test)
par(mfrow=c(2,2))
colAUC(test$logit_model,test$left, plotROC=TRUE)
colAUC(test$loglinear_model,test$left, plotROC=TRUE)
colAUC(test$tree,test$left, plotROC=TRUE)
colAUC(test$rf,test$left, plotROC=TRUE)


test$prediction<-ifelse(test$rf>=0.5,1,0)#用ROC确定的阈值,可更改;rf
conf_mat<-table(test$left,test$prediction)
accuracy<-(conf_mat[1,1]+conf_mat[2,2])/(conf_mat[1,1]+conf_mat[2,2]+conf_mat[1,2]+conf_mat[2,1])
recall<-(conf_mat[2,2])/(conf_mat[1,2]+conf_mat[2,2])
precision<-(conf_mat[2,2])/(conf_mat[2,2]+conf_mat[2,1])

test$prediction<-ifelse(test$logit_model>=-.5,1,0)#用ROC确定的阈值,可更改;阈值变大，precision上升，recall下降-看的是0？
conf_mat<-table(test$left,test$prediction)
accuracy<-(conf_mat[1,1]+conf_mat[2,2])/(conf_mat[1,1]+conf_mat[2,2]+conf_mat[1,2]+conf_mat[2,1])
recall<-(conf_mat[2,2])/(conf_mat[1,2]+conf_mat[2,2])
precision<-(conf_mat[2,2])/(conf_mat[2,2]+conf_mat[2,1])

print(c("Accuracy:",accuracy))
print(c("Precision:",precision))
print(c("Recall:",recall))

time1=Sys.time()
rf<- randomForest(left ~., data = retrain)
time2=Sys.time()
difftime(time2,time1)

time1=Sys.time()
logit_model<-glm(left~., family=binomial, data=retrain)
time2=Sys.time()
difftime(time2,time1)
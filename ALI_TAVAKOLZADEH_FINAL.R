# _________________________  ALI TAVAKOLZADEH ___ FINAL EXAM
# 
# I have neither given nor received aid on this examination, nor have I concealed any violation of the Honor Code.

############################################################################ PROBLEM 1
rm(list=ls())
library(readxl)
p1data <- read_excel("Final-Data.xlsx")$R
str(p1data)

### (a)
# first let's have a look at the plot:

qqnorm(p1data)
qqline(p1data)
# based on the plot we can assume it is normally distributed 

# make a decision base on shaping test:
shapiro.test(p1data)
# p-value = 0.3607 it is more than alpha= 0.01 so it is normally distributed 



### (b)
# null H : mu = 0.623
#     H1 : mu >0.623
#Because the variance is not known, we should use the t test.


t.test(p1data,mu=  0.623 , conf.level = 0.99, alternative = c("greater") )
# CI is  [0.6193286   ,    Inf] and as 0.623 is in this interval ans also 
# p-value = 0.2605> alpha , we accept mu=< 0.623 and so we can not accept that true mean  exceeds  0.623


### (C)

#p-value = 0.7395




###(d)
library("pwr")
s = sd(p1data)
mu1 = 0.63
mu0 = 0.623
pwr.t.test(d=(mu1-mu0)/s, n = length(p1data), sig.level = 0.01,  alternative = "greater", type = "one.sample")
# power = 0.8276855


###(e)

pwr.t.test(d=(mu1-mu0)/s,power=0.80 , sig.level = 0.01,  alternative = "greater", type = "one.sample")

#  n = 37.68172 so n= 38 would be enough




############################################################################ PROBLEM 2
rm(list=ls())

### (a)

# Confidence Intervals for Difference in Means of Two Normal Populations with Unknown but Equal Variances
delta0=0; alpha=0.05
n =10
xbar1 = 14.8
s1 =1.4
xbar2 = 15.6
s2 = 1.7
sp=sqrt(((n-1)*s1^2+(n-1)*s2^2)/(n+n-2))

t0=(xbar1-xbar2-delta0)/(sp*sqrt(1/n+1/n))
t0
t.alpha0.5=qt(alpha/2,n+n-2,lower.tail = F)
print(paste("t0=",round(t0,4)," t.alpha0.5=",round(t.alpha0.5,4)))
# "t0= -1.1487  t.alpha0.5= 2.1009" as absolute value of t0 is less than t.alpha0.5 so we accept mean of these 2 are equal

###(b)

ub<-round(xbar1-xbar2+t.alpha0.5* sp*sqrt(1/n+1/n),4)
lb <-round(xbar1-xbar2-t.alpha0.5* sp*sqrt(1/n+1/n),4)
print(paste(1-alpha," CI for mu1-mu2 is ","[",lb,",",ub,"]"))
##  "0.95  CI for mu1-mu2 is  [ -2.2631 , 0.6631 ]" as zero is included we can draw the same conclusion  

###(c)

#Inference on the Difference in Means of Two Normal Populations with Unknown Different Variances so we need nu
t0=(xbar1-xbar2-delta0)/sqrt(s1^2/n +s2^2/n)
t0
# because the n  are the same so t0 is the same as part(a)  but let's check the degree of freedom 
nu=ceiling((s1^2/n +s2^2/n)^2/((s1^2/n)^2/(n-1)+(s2^2/n)^2/(n-1)))
nu
# again nu = 18 which is equal to df for the part (a)
t.alpha0.5=qt(alpha/2,nu,lower.tail = F)
print(paste("t0=",round(t0,4)," t.alpha0.5=",round(t.alpha0.5,4)))

# "t0= -1.1487  t.alpha0.5= 2.1009" as absolute value of t0 is less than t.alpha0.5 so we accept mean if these 2 are equal
# so we got the same result with different  variances 




############################################################################ PROBLEM 3
rm(list=ls())
###(a)

# Multiple Linear Regression
# Read  data
library(readxl)
ep <- read_excel("Final-Data.xlsx",  sheet = "P3")
str(ep)

ep.Reg<-lm(y~.,data=ep)
#  extract MSE or estimate of variance 
str(summary(ep.Reg))
MSE <- (summary(ep.Reg)$sigma)^2
MSE 
# MSE = 138.9234 this is a estimation of variance 

#the coefficients' estimates
summary(ep.Reg)$coefficients[,'Estimate']

# (Intercept)           x1           x2           x3           x4 
# -123.1312463    0.7572891    7.5187840    2.4830786   -0.4811352 



### (b)

summary(ep.Reg)$coefficients[,"Std. Error"]

# (Intercept)          x1          x2          x3          x4 
# 157.2560575   0.2790898   4.0101214   1.8093856   0.5551742 



### (c)
#Based on the regression  summary, F-statistic= 10.08  with 4 and 7 degrees of freedom. Moreover, p-value is  0.00496.
#Therefore, in the significance level of  0.05, we accept that the regression is significant.
#In other words we reject the null hypothesis H0 : that all the beta are equal to zero.
summary(ep.Reg)

### (d)
summary(ep.Reg)$coefficients[,"Pr(>|t|)"]

#Based on the lm summary, p-values for t-test of each regression coefficients are
# (Intercept)          x1          x2          x3          x4 
# 0.45929379  0.03004985  0.10292646  0.21231553  0.41485592 
# As p-value is more than  5 percent for all the variable except x1 so we accept the null  hypothesis ( H0: beta = 0) for all of them except x1
# in other word we accept x2 and x3 and x4 do not have significant regression effect on y because we could not prove their bete are not equal to zero

###(e)
# beta 1
confint(ep.Reg, "x1", level = 0.99)
#           0.5 %      99.5 %
#   x1 -0.2193809    1.733959


# beta 2 
confint(ep.Reg, "x2", level = 0.99)
#         0.5 %    99.5 %
#   x2 -6.514569 21.55214

# beta 3
confint(ep.Reg, "x3", level = 0.99)
#        0.5 %   99.5 %
# x3 -3.848836 8.814993


# beta 4
confint(ep.Reg, "x4", level = 0.99)
#          0.5 %   99.5 %
#   x4 -2.423958 1.461688
# 


###(f)

x_0<-data.frame(x1=75,x2=24,x3=90,x4=98)
predict(ep.Reg,x_0, interval = "confidence", level = 0.90)
#   fit        lwr        upr
# 290.4421   276.0312   304.8529

###(g)

predict(ep.Reg,x_0, interval = "prediction", level = 0.90)

#      fit        lwr        upr
#  290.4421    263.8652   317.0189


### (h)
sprintf("%s = %3.4f","R.squared",summary(ep.Reg)$r.squared)
#  "R.squared = 0.8520"
#From the R output, we have R2 = 0.8520. It means that the linear regression model
# explained  85.2% of total error.



### (i)
plot(ep.Reg$fitted.values,ep.Reg$residuals,xlab="fitted values")
# it seems there is no any relation between residual and fitted value

###(j)
residuals <- ep.Reg$residuals
qqnorm(residuals)
qqline(residuals)
shapiro.test(residuals)
# based on P-value on the test which is 0.2469 and it is more than alpha so we can conclude that the residuals are normally distributed 



############################################################################ PROBLEM 4
rm(list=ls())

###ANOVA
### (a)
p4 <- read_excel("Final-Data.xlsx", sheet = "P4-revised")
str(p4)
p4$C2F6<-factor(p4$C2F6)
str(p4)
summary(p4)
plot(p4$C2F6,p4$Uniformity)

p4.ANOVA<-aov(Uniformity ~ C2F6 ,data=p4)
summary(p4.ANOVA)
# Because p-value=0.0534 >  = 0.05, we can not accept that C2F6 flow rate affect the uniformity.
# and Changing the levels of the factor has no effect on the mean response.


###(b)
bartlett.test(Uniformity ~ C2F6 ,data=p4)
#  p-value = 0.6171 > alpha so homogeneity of variances is ok 

par(mar=c(3,4,2,1))
layout(matrix(c(1,2,3,4),2,2))
plot(p4.ANOVA)

qqnorm(p4.ANOVA$residuals)
qqline(p4.ANOVA$residuals)
shapiro.test(p4.ANOVA$residuals)
#  p-value = 0.6111 residuals are normally distributed 
#residuals do not indicate any problems with the underlying assumptions


### (c)

#  get MSE with the following formula
MSE=summary(p4.ANOVA)[1][[1]][[3]][2]
MSE
# What of degrees of freedom?
df=p4.ANOVA$df.residual
df

#  Fisher s LSD method 

alpha=0.05
n=6
t.alpha0.5=qt(alpha/2,df,lower.tail = F)
LSD=t.alpha0.5*sqrt(2*MSE/n)
LSD
#0.8776696
# Fitted values
ybar.125=mean(p4$Uniformity[p4$C2F6==125])
ybar.160=mean(p4$Uniformity[p4$C2F6==160])
ybar.250=mean(p4$Uniformity[p4$C2F6==250])

# Compare the difference in means with LSD

abs(ybar.125-ybar.160)
#  1.1 > LSD so Mu125 is different than mu 160

abs(ybar.125-ybar.250)
#  0.6166667 < LSD so Mu125 is not  different than mu250


abs(ybar.160-ybar.250)

#   0.4833333 < LSD so Mu160 is not  different than mu250


###(d)

groupmeans=c(3.3,4.4,4)

power.anova.test(groups=length(groupmeans),between.var = var(groupmeans), within.var = 0.5 , sig.level = 0.05,n = 6)
#  power = 0.5894352





############################################################################ PROBLEM 5
rm(list=ls())

#### (a)

library(readr)
Covid <- read_csv("Covid_Data.csv")
#View(Covid)
str(Covid)
summary(Covid)
names(Covid)
# there are lot of missing value
# cdc_report_dt has read has date : good
# other column should change to factor


# Dummyes
cols <- c("age_group","sex" ,"Race.Ethnicity" ,"hosp_yn","medcond_yn","death_yn"  )
Covid[cols] <- lapply(Covid[cols], factor)
str(Covid)

##PLOT
par(mfrow=c(2,1))
# Plot the the number of all cases based on age group
plot(Covid$age_group,main = "all cases by age")
#Plot the the number of death cases based on age group
plot(Covid$age_group[Covid$death_yn=="Yes"],main = "death by age")
# the most case are in range of (20-29)
# the most deaths are in +80 age


### (b)

##### Missing value imputation 
summary(Covid)
Covid$Race.Ethnicity[Covid$Race.Ethnicity=="Missing"]<-"White, Non-Hispanic"
Covid$hosp_yn[Covid$hosp_yn=="Missing"]<-"No"
Covid$medcond_yn[Covid$medcond_yn=="Missing"]<-"No"
Covid$death_yn[Covid$death_yn=="Missing"]<-"No"
summary(Covid)
cols <- c("age_group","sex" ,"Race.Ethnicity" ,"hosp_yn","medcond_yn","death_yn"  )
Covid[cols] <- lapply(Covid[cols], factor)
summary(Covid)

######subset  train and test
train=(Covid$cdc_report_dt<"2020-11-01")
table(train)

test=Covid[!train,]
summary(test)
death_yn.test=Covid$death_yn[!train]

glm.fit=glm( death_yn~.-cdc_report_dt,data=Covid,family=binomial,subset=train)
summary(glm.fit)
# except "age_group10 - 19 Years" and "Race.EthnicityNative Hawaiian/Other Pacific Islander, Non-Hispanic" other are significant


###(c)

logodd <- glm.fit$coefficients["sexMale"]
# sexMale 
# 0.3552965 

exp(logodd)
# odd =1.426604
# so if two person with identical condition but one of them is man and the other one is woman then:
#the odds of prediction death for that man is 1.426604 times of  that woman



###(d)
logodd75 <- glm.fit$coefficients["age_group70 - 79 Years"]
logodd36 <- glm.fit$coefficients["age_group30 - 39 Years"]
odd.75 <- exp(logodd75)
odd.36 <- exp(logodd36)
ratio <- odd.75/odd.36
# 38.14909 
# as the ratio of the odds is 38.14909  so in our model prediction the odds of dying a person with 75 years is 38 times more than the person with 36 years old


### (e)
logg.white = glm.fit$coefficients["Race.EthnicityWhite, Non-Hispanic"]
odd.white = exp(logg.white)
#odd.white= 0.5642979
odd.color = 1/odd.white
#odd.color = 1.772114 
# as a person is White or not White(Color) and as we can see the odds of are prediction for being white is less than the color one


###(f)

summary(Covid)
x.A<- data.frame(cdc_report_dt =as.Date("2020-10-12") ,
                 sex = factor("Male"),
                 age_group=factor("30 - 39 Years"),
                 Race.Ethnicity=factor("Multiple/Other, Non-Hispanic"),
                 hosp_yn=factor("No"),
                 medcond_yn=factor("No")
                )

1 - predict(glm.fit,x.A,type='response')

# probability of my survival(35 year old) = 0.9991072 

x.Old<- data.frame(cdc_report_dt =as.Date("2020-10-12") ,
                 sex = factor("Male"),
                 age_group=factor("70 - 79 Years"),
                 Race.Ethnicity=factor("Multiple/Other, Non-Hispanic"),
                 hosp_yn=factor("No"),
                 medcond_yn=factor("No")
)
levels(Covid$age_group)
1- predict(glm.fit,x.Old,type='response')

# probability of elderly person(70 - 79 Years) survival =   0.9670322 



###(g)


glm.probs=predict(glm.fit,test,type="response")
length(glm.probs)
glm.pred=rep("No",1687960)
glm.pred[glm.probs>.2]="Yes"
table(death_yn.test,glm.pred)

mean(glm.pred==death_yn.test)
TN=1663022
TP=3380
FN=7847
FP=13711

sensivity=TP/(TP+FN)
sensivity
#0.3010599
specifity=TN/(TN+FP)
specifity
#0.9918228
accuracy=(TP+TN)/(TN+TP+FP+FN)
accuracy
#0.9872284
g.mean=sqrt(sensivity*specifity)
g.mean
# 0.5464413

####(h)
table(death_yn.test)
# accuracy = "Truly predicted"/ "all cases"
# accuracy = ("all cases" - "death")/ "all cases"
(1687960-11227)/1687960
# 0.9933488


###(i)
library('ROCR')
pred<-prediction(glm.probs,death_yn.test)
as.numeric(performance(pred, "auc")@y.values)
#auc =  0.9457341
perf <- performance(pred,"tpr","fpr")
plot(perf,colorize=TRUE)
abline(a=0,b=1)
# here the auc means that if you give the model a person it will predict his or her survival correctly by this probability( 0.9457341)


#####################################(j)


rm(list=ls())
library(readr)
Covid <- read_csv("Covid_Data.csv")
### ( Bonus B)

##### Missing value imputation 
summary(Covid)
Covid <- Covid[Covid$Race.Ethnicity!="Missing" & Covid$hosp_yn!="Missing" & Covid$medcond_yn!="Missing" & Covid$death_yn!="Missing" ,]
summary(Covid)
cols <- c("age_group","sex" ,"Race.Ethnicity" ,"hosp_yn","medcond_yn","death_yn"  )
Covid[cols] <- lapply(Covid[cols], factor)
summary(Covid)
######subset  train and test
train=(Covid$cdc_report_dt<"2020-11-01")
table(train)
test=Covid[!train,]
summary(test)
death_yn.test=Covid$death_yn[!train]

glm.fit=glm( death_yn~.-cdc_report_dt,data=Covid,family=binomial,subset=train)
summary(glm.fit)
### What variables are significant in this model
# sexMale ,
#age_group30 - 39 Years ,
#age_group30 - 39 Years                                               
#age_group40 - 49 Years                                             
#age_group50 - 59 Years                                             
#age_group60 - 69 Years                                           
#age_group70 - 79 Years                                              
#age_group80+ Years                                                
#Race.EthnicityWhite, Non-Hispanic
#hosp_ynYes
#medcond_ynYes

####(Bonus C)

logodd <- glm.fit$coefficients["sexMale"]
# sexMale 
# 0.2878192 
exp(logodd)
# odd =1.333516
# so if two person with identical condition but one of them is man and the other one is woman then:
#the odds of prediction death for that man is 1.333516 times of  that woman



###(Bonus d)
logodd75 <- glm.fit$coefficients["age_group70 - 79 Years"]
logodd36 <- glm.fit$coefficients["age_group30 - 39 Years"]
odd.75 <- exp(logodd75)
odd.36 <- exp(logodd36)
ratio <- odd.75/odd.36
# 17.67417  
# as the ratio of the odds is 17.67417   so in our model prediction the odds of dying a person with 75 years is 38 times more than the person with 36 years old


### (Bonus e)
logg.white = glm.fit$coefficients["Race.EthnicityWhite, Non-Hispanic"]
odd.white = exp(logg.white)
#odd.white= 0.6419625 
odd.color = 1/odd.white
#odd.color =  1.557723  
# as a person is White or not White(Color) and as we can see the odds of are prediction for being white is less than the color one


###(Bonus f)

summary(Covid)
x.A<- data.frame(cdc_report_dt =as.Date("2020-10-12") ,
                 sex = factor("Male"),
                 age_group=factor("30 - 39 Years"),
                 Race.Ethnicity=factor("Multiple/Other, Non-Hispanic"),
                 hosp_yn=factor("No"),
                 medcond_yn=factor("No")
)

predict(glm.fit,x.A,type='response')

# probability of my survival(35 year old) = 0.001070625 

x.Old<- data.frame(cdc_report_dt =as.Date("2020-10-12") ,
                   sex = factor("Male"),
                   age_group=factor("70 - 79 Years"),
                   Race.Ethnicity=factor("Multiple/Other, Non-Hispanic"),
                   hosp_yn=factor("No"),
                   medcond_yn=factor("No")
)
levels(Covid$age_group)
predict(glm.fit,x.Old,type='response')

# probability of elderly person(70 - 79 Years) survival = 0.01859053 




###(Bonus g)


glm.probs=predict(glm.fit,test,type="response")
length(glm.probs)
glm.pred=rep("No",102162)
glm.pred[glm.probs>.2]="Yes"
table(death_yn.test,glm.pred)

mean(glm.pred==death_yn.test)
TN=98262
TP=811
FN=800
FP=2289

sensivity=TP/(TP+FN)
sensivity
#0.503414
specifity=TN/(TN+FP)
specifity
# 0.9772354
accuracy=(TP+TN)/(TN+TP+FP+FN)
accuracy
#0.9697637
g.mean=sqrt(sensivity*specifity)
g.mean
#0.7013943

####(Bonus h)
table(death_yn.test)
# accuracy = "Truly predicted"/ "all cases"
# accuracy = ("all cases" - "death")/ "all cases"
(102162-1611)/102162
# 0.9842309


###(Bonus i)
library('ROCR')
pred<-prediction(glm.probs,death_yn.test)
as.numeric(performance(pred, "auc")@y.values)
#auc =   0.9259182
perf <- performance(pred,"tpr","fpr")
plot(perf,colorize=TRUE)
abline(a=0,b=1)
# here the auc means that if you give the model a person it will predict his or her survival correctly by this probability(  0.9259182)









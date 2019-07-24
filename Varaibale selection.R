#Intro to Variable selection
cts<-read.csv(file.choose(),stringsAsFactors = F)
glimpse(cts)
library(leaps)
library(tidyverse)
wages<-read.csv(file.choose())

#using best subset to find best model to use
subs<-regsubsets(earn ~., data = wages)
summary(subs)
#check for best variables to use for model
c<-summary(subs)$bic#variableswith lowest values
b<-summary(subs)$cp#variableswith lowest values
a<-summary(subs)$adjr2# highest values
#use the values to construct a dataframe
df<-data.frame(
  est=c(a,b,c),
  x=rep(1:7,3),
  type=rep(c("adjr2", "cp", "bic"),each=7)
)
df
#plot df
df %>% ggplot(aes(x,est))+
  geom_line()+
  theme_bw()+
  facet_grid(type~., scales = "free_y")
coef(subs,3)

wage<-wages
str(wage)
wage$sex<-factor(wage$sex,
             levels = c("male","female"))

subs<-regsubsets(earn~., data = wage)
#summary of subs
summary(subs)
a<-summary(subs)$adjr2
b<-summary(subs)$bic
c<-summary(subs)$cp
df<-data.frame(
  est=c(a,b,c),
  x=rep(1:7,3),
  type=rep(c("adjr2","bic","cp"), each=7)
)
df %>% ggplot(aes(x,est))+geom_line(col="blue")+
  theme_bw()+ facet_grid(type~., scales = "free_y")
coef(subs,3)#get best variables to predict earn

#find best variablesto predict crime in cts dataset
subs_2<-regsubsets(crime ~., data = cts,really.big = T)
?regsubsets















<<<<<<< HEAD
#linear modelling (Continuos variable)
=======
#linear modelling
>>>>>>> bad1a5c93d9a2b6665451ab2a661041b24ff63f9
library(tidyverse)
#import the data
crime=read.csv(file.choose())
#model the data using lm
mod=lm(tc2009 ~ low, crime);mod
#plot a graph to view the linear relationship
ggplot(crime,aes(low, predict(mod)))+
  geom_point()+ 
  geom_line()#graph shows a +ve linear r/p b/w both variables
#view summary of model
summary(mod)
#use names to get values of mod
names(mod)
#we can model this relationship using geom smooth
crime %>% ggplot(aes(low, tc2009))+
  geom_smooth(method=lm)+#use lm for linear modelling
  geom_point()
#remove confidence interval
crime %>% ggplot(aes(low,tc2009))+
  geom_smooth(method=lm, se=F)+
  geom_point()

##predicting model without intercept
no_intercept=lm(tc2009~ 0 +low, data = crime);no_intercept
#drawing the graph
ggplot(crime, aes(low,predict(no_intercept)))+
  geom_line(col="blue")+
  geom_point()#shows -ve r/p, best practice, always use intercept


#tocheck whether earnings depends on heights
wages<-read.csv(file.choose())
glimpse(wages)
mod<-lm(earn~ height, data = wages)
summary(mod)
#result shows that a one increase in height result in $2387 increase in
#earning
#for a peson who is 75cm, his salary is: $52,502
-126523 + 2387 *(75)
coef(mod)
#graph the r/p
ggplot(wages, aes(height, predict(mod)))+
  geom_line(col="red")#the more tall, the more earnings
wages %>% ggplot(aes(height, earn,alpha=I(0.4)))+
  geom_smooth(method = lm, se=F)+
  geom_point()+ theme_bw()+#white and black background
  xlab("Height in Inches")+
  ylab("Amount Earned")+
  ggtitle("Relationship Between Height and Earnings")
<<<<<<< HEAD
##using 95% confidence interval
confint(mod,level = 0.95)#0.95 is the default
autoplot(mod)#shows quantile, residuals graph, variance

#model aerning on race
model<-lm(earn ~ race, data = wages)
summary(model)

#model earning on sex sing lm
mod_sex<-lm(earn ~ sex, data = wages)#female is default
summary(mod_sex)
#use male as default
wage_2=wages# copy wages table
#reorder male and emale in sex column so that mae come first
wage_2$sex<-factor(wage_2$sex,
                   levels = c("male","female"))
mod_sex2=lm(earn ~ sex, data =wage_2)
summary(mod_sex2)
coef(mod_sex);coef(mod_sex2)
autoplot(mod_sex2)
autoplot(mod_sex)
#seems male earn higher
#plot a graph to view the earnings f both sexes

wages %>% ggplot(aes( earn, col=sex))+
  geom_density()+ theme_bw()
#view earnings byraces
wages %>% ggplot(aes(earn, col=race))+
  geom_density()+
  scale_color_discrete(name="Race")





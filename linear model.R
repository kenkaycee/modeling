#linear modelling
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




















#Non linear model
library(ggfortify)
data("diamonds")
#plot a scatterplot of price vs carat
diamonds %>% ggplot(aes(carat,price))+
  geom_point()+ geom_smooth()# hows that relationship is not linear
#use log to transform data and run linear regression
mod<-lm(log10(price) ~ log10(carat), data = diamonds)
summary(mod)
#plot the graph
diamonds %>% ggplot(aes(log10(carat), log10(price)))+geom_point()+
  geom_smooth(method = "lm")
#using polynomials to model data that cannot be transformed linearly
wages<-read.csv(file.choose())
#plot the r/p b earn and age
wages %>% ggplot(aes(age, earn))+
  geom_point()+
  geom_smooth(method = "lm")+
  coord_cartesian(ylim = c(0,75000))
mod=lm(earn ~ age, data = wages)#does not yield meaning info: no linear rp
summary(mod)# stat insigniicant: p-value 0.005
confint(mod)# too wide
#try transform with log
mod_log10<-lm(log10(earn) ~ log10(age), data = wages)#produced errors
#transform using polynomials
mod_poly<-lm(earn ~ poly(age, 3), data = wages)
summary(mod_poly)
confint(mod_poly)#narrow interval
#visualise
wages %>% ggplot(aes(age,earn))+
  geom_point()+
  geom_smooth(method = "lm", formula = y ~ poly(x,3))+#use formula of a polynomial fn
  coord_cartesian(ylim = c(0,75000))
#create different models and compare with anova
mod1<-lm(earn ~ age, data = wages)
mod2<-lm(earn ~ poly(age,2), data = wages)
mod3<-lm(earn ~ poly(age,3), data = wages)
#compare with anova
anova(mod1, mod2, mod3)
confint(mod1); confint(mod2); confint(mod3)
library(splines)
#splines are useful in moelling non linear variables
#ns= natural cubic spine, 

mod_spline<-lm(earn ~ ns(age, df=6), data = wages)
summary(mod_spline)
anova(mod_spline)
#plot the graph
wages %>% ggplot(aes(age,earn))+
  geom_point()+
  geom_smooth(method = lm, formula = y ~ ns(x, df=6))+
  coord_cartesian(ylim = c(0,50000))#shows cycle of earn ~ age













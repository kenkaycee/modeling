#multivariate model
library(tidyverse)
rmodel<- lm(earn~ height+ sex, data = wages)
summary(rmodel)
#graph the model
wages %>% ggplot(aes(height,earn,col=sex, alpha=I(1/10)))+
  geom_line(aes(y=predict(rmodel)))
qplot(height, earn, data = wages, col=sex, alpha=I(1/10))+
  theme_bw()+ geom_line(aes(y=predict(rmodel)))
ggplot(wages, aes(height, earn, col=sex))+geom_point()+
  theme_bw()+ geom_line(aes(y=predict(rmodel)), size=2, lty=3)
rmodel<- lm(earn ~ height + race+ ed+ sex + age, data = wages)
summary(rmodel)
anova(rmodel)
m1<-lm(earn ~ height + sex, data = wages)#model on height and sex
summary(m1)
m2<-lm(earn ~ height* sex, data = wages)
summary(m2)
anova(m2)
anova(m1,m2)#compare two models

#inference for multivariate regression
#test for multicollinearity using cor-closer to 0 is better
cor(wages$height, as.numeric(wages$race))
cor(wages$height, wages$ed)
cor(wages$height, as.numeric(wages$sex))






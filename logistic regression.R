#logistic regression
Titanic1<-read.csv(file.choose(), stringsAsFactors = F)
#graph of survived against age
Titanic1 %>% ggplot(aes(age, survived))+ geom_point()+
  geom_smooth(se=F)+ggtitle("Survival by Age in Titanic crash")
#first run this line of code
options(na.action = "na.exclude")#keep values with NAs
logis_mod<- gam(survived ~ s(age), data = Titanic1,family = binomial)
summary(logis_mod)
predict(logis_mod, type = "response") # or
fitted(logis_mod)

Titanic1 %>% ggplot(aes(age, survived))+
  geom_point()+
  geom_smooth(se=F)+
  geom_line(aes(y= fitted(logis_mod)), col="red")

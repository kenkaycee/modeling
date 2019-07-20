#one way anova
#best workflow for modelling:
#Plot → Model → Check Assumptions → Interpret → Plot gain
#Daphniagrowth data
daphnia<- read.csv(file.choose())
#Step 1: plot a boxplot 
daphnia %>% ggplot(aes(parasite,growth.rate, fill=parasite))+
  geom_boxplot()+
  theme_bw()+
  coord_flip()# switch xand y axes
sum_daph<-daphnia %>% group_by(parasite) %>% 
  summarise(avg=mean(growth.rate))#average growth rate by treatments
#step 2: construct the model
model_grow=lm(growth.rate ~ parasite, data = daphnia)
#Step 3: Check Assumptions of Linear models
autoplot(model_grow)
#save graph 
ggsave("daphnia.pdf")
#output result of model using summary
summary(model_grow)
#output result of model using one way anova
anova(model_grow)#simiar result with summary
#Step 6: Interpret
#plot the graphagain
ggplot(daphnia, aes(parasite, growth.rate, col=parasite))+
  geom_point(size=5)+ coord_flip()

ggplot(daphnia,aes(parasite, growth.rate, col=parasite))+
  geom_point()+
  geom_point(aes(parasite, avg), data = sum_daph, size=5,shape=18)+
  coord_flip()
#second geompoint is to get avg growth rate for each treatment
#represented by diamod shape in the graph







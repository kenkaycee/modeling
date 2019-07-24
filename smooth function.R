#smooth function to model non linear rgression
install.packages("mgcv")
library(tidyverse)
library(splines)
library(mgcv)
library(ggfortify)
#Generalised additive model
gam_mod<-gam(earn ~ s(age), data = wages)
rm(mod_smooth)
summary(gam_mod)
plot(gam_mod)
#visualise the model using gam to see r/p b/w age and earning
wages %>% ggplot(aes(age, earn))+
  geom_point()+
  geom_smooth(method = gam, formula = y ~ s(x))+
  coord_cartesian(ylim = c(0,50000))# to zoom in
#NB: For non-linear relationsip: linear model, spline, smooth fn, 
#log-transformation requires that y is a continous variables and that 
#errors are normally dsn
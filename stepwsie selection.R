#Stepwise selection: used to select best variables for modelling
#first create empty mod, lower mod, and uppe mod
start_mod<- lm(earn ~ height, data = wages)
lower_mod<- lm(earn ~ 1, data = wages)
full_mod<- lm(earn ~., data = wages)
step(start_mod,
     scope = list(upper=full_mod,
                  lower=lower_mod),
     direction = "forward")

#use step on cts data
cts<-read.csv(file.choose(),stringsAsFactors = F)
cts_2<- cts %>% select(-county, -state)

start.mod<-lm(crime ~ pop, data = cts_2)
lower.mod<-lm(crime ~ 1, data = cts_2)
full.mod<-lm(crime ~., data = cts_2)
step(start.mod,
     scope = list(lower=lower.mod, upper=full.mod),
     direction = "forward")








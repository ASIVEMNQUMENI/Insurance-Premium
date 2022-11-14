#MNQUMENI ASIVE

library(tidyverse)
library(ggplot2)
library(dplyr)
library(class)
library(MASS)
library(caret)
library(devtools)
library(forcats)
library(AER)
library(pscl)
library(Hmisc)
library(countreg)

# pre processing 
data <- car_insurance_claim
data1 <- unique(data) #remove duplicates
str(data1)
names(data1) <- str_to_lower(str_replace_all(names(data1), c(" " = "_" , "," = "", "\\*" = "", "\\(" = "", "\\)" = "", "`" = "", "\\/" = "_"))) # change to small letters
names <- c('car_type' ,'gender','education','occupation','urbanicity','revoked','car_use','mstatus','parent1')
# since we will be fitting a gamma and a Gaussian dists with log link function we want all values to be positive 
data2 <- data1%>% filter(data1 $ target_amt > 0)
data2 <- data2%>% filter(data2 $ clm_freq > 0)
summary(data2$target_amt)

#remove out lairs over 99 percentile
data3 <- data2[data2$target_amt > quantile(data2$target_amt, 0.0001), ] 
data4 <- data3[data3$target_amt< quantile(data3$target_amt, 0.999), ]
summary(data4$target_amt)

#check the distribution of target/claim amount
hist(data4$target_amt) # the distribution is highly positively skewed suggesting that we can use gamma distribution
data4$log_tag_amt <- log(data4$target_amt)
hist(data4$log_tag_amt) # The logarithm if the target amount resembles a normal or Gaussian distribution

data5 <- data4[data4$target_amt < quantile(data3$target_amt, 0.999), ]

#partition the data set on to training and testing data sets.
data_partition <-caret:: createDataPartition(data5$clm_freq, times = 1,p = 0.8,list = FALSE)
str(data_partition)
training <- data5[data_partition,]
testing  <- data5[-data_partition,]

# scatter plot matrix of numeric cols vs target_amt
numeric_cols <- unlist(lapply(training,is.numeric),use.names = FALSE)
training[,numeric_cols]%>%
  gather(-target_amt,key = "key",value ='value')%>%
  ggplot(aes(x=value,y=target_amt))+
  geom_point(alpha = 0.2)+
  facet_wrap(~key,scales = 'free')

#model gamma, 
model_gamma <- glm(target_amt ~bluebook + car_type + gender,
                   data = training,offset = log(clm_freq),family=Gamma(link="log"))
summary(model_gamma)
plot(model_gamma)
testing$pred_gamma <- predict(model_gamma, newdata=testing, type="response")
sqrt(mean((testing$pred_gamma - testing$target_amt)^2)) 

#model Gaussian
model_gaussian <- glm(target_amt ~ bluebook+car_type+ gender,
                      data = training,offset = log(clm_freq),family=gaussian(link="log"))
summary(model_gaussian)
plot(model_gaussian)
testing$pred_gauss <- predict(model_gaussian, newdata=testing, type="response")
sqrt(mean((testing$pred_gauss - testing$target_amt)^2)) 

# Comparing the two models, gamma has a lower AIC compared to the Gaussian but on the other hand, Gaussian has a lower RMSE 
#I'm not sure which one to chose at this point


#fit glm for claim frequency on the original data set
data6 <- car_insurance_claim
data6 <- unique(data6) #remove duplicates
str(data6)
names(data6) <- str_to_lower(str_replace_all(names(data6), c(" " = "_" , "," = "", "\\*" = "", "\\(" = "", "\\)" = "", "`" = "", "\\/" = "_"))) # change to small letters
names <- c('car_type' ,'gender','education','occupation','urbanicity','revoked','car_use','mstatus','parent1')
data6[,names] <- lapply(data6[,names] , factor)
str(data6)

#split it into two

data_partition <-caret:: createDataPartition(data6$clm_freq, times = 1,p = 0.8,list = FALSE)
str(data_partition)
train <- data6[data_partition,]
test  <- data6[-data_partition,]

#fit poisson dist
poissonglm <- glm(clm_freq ~   car_use + car_type + age+ mstatus +occupation + gender+car_use +parent1,data=train,family = poisson(link="log") ,offset=log(exposure))
summary(poissonglm)
dispersiontest(poissonglm,trafo = 1) #from this we notice that there's over-dispersion, therefore zero inflated poisson might be a good fit

zip <- zeroinfl(clm_freq ~   car_use + car_type + age+ mstatus +occupation + gender+car_use +parent1|car_use + car_type + age+ mstatus +occupation + gender+car_use +parent1,offset=log(exposure),data=train,dist = "poisson",link= "logit")
summary(zip)

par(mfrow = c(1, 2))
rootogram(poissonglm,max = 10,main="Poisson") 
rootogram(zip,max = 10,main="zip")

# this model is nowhere near perfect as we can see. 
#On this dataset we had more people who claimed twice compared to those that claimed  only once. which makes it hard to fit a poisson dist or negative binomial
testing$claim_frq_poi <- predict(poissonglm,newdata = testing, type = 'response') #predict claims
testing$claim_frq_zip <- predict(zip,newdata = testing, type = 'response') #predict claims


summary(testing$claim_frq_poi)
summary(testing$claim_frq_zip)

#the means are not what i expected them to be 

sum(data1$clm_freq)/sum(data1$exposure)# actual mean is 0.150255

#calculate premium
testing$premium_gama <- testing$claim_frq_poi*testing$pred_gamma
testing$premium_gauss <- testing$claim_frq_poi*testing$pred_gauss

testing <- testing[!is.na(testing$premium1),]

sum(testing$target_amt)
sum(testing$premium1)
sum(testing$premium2)

#furthermore, the total premiums sum up to a value that's lower that the total claims made in any given year. I guess it's because of the claim freq that i predicted with my models being inaccurate.
#I think i did a good job in choosing the models and the variables to predict the claim severity.
#I'm not sure if the problem is the dataset that I'm using are my models,

#Thank you for your attention.


library(ggplot2)
library(tidyverse)
dat = read.csv("350s/Placement_Data_Full_Class.csv",stringsAsFactors=FALSE, sep=",")

head(dat)
str(dat)
dat = dat[ -c(1)]
dat[dat==""] = NA
sapply(dat,function(x) sum(is.na(x)))
sapply(dat, function(x) length(unique(x)))

# Give avg values to missing salary data
dat$salary[is.na(dat$salary)] <- mean(dat$salary,na.rm = T)


#These graphs are designed to show the correlation between getting placed or not based on a predictor variable 
#You will need to load them one at a time to view them.

#work experience
dat %>% ggplot(aes(x = workex)) +  geom_bar(aes(fill = status))

#ssc_p
dat %>% ggplot(aes(x = ssc_p, fill = ssc_p)) +  geom_bar(aes(fill = status))

#degree_p
dat %>% ggplot(aes(x = degree_p, fill = degree_p)) +  geom_bar(aes(fill = status))

#hsc_p
dat %>% ggplot(aes(x = hsc_p, fill = hsc_p)) +  geom_bar(aes(fill = status))

#mba_p
dat %>% ggplot(aes(x = mba_p, fill = mba_p)) +  geom_bar(aes(fill = status))

#gender
dat %>% ggplot(aes(x = gender, fill = gender)) +  geom_bar(aes(fill = status))

#degree_t
dat %>% ggplot(aes(x = degree_t, fill = degree_t)) +  geom_bar(aes(fill = status))

#ssc_b
dat %>% ggplot(aes(x = ssc_b, fill = ssc_b)) +  geom_bar(aes(fill = status))

#hsc_s
dat %>% ggplot(aes(x = hsc_b, fill = hsc_b)) +  geom_bar(aes(fill = status))

dat.new = subset(dat, select = c(2,4,7,10,12))

dat.new $ gender = factor(dat $ gender)
dat.new $ ssc_b = factor(dat $ ssc_b)
dat.new $ hsc_b = factor(dat $ hsc_b)
dat.new $ hsc_s = factor(dat $ hsc_s)
dat.new $ degree_t = factor(dat $ degree_t)
dat.new $ workex = factor(dat $ workex)
dat.new $ specialisation = factor(dat $ specialisation)
dat.new $ status = factor(dat $ status)

datglm = dat.new[, c("status", "ssc_p", "hsc_p", "degree_p", "mba_p", "workex")]

datglm$status = ifelse(datglm$status == 'Placed',1,0)
train = datglm[1:170,]
test = datglm[170:215,]

trainedModel = glm(status ~. ,family=binomial(link='logit'),data=train)
fitted.results = predict(trainedModel,newdata = test, type='response')
fitted.results = ifelse(fitted.results > 0.5,1,0)
misClasificError = mean(fitted.results != test$status)
print(paste('Accuracy',1 - misClasificError))

tester = ifelse(test$status == 0, "Not Placed", "Placed")
predict = ifelse(fitted.results == 0, "Not Placed", "Placed")
dat_compare = matrix(c(tester, predict ) , ncol=2)
colnames(dat_compare) = c("Test","Prediction")
rownames(dat_compare) = c(170:215)
dat_compare
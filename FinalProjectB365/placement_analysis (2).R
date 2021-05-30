library(rpart)
library(rpart.plot)
data=read.csv("350s/Placement_Data_Full_Class.csv",stringsAsFactors=FALSE, sep=",")
dimnames(data)

set.seed(350)
dt = sort(sample(nrow(data), nrow(data)*0.8))
train = data[dt,]
test = data[-dt,]

# Using all factors
cat("Tree using all attributes \n")
fit=rpart(status ~ gender + ssc_p + ssc_b + hsc_p + hsc_b + hsc_s + degree_p + degree_t +workex + etest_p + specialisation + mba_p,
         data = train, method="class", parms=list(split="information"))
fit
#print(fit)
rpart.plot(fit)
printcp(fit)
newfit = prune(fit, cp=0.01)
rpart.plot(newfit)
?rpart.plot

predicted = predict(fit, test)
?predict



n = nrow(predicted)
predicted_vals = rep(FALSE, n)
predicted_id = dimnames(predicted)[[1]]
TP = 0
TN = 0
FP = 0
FN = 0
for(i in 1:n){
  if(predicted[i, "Not Placed"] >= 0.5) {predicted_vals[i] = FALSE}
  else{predicted_vals[i] = TRUE}
  if ((data[predicted_id[i],"status"] == "Placed") && (predicted_vals[i])){
    TP = TP + 1;
  }
  else if((data[predicted_id[i],"status"] == "Not Placed") && (predicted_vals[i] == FALSE)){
    TN = TN + 1;
  }
  else if((data[predicted_id[i],"status"] == "Placed") && (predicted_vals[i] == FALSE)){
    FP = FP + 1;
  }
  else {
    FN = FN + 1;
  }
}

TP
TN
FP
FN

error_matrix = matrix(c(TP, FN, FP, TN), nrow = 2, ncol = 2)
rownames(error_matrix) = c("+", "-")
colnames(error_matrix) = c("+", "-")
error_matrix

error_rate = (TP + TN)/(TP + TN + FP + FN)
accuracy = 1 - error_rate
cat("Accuracy using tree with all attributes: ", accuracy, "\n")


# Removing ssc_p, hsc_p, degree_p, mba_p 
cat("Tree removing ssc_p, hsc_p, degree_p, mba_p \n")
fit2=rpart(status ~ gender + ssc_b + hsc_b + hsc_s + degree_t + etest_p + specialisation + workex,
          data = train, method="class", parms=list(split="information"))
fit2
printcp(fit2)
newfit2=prune(fit2, cp=0.017857)
rpart.plot(newfit2)
predicted2 = predict(newfit2, test)

n = nrow(predicted2)
predicted_vals2 = rep(FALSE, n)
predicted_id2 = dimnames(predicted2)[[1]]
TP2 = 0
TN2 = 0
FP2 = 0
FN2 = 0
for(i in 1:n){
  if(predicted2[i, "Not Placed"] >= 0.5) {predicted_vals2[i] = FALSE}
  else{predicted_vals2[i] = TRUE}
  if ((data[predicted_id2[i],"status"] == "Placed") && (predicted_vals2[i])){
    TP2 = TP2 + 1;
  }
  else if((data[predicted_id2[i],"status"] == "Not Placed") && (predicted_vals2[i] == FALSE)){
    TN2 = TN2 + 1;
  }
  else if((data[predicted_id2[i],"status"] == "Placed") && (predicted_vals2[i] == FALSE)){
    FP2 = FP2 + 1;
  }
  else {
    FN2 = FN2 + 1;
  }
}

TN2
TP2
FP2
FN2

error_rate2 = (TP2 + TN2)/(TP2 + TN2 + FP2 + FN2)
accuracy2 = 1 - error_rate2
cat("Accuracy using tree without ssc_p, hsc_p, degree_p, mba_p: ", accuracy2, "\n")

# Based only on percentages from school
cat("Tree using ssc_p, hsc_p, degree_p, and mba_p \n")
fit3=rpart(status ~ ssc_p + hsc_p + degree_p + mba_p,
           data = train, method="class", parms=list(split="information"))
fit3
printcp(fit3)
newfit3 = prune(fit3, cp = 0.01)
rpart.plot(newfit3)
predicted3 = predict(newfit3, test)

n = nrow(predicted3)
predicted_vals3 = rep(FALSE, n)
predicted_id3 = dimnames(predicted3)[[1]]
TP3 = 0
TN3 = 0
FP3 = 0
FN3 = 0
for(i in 1:n){
  if(predicted3[i, "Not Placed"] >= 0.5) {predicted_vals3[i] = FALSE}
  else{predicted_vals3[i] = TRUE}
  if ((data[predicted_id3[i],"status"] == "Placed") && (predicted_vals3[i])){
    TP3 = TP3 + 1;
  }
  else if((data[predicted_id3[i],"status"] == "Not Placed") && (predicted_vals3[i] == FALSE)){
    TN3 = TN3 + 1;
  }
  else if((data[predicted_id3[i],"status"] == "Placed") && (predicted_vals3[i] == FALSE)){
    FP3 = FP3 + 1;
  }
  else {
    FN3 = FN3 + 1;
  }
}

TN3
TP3
FP3
FN3

error_rate3 = (TP3 + TN3)/(TP3 + TN3 + FP3 + FN3)
accuracy3 = 1 - error_rate3
cat("Accuracy using hsc_p, ssc_p, degree_p, mba_p: ", accuracy3, "\n")

# Based on specialization, workex, degree type
cat("Tree using specialisation, workex, degree_t \n")
fit4=rpart(status ~ specialisation + workex + degree_t,
           data = train, method="class", parms=list(split="information"))
fit4
printcp(fit4)
newfit4 = prune(fit4, cp=0.01)
rpart.plot(newfit4)
predicted4 = predict(newfit4, test)

n = nrow(predicted4)
predicted_vals4 = rep(FALSE, n)
predicted_id4 = dimnames(predicted4)[[1]]
TP4 = 0
TN4 = 0
FP4 = 0
FN4 = 0
for(i in 1:n){
  if(predicted4[i, "Not Placed"] >= 0.5) {predicted_vals4[i] = FALSE}
  else{predicted_vals4[i] = TRUE}
  if ((data[predicted_id4[i],"status"] == "Placed") && (predicted_vals4[i])){
    TP4 = TP4 + 1;
  }
  else if((data[predicted_id4[i],"status"] == "Not Placed") && (predicted_vals4[i] == FALSE)){
    TN4 = TN4 + 1;
  }
  else if((data[predicted_id4[i],"status"] == "Placed") && (predicted_vals4[i] == FALSE)){
    FP4 = FP4 + 1;
  }
  else {
    FN4 = FN4 + 1;
  }
}

TN4
TP4
FP4
FN4

error_rate4 = (TP4 + TN4)/(TP4 + TN4 + FP4 + FN4)
accuracy4 = 1 - error_rate4
cat("Accuracy using specialisation, workex, and degree_t: ", accuracy4, "\n")


# highest correlations ~ accuracy = 32.55%
cat("Tree using attributes with highest correlation \n")
fit5=rpart(status ~ ssc_p + hsc_p + degree_p + workex,
           data = train, method="class", parms=list(split="information"))
fit5
printcp(fit5)
newfit5=fit5
rpart.plot(newfit5)
predicted5 = predict(newfit5, test)

n = nrow(predicted5)
predicted_vals5 = rep(FALSE, n)
predicted_id5 = dimnames(predicted5)[[1]]
TP5 = 0
TN5 = 0
FP5 = 0
FN5 = 0
for(i in 1:n){
  if(predicted2[i, "Not Placed"] >= 0.5) {predicted_vals2[i] = FALSE}
  else{predicted_vals5[i] = TRUE}
  if ((data[predicted_id5[i],"status"] == "Placed") && (predicted_vals5[i])){
    TP5 = TP5 + 1;
  }
  else if((data[predicted_id5[i],"status"] == "Not Placed") && (predicted_vals5[i] == FALSE)){
    TN5 = TN5 + 1;
  }
  else if((data[predicted_id5[i],"status"] == "Placed") && (predicted_vals5[i] == FALSE)){
    FP5 = FP5 + 1;
  }
  else {
    FN5 = FN5 + 1;
  }
}

TN5
TP5
FP5
FN5

error_rate5 = (TP5 + TN5)/(TP5 + TN5 + FP5 + FN5)
accuracy5 = 1 - error_rate5
cat("Accuracy using attributes with the highest correlation: ", accuracy5, "\n")





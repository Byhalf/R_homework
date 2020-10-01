library(ISLR)

attach(Auto)
############ question 1.1 ############
data = Auto[1:8]

m <- lm(mpg~cylinders+displacement+horsepower+weight+acceleration+year+
        origin, data)
summary(m)

############ question 1.2 ############

sorted <- Auto[order(Auto$acceleration),]
attach(sorted)

fit1 <- lm(mpg ~ poly(acceleration, 1), data=sorted) 
fit2 <- lm(mpg ~ poly(acceleration, 2), data=sorted) 
fit3 <- lm(mpg ~ poly(acceleration, 3), data=sorted) 
fit4 <- lm(mpg ~ poly(acceleration, 4), data=sorted) 
fit5 <- lm(mpg ~ poly(acceleration, 5), data=sorted) 

message("r squared degree 1 = ",summary(fit1)$r.squared)
message("r squared degree 2 = ",summary(fit2)$r.squared)
message("r squared degree 3 = ",summary(fit3)$r.squared)
message("r squared degree 4 = ",summary(fit4)$r.squared)
message("r squared degree 5 = ",summary(fit5)$r.squared)



plot(acceleration, mpg, main="ISLR: Auto data set", 
	xlab = "Acceleration",
	ylab = "Miles Per Gallon",
	pch = 19,
    lwd = 5,
	col = "red")
 
points(acceleration, predict(fit1), type="l", lwd=5, col="blue") 
points(acceleration, predict(fit2), type="l", lwd=5, col="yellow") 
points(acceleration, predict(fit3), type="l", lwd=5, col="purple") 
points(acceleration, predict(fit4), type="l", lwd=5, col="orange") 
points(acceleration, predict(fit5), type="l", lwd=5, col="green") 

legend("topright",
	c("Linear", "Degree 2","Degree 3", "Degree 4", "Degree 5"),
	col = c("blue","yellow","purple", "orange", "green"),
	lty = c(1,1,1,1,1),
	lwd=c(5,5,5,5,5))

detach(sorted)

########### Question 2.1 ##############

median.mpg = median(Auto$mpg)


mpg01 <- Auto$mpg > median.mpg
mpg01 = mpg01 * 1

d = Auto[-1]
d <- cbind(mpg01,d)
freqs <- table(d$mpg01)/length(d$mpg01)
mpg01.entropy = -sum(freqs * log2(freqs))

message("entropy = ", mpg01.entropy)





########### Question 2.2 ##############
set.seed(123);
num.d = nrow(d)
num.train <- round(0.8 * num.d)
num.test <- num.d - num.train
s <- sample(num.d) 
indices.train <- s[1:num.train]
indices.test <- s[(num.train+1):num.d]
train <- d[indices.train,]
test <- d[indices.test,]


#randomise d
#set.seed(123);
# d <- d[sample(nrow(d)),]

# train <- d[1:round(nrow(d)/100*80),]
# test <- d[round(nrow(d)/100*80)+1:nrow(d),]

########### Question 2.3 ##############


m.0 <- glm(mpg01 ~ -., 
	data = train[1:8], 
	family = binomial(link = 'logit'))

# message("\nModel built.")

pred_0 <- predict(m.0, type = 'response', newdata = test[1:8])
cm <- table(test$mpg01, pred_0)
accuracy= sum(diag(cm))/sum(cm)

message("accuracy no features = ",accuracy)

########### Question 2.4 ##############

m.1 <- glm(mpg01 ~ cylinders+displacement+horsepower+weight+acceleration+year+
        origin, 
	data = train[1:8], 
	family = binomial(link = 'logit'))
pred_2 <- predict(m.1, type = 'response', newdata = train[1:8])
pred_1 <- predict(m.1, type = 'response', newdata = test[1:8])

y_pred1 <- ifelse(pred_1 > 0.5, 1, 0)
y_pred2 <- ifelse(pred_2 > 0.5, 1, 0)


cm1 <- table(test$mpg01, y_pred1 )
accuracy1= sum(diag(cm1))/sum(cm1)

message("accuracy all features ", accuracy1)

print(cm1)

TP = cm1[2,2]
FP = cm1[2,1]
TN = cm1[1,1]
FN = cm1[1,2]

cm1.sensitivity = round(TP/(TP+FP),3)
cm1.specitivity = round(TN/(TN+FP),3)

m.1.training_error <- 1/nrow(train)*sum((train$mpg01-y_pred2)^2)

print("#################################")

message("training error rate = ", m.1.training_error)

print("#################################")

message("Sensitivity = ", cm1.sensitivity)
message("specitivity = ", cm1.specitivity)

m.1.sample_error <- 1/nrow(test)*sum((test$mpg01-y_pred1)^2)
message("test error rate = ", m.1.sample_error)

######### Question 2.5 #############

evalution_logi <- function(pred,d,threshold) {
	y_pred <- ifelse(pred > threshold, 1, 0)
	cm <- table(d, y_pred)

	accuracy= sum(diag(cm))/sum(cm)
	message("accuracy all features ", accuracy)
	print(cm)

	TP = cm[2,2]
	FP = cm[2,1]
	TN = cm[1,1]
	FN = cm[1,2]
	cm.precision = TP/(TP+FP)
	cm.sensitivity = TP/(TP+FP)
	cm.specitivity = TN/(TN+FP)

	message("Sensitivity(recall) = ", cm.sensitivity)
	message("precision = ", cm.precision)

	m.sample_error <- 1/length(d)*sum((d-y_pred)^2)
	message("sample error rate = ", m.sample_error)

	cm.f_score = 2*(cm.precision*cm.sensitivity)/(cm.precision+cm.sensitivity)
	message("F-measure = ",cm.f_score)
}
evalution_logi(pred_1,test$mpg01,0.1)
evalution_logi(pred_1,test$mpg01,0.9)


######### question 2.6 #############

library(rpart)
m.decTree = rpart(mpg01 ~ cylinders+displacement+horsepower+weight+acceleration+year+
        origin,train[1:8], cp=0.01)

library(rpart.plot)
rpart.plot(m.decTree,roundint=FALSE)

prediction <- predict(m.decTree, train[1:8])
table(prediction)


m.decTree.train.cm = table(prediction, train$mpg01)


# training eror rate + test error rate
#error rate = 1 - accuracy

training_error_rate = 1-sum(diag(m.decTree.train.cm))/num.train
message("Training error rate = ", training_error_rate)

prediction.test <- predict(m.decTree, test[1:8])
m.decTree.test.cm = table(prediction.test, test$mpg01)

test_error_rate = 1-sum(diag(m.decTree.test.cm))/num.test
message("Test error rate = ", test_error_rate)

printcp(m.decTree)

m.decTree.cp = rpart(mpg01 ~ cylinders+displacement+horsepower+weight+acceleration+year+
        origin,train[1:8], cp=0.001)
printcp(m.decTree.cp)
#cp = 0.01 seems fine
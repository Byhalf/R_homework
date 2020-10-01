library(randomForest)
library(ISLR)
library(rpart)
library(randomForest)
library(glmnet)
library(ROCR)
attach(Caravan)
library(rpart.plot)

#############Part 1###################

# split data into 1000 test examples and the rest for training
seed = 132
set.seed(seed)
# seed chosen so positive purchased in train is divisible by 10


test.i        = sample(nrow(Caravan), 1000)
caravan.test  = Caravan[test.i,  ]

train.i       = (1:nrow(Caravan))[-test.i]
caravan.train = Caravan[train.i, ]

message("######### part 1 ###########")

message("percentage of purchases in 100 random selected values of training data =",
sum(caravan.train$Purchase=="Yes")/sum(caravan.train$Purchase=="No")*100)

moshoofd.purchased = table(caravan.train$MOSHOOFD,caravan.train$Purchase)
percentage = numeric()
totalPeople = numeric()

for(i in 1:nrow(moshoofd.purchased)){
    totalPeople[i] = moshoofd.purchased[i,1]+moshoofd.purchased[i,2]
    percentage[i] = moshoofd.purchased[i,2]/totalPeople[i]*100
}
level = 1:10
moshfoofd.analysis = cbind(level,totalPeople,percentage)

mostype.purchased = table(caravan.train$MOSTYPE,caravan.train$Purchase)
#mostype.purchased
percentage = numeric()
totalPeople = numeric()
for(i in 1:40){
    if(i<14)
    {
        totalPeople[i] = mostype.purchased[i,1]+mostype.purchased[i,2]
        percentage[i] = mostype.purchased[i,2]/totalPeople[i]*100
    }
    else{
        if(i==14){
        totalPeople[i]=0
        percentage[i] =0
        }
        totalPeople[i+1] = mostype.purchased[i,1]+mostype.purchased[i,2]
        percentage[i+1] = mostype.purchased[i,2]/totalPeople[i+1]*100
    }


}
level = 1:41

mostype.analysis = cbind(level,totalPeople,percentage)
print(moshfoofd.analysis)
print(mostype.analysis)

table(Purchase)/length(Purchase)
table(Caravan$Purchase)

table(MOSHOOFD)/nrow(Caravan)
#correlattion
table(Purchase, Caravan[,5])
#we can see intuitivly lvl 4 
#Pr(Type|Purchase = No)
round((table(Purchase, Caravan[,5]))[1,]/5474*100,1)
#Pr(Type|Purchase = Yes)
round((table(Purchase, Caravan[,5]))[2,]/5474*100,1)
#Entropy
#-sum(Caravan[-1]*log(Caravan[-1]))
entropy = function(x){
return(-sum(x*log2(x)))
}
ent.Purchase = entropy(table(Caravan[,86])/nrow(Caravan))
ent.Subtype = entropy(table(Caravan[,1])/nrow(Caravan))
ent.Maintype = entropy(table(Caravan[,5])/nrow(Caravan))
entropy.condit = function(x,y){
    N = NROW(x)
    p.y = table(y)/N
    p.joint= as.vector(table(y,x))/ N
    p.cond = p.joint/rep(p.y,NROW(table(x)))
    H.cond = -sum(p.joint[p.joint>0]*log2(p.cond[p.cond>0]))
    return(H.cond)
}
entropy.condit(Purchase,MOSHOOFD)
entropy.condit(Purchase,Caravan[,1])
#very similar , so choosing the one with less data sparcity 
# mainType. (MOSHOOFS)

message("######### part 2 ###########")


####################Part 2################
caravan.train.yes = caravan.train[caravan.train$Purchase=="Yes",]
caravan.train.no = caravan.train[caravan.train$Purchase=="No",]

#Making folds with equal number of positives
cv.sample.no <- sample(1:nrow(caravan.train.no))
cv.sample.yes <- sample(1:nrow(caravan.train.yes))

#the number of positives is divisable by 10 no need to add zeros
add.zeros <- 10 - nrow(caravan.train.no) %% 10
if(add.zeros < 10) cv.sample.no <- c(cv.sample.no, rep(0, add.zeros))

cv.index.no <- matrix(data = cv.sample.no, nrow = 10)
cv.index.yes <- matrix(data = cv.sample.yes, nrow = 10)
cv.index = cbind(cv.index.no,cv.index.yes)

########## 2.a decision tree ###############

# function to help calculate confidence interval 
calc_error <- function(sd) {
    return (qt(0.975,df=10-1)*sd/sqrt(10))
}

cps = c(0.001,0.0025,0.00175, 0.002)
AUC.decTree.cp = matrix(data = rep(0,length(cps)*10), nrow = 10)

for(i in 1:10){
    message("decision tree fold #",i)
    cv.train <- caravan.train[ - cv.index[i,][cv.index[i,] > 0], ]
    cv.test  <- caravan.train[ cv.index[i,][cv.index[i,] > 0], ]
 
    count=1
    for(cp in cps){
        decTree = rpart(Purchase ~ .,cv.train,cp=cp)
        prediction <- predict(decTree, cv.test,type="prob")[,2]
        perf.prediction = prediction(prediction, cv.test$Purchase)

        pauc.20 = round(performance(perf.prediction, measure = "auc",fpr.stop=0.2)@y.values[[1]], 3)
        AUC.decTree.cp[i,count] = pauc.20
        count = count+1
    }

  }

mean.decTree = apply(AUC.decTree.cp,2,mean)
sd.decTree = apply(AUC.decTree.cp,2,sd)
error.decTree = sapply(sd.decTree,calc_error)
low.interval <- mean.decTree - error.decTree
high.interval <- mean.decTree + error.decTree
cp.table <- data.frame("cp"=cps,"mean"=mean.decTree,"sd"=sd.decTree,
"low interval"=low.interval,"high interval"=high.interval)
print(cp.table)
message("best cp=0.00175")

################## 2.b Random forest ###############

ntrees = c(10,150,500,600,700,800)
AUC.RF.ntree= matrix(data=rep(0,length(ntrees)*10),nrow=10)

for(i in 1:10){
    message("Random forest ntree fold #",i)
    cv.train <- caravan.train[ - cv.index[i,][cv.index[i,] > 0], ]
    cv.test  <- caravan.train[ cv.index[i,][cv.index[i,] > 0], ]
    count = 1
    for(ntree in ntrees){
        RF = randomForest(Purchase ~ ., cv.train,ntree=ntree)
        prediction = predict(RF,cv.test,type="prob")[,2]
        perf.prediction = prediction(prediction, cv.test$Purchase)
        pauc.20 = round(performance(perf.prediction, measure = "auc",fpr.stop=0.2)@y.values[[1]], 3)
        AUC.RF.ntree[i,count] = pauc.20
        count = count + 1
    }
  }
mean.RF.ntree = apply(AUC.RF.ntree,2,mean)
sd.RF.ntree = apply(AUC.RF.ntree,2,sd)
error.RF.ntree = sapply(sd.RF.ntree,calc_error)
low.interval <- mean.RF.ntree - error.RF.ntree
high.interval <- mean.RF.ntree + error.RF.ntree
ntree.table <- data.frame("ntree"=ntrees,"mean"=mean.RF.ntree,"sd"=sd.RF.ntree,"low interval"=low.interval,"high interval"=high.interval)
print(ntree.table)
message("best ntree= 700")

mtrys = 7:15
AUC.RF.mtry= matrix(data=rep(0,length(mtrys)*10),nrow=10)

for(i in 1:10){
    message( "fold random forest mtry #",i)
    cv.train <- caravan.train[ - cv.index[i,][cv.index[i,] > 0], ]
    cv.test  <- caravan.train[ cv.index[i,][cv.index[i,] > 0], ]
    count = 1
    for(mtry in mtrys){
        RF.5 = randomForest(Purchase ~ ., cv.train,mtry=mtry)
        prediction.5 = predict(RF.5,cv.test,type="prob")[,2]
        perf.prediction = prediction(prediction.5, cv.test$Purchase)
        pauc.20 = round(performance(perf.prediction, measure = "auc",fpr.stop=0.2)@y.values[[1]], 3)
        AUC.RF.mtry[i,count] = pauc.20
        count = count + 1
    }
  }
print(AUC.RF.mtry) 
mean.RF.mtry = apply(AUC.RF.mtry,2,mean)
sd.RF.mtry = apply(AUC.RF.mtry,2,sd)
error.RF.mtry= sapply(sd.RF.mtry,calc_error)
low.interval <- mean.RF.mtry - error.RF.mtry
high.interval <- mean.RF.mtry + error.RF.mtry
# RF.mtry.table <- data.frame("mtry"=mtrys,"mean"=mean.decTree,"sd"=sd.decTree,
# "low interval"=low.interval,"high interval"=high.interval)
# print(mtry.RF.table)
message("best mtry = 12")

################## 2.c regularized logistic regression ###################
x <- model.matrix(Purchase ~ ., data = caravan.train)
y <- data.matrix(caravan.train$Purchase=="Yes")    
x.test <- model.matrix(Purchase ~ ., data = caravan.test)
y.test <- data.matrix(caravan.test$Purchase)

f<-numeric() # folds id
e<-numeric() # examples id
for(i in 1:10) {
  #print(length(cv.index[i,][cv.index[i,] > 0]))
  f <- c(f, rep(i,ncol(cv.index)))
  e <- c(e, cv.index[i,])
}
tmp <- data.frame(f,e)
tmp <- tmp[ which(tmp$e !=0), ]
tmp <- tmp[order(tmp$e),]
foldid <- tmp$f

alphas = seq(0,1,0.05)
pauc.20 = numeric(length(length(alphas)))
lambdas = numeric(length(length(alphas)))
count=1
for(alpha in alphas){
    fit.ridge.cv <- cv.glmnet(x, y,
                          foldid = foldid,
                          family = "gaussian",
                          type.measure = "mse",
                          alpha = alpha,
                          lambda = grid)
    # prediction
    lambda.best <- fit.ridge.cv$lambda.min
    #message("best lambda=", lambda.best)
    pred.ridge.cv <- predict(fit.ridge.cv, newx = x.test, s = lambda.best)
    prediction <- prediction(pred.ridge.cv, y.test)
    pauc.20[count]=round(performance(prediction, measure = "auc",fpr.stop=0.2)@y.values[[1]], 3)
    lambdas[count]= lambda.best
    count = count+1
}
print(cbind(pauc.20,lambdas))
message("best lambda=0.1, best alpha=0.084")

########### 2.d ##################
decTree = rpart(Purchase ~ .,caravan.test,cp=0.0175)
prediction.test <- predict(decTree, caravan.test,type="prob")[,2]
perf.prediction = prediction(prediction.test, caravan.test$Purchase)
pauc.20 = round(performance(perf.prediction, measure = "auc",fpr.stop=0.2)@y.values[[1]], 3)

plot(performance(perf.prediction, measure='tpr', x.measure='fpr'), main=paste('DC AUC.20 = ', pauc.20))


RF = randomForest(Purchase ~ ., caravan.train,ntree=500,mtry=12)
prediction = predict(RF,caravan.test,type="prob")[,2]
perf.prediction = prediction(prediction, caravan.test$Purchase)


pauc.20 = round(performance(perf.prediction,
    measure = "auc",fpr.stop=0.2)@y.values[[1]], 3)

plot(performance(perf.prediction, measure='tpr', x.measure='fpr'),
    main=paste('RF AUC.20 = ', pauc.20))

reg_log =  cv.glmnet(x, y,family = "gaussian", type.measure = "mse",alpha = 0.084,
    lambda = 0.1)
prediction <- predict(fit.ridge.cv, newx = x.test, s = lambda.best)
perf.prediction <- prediction(prediction, y.test)
pauc.20 = round(performance(perf.prediction,
    measure = "auc",fpr.stop=0.2)@y.values[[1]], 3)

plot(performance(perf.prediction, measure='tpr', x.measure='fpr'),
    main=paste('regularised logistic regression AUC.20 = ', pauc.20))

########### Part 3 ###################
variable.importance = RF$importance[order(-RF$importance),]
print(length(variable.importance))
cutoff.step = 5
cutoff.seq = seq(5, length(variable.importance), by= cutoff.step)
n = length(cutoff.seq)

pauc.20.test = numeric(n)
counter = 1

for(i in cutoff.seq){
    names = c(names(variable.importance[1:i]),"Purchase")
    RF.v = randomForest(Purchase ~ ., caravan.train[names], ntree = 100)
    prediction.test = predict(RF.v,caravan.test,type="prob")[,2]
    perf.prediction = prediction(prediction.test, caravan.test$Purchase)
    pauc.20 = round(performance(perf.prediction, measure = "auc",fpr.stop=0.2)@y.values[[1]], 3)
    pauc.20.test[counter]=pauc.20
    counter = counter+1
}
plot(cutoff.seq,pauc.20.test,main="Variable importance according to RF")
#keep only the first five ones

library(rpart.plot)
rpart.plot(decTree,roundint=FALSE)
#same as RF


############ Part 4 ###############
decTree = rpart(Purchase ~ .,caravan.test,cp=0.0175)

blind.T = read.table("caravan.test.1000.csv")

predict = predict(decTree, caravan.test,type="prob")[,2]


sample <- sample(nrow(caravan.train), nrow(caravan.train)*0.9, replace = FALSE)


train <- Caravan[sample, ]
test <- Caravan[-sample, ]
test.X <- caravan.test[,-86]
test.Y <- caravan.test$Purchase

pred <- prediction(predict, test.Y)
perf <- performance(pred, measure = "tpr", x.measure = "fpr", fpr.stop = 0.2)
cutoffs <- data.frame(cut=perf@alpha.values[[1]], fpr=perf@x.values[[1]], tpr=perf@y.values[[1]])

cutoffs <- data.frame(cut=perf@alpha.values[[1]], fpr=perf@x.values[[1]], tpr=perf@y.values[[1]])
cutoff <- 0.0

for(cut in cutoffs$cut){
  if(cut != Inf && cut < 1 && cut > 0){
    pred <- factor(ifelse(predict > cut, 'Yes', 'No'))
    if(sum(pred == 'Yes') >= 100){
      cutoff <- cut
      break
    }
  }
}
message("cutoff = ",cutoff)




pred <- ifelse(predict > cutoff, 1, 0)

which(pred == 1)

write(pred, "T.prediction.txt", sep = "\n")


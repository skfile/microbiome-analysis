#DO
#LASSO TIME - SEE https://www.r-bloggers.com/ridge-regression-and-the-lasso/
x <- model.matrix(ASD~ ., nnormdata)
y <- nnormdata$ASD
lambda <- 10^seq(10, -2, length = 100)

#create test and training sets
library(glmnet)

set.seed(489)
train = sample(1:nrow(x), nrow(x)/2)
test = (-train)
ytest = y[test]
#OLS

#Linear and Logistic Modeling!!
nndatalm <- lm(ASD~., data = nnormdata)
nndataglm <- glm(ASD~., data = nnormdata)
summary(nndataglm)
library(aod)
wald.test(b = coef(nndataglm), Sigma = vcov(nndataglm), Terms = 1)
library(gam)
nndatagam <- gam(ASD~., data = nnormdata)
summary(nndatagam)
# write.csv(nndatagam$effects, file = "gamplotforeffects")

#McFadden R squared for logsistic for model fit value
library(pscl)
pR2(nndataglm)
#Tests for deviance within the logistic model
anova(nndataglm, test = "Chisq")

#False Positive on glm
library(ROCR)
p <- predict(nndataglm, nnormdata, type="response")
pr <- prediction(p, nnormdata$ASD)
prf <- performance(pr, measure = "tpr", x.measure = "fpr")
plot(prf)

auc <- performance(pr, measure = "auc")
auc <- auc@y.values[[1]]
auc

plot(ridge.mod$beta, text(x,y))



anovanndata <- anova(nndataglm, test = "Chisq")
coef(nnormdata) 

#ridge
ridge.mod <- glmnet(x, y, alpha = 0, lambda = lambda)
predict(ridge.mod, s = 0, exact = T, type = 'coefficients')[1:6,]

nndatalm <- lm(ASD~., data = nndata, subset = train)
ridge.mod <- glmnet(x[train,], y[train], alpha = 0, lambda = lambda)
#find the best lambda from our list via cross-validation
cv.out <- cv.glmnet(x[train,], y[train], alpha = 0)

bestlam <- cv.out$lambda.min

#make predictions
ridge.pred <- predict(ridge.mod, s = bestlam, newx = x[test,])
s.pred <- predict(nndatalm, newdata = nnormdata[test,])
#check MSE
mean((s.pred-ytest)^2)

#a look at the coefficients
out = glmnet(x[train,],y[train],alpha = 0)
predict(ridge.mod, type = "coefficients", s = bestlam)

lasso.mod <- glmnet(x[train,], y[train], alpha = 1, lambda = lambda)
lasso.pred <- predict(lasso.mod, s = bestlam, newx = x[test,])
mean((lasso.pred-ytest)^2)
#output of about 0.02 which means their is a high likelyhood that they are notable predictors!
lasso.coef  <- predict(lasso.mod, type = 'coefficients', s = bestlam) #need to open/download

#randomForest
library(randomForest)
nnormdata.rf<- randomForest(ASD~ ., nnormdata)

#Principal Component Analysis:
pcannormdata <- prcomp(nnormdata)
biplot(pcannormdata)

#REAL way to plot lasso given data:
plot(lasso.mod, "norm", label = TRUE)

#matplot(lasso.coef)

#Clustering
library(cluster)
distnnormdata <- dist(nnormdata)
hnnormdata <- hclust(distnormndata)
library(mclust)
mnnormdata <- Mclust(distnnormdata)
library(e1071)
bnnormdata <- bclust(nnormdata)

#e1071: Let the vectors support your model

#kernLab: kernel trick packaged well

#FOR CNTREE
library(e1071)
ctreennormdata <- ctree(ASD~ .,nnormdata)

#Getting started with Naive Bayes
#MAYBE?

#STUFF TO LOOK AT:

pcannormdata
biplot(pcannormdata)
plot(nndatalm)
plot(nndataglm)
plot(lasso.mod, "norm", label = TRUE)
plot(ridge.mod, "norm", label = TRUE)
plot(mnnormdata)
plot(bnnormdata)
plot(ctreennormdata)
anovanndata
#writecsv for nnormdata.rf

#correlation script
library(ltm)
biserial.cor(x,y, level=2)
cor.test(x,y)


library(caret)
set.seed(7)
fit.lda <- train(ASD~., data=nnormdata, method="lda")
# b) nonlinear algorithms
# CART
set.seed(7)
fit.cart <- train(ASD~., data=nnormdata, method="rpart")
# kNN
set.seed(7)
fit.knn <- train(ASD~., data=nnormdata, method="knn")
# c) advanced algorithms
# SVM
set.seed(7)
fit.svm <- train(ASD~., data=nnormdata, method="svmRadial")
# Random Forest
set.seed(7)
fit.rf <- train(ASD~., data=nnormdata, method="rf")

results <- resamples(list(cart=fit.cart, knn=fit.knn, svm=fit.svm, rf=fit.rf))
summary(results)

dotplot(results)

library(pvclust)
#can be used to create pvalues hieracl clustering


write.csv(anova(nndatalm), file = "vlinearmodelpvalues")
write.csv(anova(nndataglm), file = "vlogisticmodelpvalues")
write.csv(anova(nndatagam), file = "vgammodelpvalues")

std_residlm  <- rstandard(nndatalm)
cooks_dlm <- cooks.distance(nndatalm)
hat_valueslm <- hatvalues(nndatalm)
plot(hat_valueslm, std_residlm, cex = 10*sqrt(cooks_dlm))

std_residglm  <- rstandard(nndataglm)
cooks_dglm <- cooks.distance(nndataglm)
hat_valuesglm <- hatvalues(nndataglm)
plot(hat_valuesglm, std_residglm, cex = 10*sqrt(cooks_dglm))

std_residgam  <- rstandard(nndatagam)
cooks_dgam <- cooks.distance(nndatagam)
hat_valuesgam <- hatvalues(nndatagam)
plot(hat_valuesgam, std_residlm, cex = 10*sqrt(cooks_dgam))



#library(randomForest)
#varImpPlot(nnormdata.rf)

library(relaimpo)
relimplm <- calc.relimp(nndatalm)
#also can use : calc.relimp(nndatalm, type=c("lmg","last"...), rela = TRUE)

#also a bootsrap method available

#also please use for glm (general linear model)

#for asd only and nonasd only
library(Vegan)
diversity <- divesity(nnormdata, type = simpson)

#randyplot
library(party)
cf <- cforest(ASD ~ ., data=nnormdata)

pt <- prettytree(cf@ensemble[[1]], names(cf@data@get("input"))) 
nt <- new("BinaryTree") 
nt@tree <- pt 
nt@data <- cf@data 
nt@responses <- cf@responses 

plot(nt, type="simple")

cor(nnormdata,use="complete.obs", method="kendall") 
cov(nnormdata, use="complete.obs")

# Correlations with significance levels
library(Hmisc)
rcorr(x, type="pearson") # type can be pearson or spearman

#mtcars is a data frame 
rcorr(as.matrix(nnormdata))

options(max.print = 1000000)
summarynndata <- summary(nndataglm)
capture.output(summarynndata, file = "myfile.txt")

chisanova <- anova(nndataglm, test="Chisq")


write.csv(importance(nnormdata.rf), file = "vrfimportance")

#http://topepo.github.io/caret/train-models-by-tag.html#binary-predictors-only
#http://thestatsgeek.com/2014/02/08/r-squared-in-logistic-regression/
#https://machinelearningmastery.com/machine-learning-in-r-step-by-step/

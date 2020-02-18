#------------------
# Data Preparation
#------------------
library(data.table)
library(ggplot2)
library(plotly)
#Read datasets
#Download the data from http://www.saedsayad.com/datasets/BikeRental.zip
train <- read.csv("data/BikeRental/bike_rental_train.csv")
test <- read.csv("data/BikeRental/bike_rental_test.csv")

#Rows and Cols
dim(train)
dim(test)

#Columns name
colnames(train)
colnames(test)

#Show  
head(train)
head(test)


#Rows and Cols
dim(train)
dim(test)

#Columns name
colnames(train)
colnames(test)

#Show  
head(train)
head(test)

#Scatter plot
pairs(~temp+humidity+windspeed+bike_rent_count, data=head(train,100), main="Scatterplot - train", col="darkgreen")
pairs(~temp+humidity+windspeed+bike_rent_count, data=head(test,100), main="Scatterplot - test", col="brown")

#--------------------------------------
# GLM
#--------------------------------------

library(glmnet)
library(dplyr)
benchmark_glmnet = cv.glmnet(train%>%select(-bike_rent_count)%>%as.matrix(),train$bike_rent_count)
pred.benchmark = predict(benchmark_glmnet,test%>%select(-bike_rent_count)%>%as.matrix())
pred.benchmark = pred.benchmark[,1]
err.benchmark <- test$bike_rent_count - pred.benchmark 
rmse.benchmark <- sqrt(mean((err.benchmark^2)))
#--------------------------------------
# Support Vector Machines - Regression
#--------------------------------------
library(e1071)


#Train
model.SVR <- svm(bike_rent_count~., train)
summary(model.SVR)

#Residual plot
res.SVR = train$bike_rent_count-predict(model.SVR, newdata=train)
plot(train$temp, res.SVR, ylab="Residuals", xlab="Temperature", main="Residual Plot") 
abline(0, 0)

#Q-Q plot
stdres.SVR = scale(res.SVR)
qqnorm(stdres.SVR, ylab="Standardized Residuals", xlab="Normal Scores", main="QQ Plot") 
qqline(stdres.SVR)

#Test
pred.SVR <- predict(model.SVR, newdata=test)
err.SVR <- test$bike_rent_count - pred.SVR 
rmse.SVR <- sqrt(mean((err.SVR^2)))
rmse.SVR
rmse.benchmark

#Errors histogram
hist(train$bike_rent_count, main="bike_rent_count", sub="values", xlab="Number", breaks=10, col="darkred")
hist(err.SVR, main="bike_rent_count", sub="(Actual-Predicted)", xlab="Error", breaks=10, col="darkred")

#--------------------------------------
# Comparaison divers noyaux et params
#--------------------------------------
run_svm = function(kernel, degree=3,gamma=1){
  model.SVR <- svm(bike_rent_count~., train,kernel=kernel,degree=degree,gamma=gamma)
  pred.SVR <- predict(model.SVR, newdata=test)
  err.SVR <- test$bike_rent_count - pred.SVR 
  sqrt(mean((err.SVR^2)))
}
run_svm('radial')
# 154
vals = 1:10
fine_tune_rbf = pbapply::pblapply(vals,function(x){
  gamma = 5^(x-5)
  kernel = 'radial'
  time <- system.time(perf <- run_svm(kernel,gamma = gamma))
  res = data.table(time = time[3], error = perf, gamma = gamma, kernel = kernel)
  print(res)
  res
})
fine_tune_rbf = rbindlist(fine_tune_rbf)
fine_tune_rbf$id = vals
g <- ggplot(fine_tune_rbf,aes(x=gamma^2,y=time,fill=error))+xlab("gamma")+geom_col()+scale_x_continuous(trans="log10")
ggplotly(g)


run_svm('linear')
#160
run_svm('polynomial',2)
run_svm('polynomial',3)
run_svm('polynomial',4)
run_svm('polynomial',5)


#--------------------------------------
# Support Vector Machines - Regression
#--------------------------------------

library(gbm)
# On remarquera qu'on peut utiliser le paramètre "var.monotone"
gbm = gbm(bike_rent_count ~ .,
          # var.monotone=c(1,-1,1),
          data=train,shrinkage =.01,
          n.trees = 500,
          n.minobsinnode = 10,
          interaction.depth = 4,
          bag.fraction = .6,
          train.fraction = .7,
          verbose = T,
          distribution = "poisson")
gbm.perf(gbm)
# On peut immédiatement examiner les dépendances partielles
plot(gbm,"temp")
plot(gbm,"humidity")
plot(gbm,"windspeed")

# Faisons un gridsearch sur la profondeur et la taille des feuilles
grid = expand.grid(minobs = c(10,20,50,100,200),
                   depth = c(1,2,3,4))
perf = pbapply::pbapply(grid,1,function(x){
  time = system.time(gbm <- gbm(bike_rent_count ~ .,
            var.monotone=c(1,-1,1),cv.folds=2,
            data=train,shrinkage =.01,
            n.trees = 1000,
            n.minobsinnode = x['minobs'],
            interaction.depth = x['depth'],
            bag.fraction = .6,
            train.fraction = .7,verbose = F,
            distribution = "poisson"))
  best_iter = gbm.perf(gbm, method = "cv")
  pred.gbm = predict(gbm,newdata = test,type="response")
  err.gbm <- test$bike_rent_count - pred.gbm 
  rmse = sqrt(mean((err.gbm^2)))
  data.table(error = rmse, minobs=x['minobs'],depth=x['depth'],time=time[3],best_iter=best_iter)
})

perf = rbindlist(perf)

g <- ggplot(data=perf,aes(x=depth,size=log(minobs),y=error,color=time))+
  geom_point()
ggplotly(g)

# Bilan : GBM > SVM > GLMNET

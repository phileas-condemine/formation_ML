library(data.table)
library(glmnet)

data = fread("data/prepared/dataset.csv")
# train_id = sample(nrow(data),3E+4)
train_id = which(year(data$date_entree)<2018)

# data[,nb:=nb-nb_lag1]
#affluence moyenne de l'établissement sur la période d'apprentissage


data = data[!is.na(nb)]
data = data[!is.na(nb_lag1)]
data[,finess_avg := mean(nb[year(date_entree)<2018],na.rm=T),by="finess"]

data[,date_entree:=NULL]
data[,finess:=NULL]
data[,longitude:=NULL]
data[,latitude:=NULL]
data[,ninsee_nearest:=NULL]



for (var in names(data)){
  print(var)
  setnames(data,var,"var")
  # avg = mean(data$var,na.rm=T)
  # data[is.na(var),var:=avg]
  data[is.na(var),var:=0]
  setnames(data,"var",var)
}
dim(na.omit(data))


names(data)[sapply(data,function(x)sum(is.nan(x)))>0]
names(data)[sapply(data,function(x)sum(is.infinite(x)))>0]
names(data)[sapply(data,function(x)sum(is.na(x)))>0]

train = data[train_id]
test = data[-train_id]
summary(train$nb_lag1)

ts_vars = c("nb_lag1","nb_lag2","nb_lag7","nb_lag30")
days = c("lundi","mardi","mercredi","jeudi","vendredi","samedi","dimanche")


vars_to_investigate = setdiff(names(data),c("nb",days,ts_vars,"finess_avg"))
one_var = sample(vars_to_investigate,1)

TEST AVEC BIGAM/BIGGAM ?

var_per_var = pbapply::pblapply(vars_to_investigate,function(one_var){
  dtrain = as.matrix(train[,c(days,"finess_avg",one_var),with=F])
  dtest = as.matrix(test[,c(days,"finess_avg",one_var),with=F])
  
  model = glmnet(x=dtrain,y = train$nb,
                 family = "poisson"
                 # ,offset=log(train$nb_lag1)
                 )
  predictions = predict.glmnet(model,newx = dtest
                               # ,newoffset = log(test$nb_lag1)
                               )
  
  perfs = apply(predictions,2,function(pred){
    MLmetrics::NormalizedGini(pred,test$nb)
  })
  perf = max(perfs)
  
  coefs = coef.glmnet(model)
  coefs = as.matrix(coefs)
  best_iter = which.max(perfs)
  best_coefs = coefs[,best_iter]
  data.table(var =one_var, coef = best_coefs[[one_var]],rank = best_iter/ncol(coefs),var_mean = mean(test[[one_var]]),var_sd = sd(test[[one_var]]),perf = perf)
})
var_per_var = rbindlist(var_per_var)

var_per_var[,mean_effect := var_sd * coef]
setorder(var_per_var,-perf)
var_per_var

library(data.table)
library(xgboost)
data = fread("data/prepared/dataset.csv")
setnames(data,"date_entree","date")
# train_id = sample(nrow(data),3E+4)
data = data[
  data[,.(non_zero=sum(nb>0),avg = mean(nb),supseuil = sum(nb>=5)),
       by=c("finess","categorie_age","mode_sortie")][
         supseuil>200,.(finess,categorie_age,mode_sortie)],
  on=c("finess","categorie_age","mode_sortie")]

# data[,nb:=nb-nb_lag1]
data = data[!is.na(nb)]
# affluence moyenne de l'établissement sur la période d'apprentissage
data[,finess_avg := mean(nb[year(date)<2018],na.rm=T),by="finess"]
train_id = which(year(data$date)<2018)

data[,categorie_age:=ifelse(categorie_age=="[0,3]",1,ifelse(categorie_age=="(3,60]",2,3))]
table(data$categorie_age)
data[,mode_sortie:=ifelse(mode_sortie=="rad",1,2)]
table(data$mode_sortie)
data[,date := as.Date(date,"%Y-%m-%d")]
# data[,date:=NULL]
# data[,finess:=NULL]
data[,longitude:=NULL]
data[,latitude:=NULL]
data[,ninsee_nearest:=NULL]
dates= data$date
data = data[,lapply(.SD,as.numeric),.SDcols=setdiff(names(data),"date")]
data$date = dates
train = data[train_id]
test = data[-train_id]




norm_gini <- function(preds, dtrain) {
  labels <- getinfo(dtrain, "label")
  err <- MLmetrics::NormalizedGini(preds,labels)
  return(list(metric = "gini", value = err))
}



run_xgb = function(train_,test_){
  watchlist = list(train=train_,test=test_)
  param <- list(
    # objective = "reg:squarederror",
    objective= "reg:linear",
    # objective = "count:poisson",
    max.depth = 4, subsample =.5,colsample_bytree=1,
    eta = .025, nthread = 3)
  bst <<- xgb.train(param,train_, watchlist = watchlist, feval = norm_gini,
                    maximize = T, early_stopping_rounds = 100,
                    nrounds = 1000, print_every_n = 10L)
  pred = predict(bst,newdata = test_)
  return(MLmetrics::NormalizedGini(pred,getinfo(test_, "label")))
}
ts_vars = c("nb_lag1","nb_lag2","nb_lag7","nb_lag30")
days = c("lundi","mardi","mercredi","jeudi","vendredi","samedi","dimanche")


################################################
##### MODELISATION DU COMPTAGE 
# benchmark AR
train_ = xgb.DMatrix(data = as.matrix(train[,c(ts_vars,days),with=F]),label=train$nb)
test_ = xgb.DMatrix(data = as.matrix(test[,c(ts_vars,days),with=F]),label=test$nb)
benchmark_perf = run_xgb(train_,test_)

# les variables d'AR n'ont pas d'effet causal elles capturent un effet indû qu'il vaudrait mieux expliquer par pollution ou meteo...
#wo time series vars
table(train$nb)
train_ = xgb.DMatrix(data = as.matrix(train[,-c("nb","mode_sortie","date","finess",ts_vars),with=F]),label=train$nb)
test_ = xgb.DMatrix(data = as.matrix(test[,-c("nb","mode_sortie","date","finess",ts_vars),with=F]),label=test$nb)
external_perf = run_xgb(train_,test_)
xgb.importance(model = bst)




#w all vars : ts + external
train_ = gb.DMatrix(data = as.matrix(train[,-c("nb","date"),with=F]),label=train$nb)
test_ = xgb.DMatrix(data = as.matrix(test[,-c("nb","date"),with=F]),label=test$nb)
all_vars_perf = run_xgb(train_,test_)

benchmark_perf
external_perf
all_vars_perf
xgb.importance(model = bst)


################################################
##### MODELISATION EN PROPORTION DU VOLUME ANNUEL DE L'ETABLISSEMENT 
rm(my_motifsortie)
lapply(c("rad","hospi","tout"),function(my_motifsortie){
  # print(my_motifsortie)
  train = data[train_id]
  test = data[-train_id]
  train = train[!is.na(finess_avg)&finess_avg>0]
  # uniqueN(train[,c("categorie_age","date","finess"),with=F])
  test = test[!is.na(finess_avg)&finess_avg>0]
  table(train$mode_sortie)
  # my_motifsortie = "rad" # ou hospi ou tout
  if(my_motifsortie == "rad"){
    print("rad")
    train = train[mode_sortie==1]
    # uniqueN(train[,c("categorie_age","date","finess"),with=F])
    test = test[mode_sortie==1]
  } else if(my_motifsortie == "hospi"){
    print("hospi")
    train = train[mode_sortie==2]
    # uniqueN(train[,c("categorie_age","date","finess"),with=F])
    test = test[mode_sortie==2]
  } else if(my_motifsortie == "tout"){
    print("tout")
    train = train[,c(.SD[1],nb2=sum(nb)),by=c("categorie_age","date","finess")]
    # uniqueN(train[,c("categorie_age","date","finess"),with=F])
    train[,nb:=NULL]
    setnames(train,"nb2","nb")
    test = test[,c(.SD[1],nb2=sum(nb)),by=c("categorie_age","date","finess")]
    test[,nb:=NULL]
    setnames(test,"nb2","nb")
  }
  
  # les variables d'AR n'ont pas d'effet causal elles capturent un effet indû qu'il vaudrait mieux expliquer par pollution ou meteo...
  #wo time series vars
  vars_explicatives = setdiff(names(train),c("nb",ts_vars,"finess_avg","date"))
  
  train_ = xgb.DMatrix(data = as.matrix(train[,vars_explicatives,with=F]),label=train$nb/train$finess_avg)
  test_ = xgb.DMatrix(data = as.matrix(test[,vars_explicatives,with=F]),label=test$nb/test$finess_avg)
  external_perf = run_xgb(train_,test_)
  xgb.importance(model = bst)
  save(vars_explicatives,file="data/varexp.RData")
  print(paste("save xgb",my_motifsortie))
  xgb.save(bst,fname = sprintf("data/prepared/model_%s.xgb",my_motifsortie))
  test$pred = predict(bst,test_) * test$finess_avg
  print(paste("save test",my_motifsortie))
  save(test,file = sprintf("data/prepared/test_%s.RData",my_motifsortie))
  test[,pred:=NULL]
})

# benchmark AR
train_ = xgb.DMatrix(data = as.matrix(train[,c(ts_vars,days),with=F]),label=train$nb/train$finess_avg)
test_ = xgb.DMatrix(data = as.matrix(test[,c(ts_vars,days),with=F]),label=test$nb/test$finess_avg)
benchmark_perf = run_xgb(train_,test_)


#w all vars : ts + external
train_ = xgb.DMatrix(data = as.matrix(train[,-c("nb","finess_avg","date","finess"),with=F]),label=train$nb/train$finess_avg)
test_ = xgb.DMatrix(data = as.matrix(test[,-c("nb","finess_avg","date","finess"),with=F]),label=test$nb/test$finess_avg)
all_vars_perf = run_xgb(train_,test_)

benchmark_perf
external_perf
all_vars_perf
xgb.importance(model = bst)


################################################
##### MODELISATION DU DEPASSEMENT PAR RAPPORT A LA SEMAINE PRECEDENTE

train = data[train_id]
test = data[-train_id]
train = train[!is.na(nb_lag7)]
test = test[!is.na(nb_lag7)]

# benchmark AR
train_ = xgb.DMatrix(data = as.matrix(train[,c("nb_lag1","nb_lag2"),with=F]),label=train$nb>train$nb_lag7)
test_ = xgb.DMatrix(data = as.matrix(test[,c("nb_lag1","nb_lag2"),with=F]),label=test$nb>test$nb_lag7)
benchmark_perf = run_xgb(train_,test_)

# les variables d'AR n'ont pas d'effet causal elles capturent un effet indû qu'il vaudrait mieux expliquer par pollution ou meteo...
#wo time series vars
train_ = xgb.DMatrix(data = as.matrix(train[,-c("nb",ts_vars,"finess_avg"),with=F]),label=train$nb>train$nb_lag7)
test_ = xgb.DMatrix(data = as.matrix(test[,-c("nb",ts_vars,"finess_avg"),with=F]),label=test$nb>test$nb_lag7)
external_perf = run_xgb(train_,test_)
xgb.importance(model = bst)


#w all vars : ts + external
train_ = xgb.DMatrix(data = as.matrix(train[,-c("nb","nb_lag7"),with=F]),label=train$nb>train$nb_lag7)
test_ = xgb.DMatrix(data = as.matrix(test[,-c("nb","nb_lag7"),with=F]),label=test$nb>test$nb_lag7)
all_vars_perf = run_xgb(train_,test_)


benchmark_perf
external_perf
all_vars_perf
xgb.importance(model = bst)

library(data.table)
data = fread("data/prepared/dataset.csv")
# train_id = sample(nrow(data),3E+4)
train_id = which(year(data$date_entree)<2018)

# data[,nb:=nb-nb_lag1]
data = data[!is.na(nb)]
#affluence moyenne de l'établissement sur la période d'apprentissage
data[,finess_avg := mean(nb[year(date_entree)<2018]),by="finess"]

data[,date_entree:=NULL]
data[,finess:=NULL]
data[,longitude:=NULL]
data[,latitude:=NULL]
data[,ninsee_nearest:=NULL]

data = data[,lapply(.SD,as.numeric)]

train = data[train_id]
test = data[-train_id]

library(kernlab)
model = kernlab::ksvm(x = x.train_,y=y.train_)



ts_vars = c("nb_lag1","nb_lag2","nb_lag7","nb_lag30")
days = c("lundi","mardi","mercredi","jeudi","vendredi","samedi","dimanche")

# benchmark AR
x.train_ = as.matrix(train[,c(ts_vars,days),with=F])
y.train_ = train$nb
x.test_ = as.matrix(test[,c(ts_vars,days),with=F])
y.test_ = test$nb

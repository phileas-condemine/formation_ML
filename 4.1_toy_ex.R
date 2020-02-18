# rbfdot Radial Basis kernel "Gaussian"
# polydot Polynomial kernel
# vanilladot Linear kernel
# tanhdot Hyperbolic tangent kernel
# laplacedot Laplacian kernel
# besseldot Bessel kernel
# anovadot ANOVA RBF kernel
# splinedot Spline kernel
# stringdot String kernel

library(data.table)
library(ggplot2)
library(plotly)
library(kernlab)
library(dplyr)
names(iris)
data = iris
nm = names(data)
nm = setdiff(nm,'Species')
x = "Sepal.Width"#sample(nm,1)
y = "Petal.Length"#sample(setdiff(nm,x),1)
data = data[data$Species%in%c("virginica","versicolor"),]
train_id = sample(nrow(data),60)
data$Species = droplevels(data$Species)
# data$Petal.Length <- data$Petal.Length^2
# data = data[,c(x,y,"Species")]
ggplot(data=data,aes_string(x=y,y=x,shape="Species",color="Species",size=3))+geom_point()
train = data[train_id,] %>% mutate_if(is.numeric,scale)
test = data[-train_id,] %>% mutate_if(is.numeric,scale)


# degre 1
svc = ksvm(Species ~ ., data = train, kernel="vanilladot")
# les vecteurs support sont les points pleins, les autres sont détourés seulement
slice_at = c("Petal.Width"=mean(train$Petal.Width),
             "Sepal.Length"=mean(train$Sepal.Length))
plot(svc, data = train,slice=slice_at)
table(predict(svc,test))
MLmetrics::ConfusionMatrix(predict(svc,test),test$Species)
# cf Modélisation prédictive et apprentissage statistique avec R par Stéphane TUFFERY 
svc@nSV
coef = colSums(svc@coef[[1]] * train[svc@alphaindex[[1]],setdiff(names(train),"Species")])

# degre 2 => il faut paramétrer la constante et l'échelle.
grid = expand.grid(scale=exp(seq(-10,10,1)),offset=exp(seq(-10,10,1)))
one_svm = function(scale,offset){
svc = ksvm(Species ~ ., data = train, kernel="polydot"
           ,kpar=list(degree=2,scale=scale,offset=offset)
           # ,kpar="automatic"
           )
plot(svc, data = train,slice=slice_at)
table(predict(svc,test))
perf = MLmetrics::F1_Score(predict(svc,test),test$Species)
data.table(perf = perf, offset=offset,scale=scale)
}
perfs = pbapply::pbapply(grid,1,function(x){
  one_svm(x['scale'],x['offset'])
})

perfs = rbindlist(perfs)
# save(perfs,file="data/BikeRental/output/svm_poly2_optim.RData")
ggplot(perfs,aes(x=scale,y=offset,fill=perf))+geom_tile()+scale_x_continuous(trans="log")+scale_y_continuous(trans="log")





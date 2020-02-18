library(data.table)
library(rpart)
library(rpart.plot)
library(ggplot2)
x1 = sample(1:100,size=10000,replace=T)/100
x2 = sample(0:1,size=10000,replace=T)
y= x1 + x2
data = data.table(x1=x1,x2=x2,y=y)
tree = rpart(data = data,y ~ x1+x2,control = rpart.control(cp=0.001))
rpart.plot(tree)

data$pred = predict(tree)
ggplot(data=unique(data[x2==0,c("pred","x1")]),aes(x=x1,y=pred))+
  geom_line()+geom_abline(slope=1,intercept=0)

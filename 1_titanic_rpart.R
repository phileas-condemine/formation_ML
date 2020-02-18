library(data.table)
library(rpart)
library(rpart.plot)
# https://web.stanford.edu/class/archive/cs/cs109/cs109.1166/problem12.html
# download.file("https://web.stanford.edu/class/archive/cs/cs109/cs109.1166/stuff/titanic.csv",destfile = "data/titanic.csv",mode = "wb")
titanic = fread("data/titanic.csv")
titanic[,Name:=NULL]
sapply(titanic, function(x)sum(is.na(x)))
n = nrow(titanic)
titanic = titanic[sample(n,.8*n)]#sampling
tree = rpart(data = titanic,Survived ~ .)
# rpart.plot(tree)
print(tree)
jpeg(filename="rpartplot.jpeg",width = 1000,height = 1000)
# svg("rpartplot.svg")
rpart.plot(tree)
dev.off() 

library(tree)
tree2D = tree(data = titanic,Survived ~ Age + Fare,
              control=tree.control(mindev=1E-3,minsize = 20,nobs=nrow(titanic)))
jpeg(filename="partition.jpeg",width = 1000,height = 1000)
# svg("partition.svg")
plot(titanic$Age,titanic$Fare, type="n",
     xlab="Age", ylab="Prix",ylim = c(0,100))
text(titanic$Age, titanic$Fare, c("M", "S")[titanic$Survived+1],cex = 1.5)
partition.tree(tree2D, add = TRUE, cex = 2)
dev.off() 

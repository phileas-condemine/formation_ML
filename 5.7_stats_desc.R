library(data.table)
library(ggplot2)
library(plotly)

data = fread("data/prepared/dataset.csv")
data = data[!is.na(nb)]
#affluence moyenne de l'établissement sur la période d'apprentissage
data[,finess_avg := mean(nb[year(date_entree)<2018]),by="finess"]

compare2D = function(data,var){
  setnames(data,var,"var")
  data[,var_finess_avg := mean(var[year(date_entree)<2018],na.rm=T),by="finess"]
  
  # polluant > moyenne vs flux urgences > moyenne
  g <- ggplot(data = data[sample(nrow(rpu),1000)],
         aes(y=nb/finess_avg,x=var/var_finess_avg))+
    geom_point()+geom_smooth(method="lm")+xlab(var)+ylab("dépassement flux urgences")
  setnames(data,"var",var)
  g
}

compare2D(data,"no2_avg_7J")

names(data)

library(data.table)
library(R.utils)
library(ggplot2)
library(plotly)

#### DOWNLOAD ####
DL=F

if (DL){
  dates = expand.grid(V1=2016:2018,V2=1:12)
  dates = dates$V1*100 + dates$V2
  date = sample(dates,1)
  for (date in dates[5:length(dates)]){
    print(date)
    file = sprintf("data/weather/synop_%s.csv.gz",date)
    download.file(sprintf("https://donneespubliques.meteofrance.fr/donnees_libres/Txt/Synop/Archive/synop.%s.csv.gz",
                          date),destfile = file)
    gunzip(file)
  }
}

#### PREPARE ####
weather = pbapply::pblapply(list.files("data/weather/"),function(file){
  fread(paste0("data/weather/",file),colClasses = "character")
})
weather = rbindlist(weather)
weather[,day:=substr(date,1,8)]
weather[,day:=as.Date(day,"%Y%m%d")]
weather[,hour:=substr(date,9,10)]


weatherOrly = weather[numer_sta == "07149"]
table(weatherOrly$hour)
# https://donneespubliques.meteofrance.fr/%3Ffond%3Dproduit%26id_produit%3D90%26id_rubrique%3D32
# https://www.wmo.int/pages/prog/www/WMOCodes/WMO306_vI1/Publications/2016update/WMO306_vI1_en_2011UP2016.pdf

############## CHECK CORR  #################
load("data/prepared/aggr_rpu.RData")

stat_univ = function(isnum,var){
  weatherOrly = weather[numer_sta == "07149"]
  setnames(weatherOrly,var,"var")
  if(isnum){
    suppressWarnings(weatherOrly[,var:=as.numeric(var)])
    weatherOrly[,var:=as.numeric(Hmisc::cut2(var,g = 10))]
  }else{
    weatherOrly[,var:=as.numeric(var)]
  }
  stats = merge(
    weatherOrly[,.(day,var)],
    agg_rpu,by.x="day",by.y="date_entree"
  )[,.(somme = sum(nb),n_rows = .N,avg = mean(nb),se = sd(nb)),by="var"]
  ggplot(data=na.omit(stats))+aes(x=var,y=avg,ymin = avg-se,ymax=avg+se
  )+
    geom_line(aes(alpha=log(n_rows)/max(log(n_rows))))+
    geom_ribbon(alpha=.1)+ggtitle(var)+theme(legend.title=element_blank())
}


isnum = F;var = "etat_sol"
isnum = T;var = "hnuage1"
isnum = T;var = "hnuage2"
isnum = T;var = "hnuage3"
isnum = T;var = "hnuage4"
isnum = T;var = "tend24"
isnum = T;var = "rr24"

# isnum = F;var = "cod_tend"
# isnum = T;var = "ht_neige"
isnum = T;var = "ssfrai"

isnum = T;var = "u"
isnum = T;var = "t"
isnum = T;var = "dd"
isnum = T;var = "rafper"

stat_univ(isnum,var)

table(weatherOrly$hnuage4)


table(weatherOrly$cod_tend)

summary(as.numeric(weatherOrly$hnuage1))
summary(as.numeric(weatherOrly$ht_neige))


#####################################



weatherOrly = weather[numer_sta == "07149"]
suppressWarnings({
  # weatherOrly[,ht_neige:=as.numeric(ht_neige)]
  weatherOrly[,hnuage1:=as.numeric(hnuage1)]
  weatherOrly[,etat_sol:=as.numeric(etat_sol)]
  weatherOrly[,rr24:=as.numeric(rr24)]
  weatherOrly[,t:=as.numeric(t)]
  weatherOrly[,u:=as.numeric(u)]
  
})

meteo= weatherOrly[,c("day","hour","hnuage1","t","u","rr24"),with=F]

meteo = meteo[,.(t_min = min(t),
                 t_max = max(t),
                 t_sd = sd(t),
                 t_mean = mean(t),
                 t_med = median(t),
                 u_min = min(u),
                 u_max = max(u),
                 u_sd = sd(u),
                 u_mean = mean(u),
                 u_med = median(u),                 
                 hnuage_min = min(hnuage1),
                 hnuage_max = max(hnuage1),
                 
                 rr24_min = min(rr24,na.rm=T),
                 rr24_max = max(rr24,na.rm=T)
),by="day"]
meteo[is.infinite(rr24_min),rr24_min:=0]
meteo[rr24_min<0,rr24_min:=0]
meteo[is.infinite(rr24_max),rr24_max:=0]
meteo[rr24_max<0,rr24_max:=0]

etat_sol = weatherOrly[etat_sol%in%c(0,1,2,4,5),.(etat_sol = max(etat_sol)),by="day"]
etat_sol = merge(etat_sol,unique(agg_rpu[,.(date_entree)]),
                 by.x="day",by.y="date_entree",all.y=T)
etat_sol$value = 1
etat_sol = dcast(etat_sol,day ~ etat_sol, value.var = "value", fill=0)
etat_sol[,"NA":=NULL]
names(etat_sol) <- c("day","sol_sec","sol_humide","sol_innonde","sol_gele","sol_givre")

meteo = merge(meteo,etat_sol,by="day")

fwrite(meteo,"data/prepared/meteo.csv")






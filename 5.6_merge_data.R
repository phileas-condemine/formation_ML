library(data.table)

finess_geoloc = fread("data/prepared/GEOLOCALISATION_FINESS.csv",select=c("finess","longitude","latitude"))
finess_geoloc = finess_geoloc[,.SD[1],by="finess"]
table(nchar(finess_geoloc$finess))
rpu = fread("data/prepared/finess2k_daily_respi_circu.csv")
rpu[,finess:=as.character(finess)]
rpu[,date_entree := as.Date(date_entree,"%d/%m/%Y")]
dates = unique(rpu$date_entree)
finess = unique(rpu$finess)
categories_age = unique(rpu$categorie_age)
modesortie = unique(rpu$mode_sortie)
rpu_fill = data.table(expand.grid(date_entree = dates, finess = finess, categorie_age = categories_age,mode_sortie=modesortie))
rpu_fill$nb = 0
rpu_fill[,nb:=as.integer(nb)]
rpu[,nb:=as.integer(nb)]
nrow(rpu_fill)
nrow(rpu)
rpu_fill = rpu_fill[rpu,on=c("date_entree","categorie_age","finess",'mode_sortie'),nb:=i.nb]
summary(rpu_fill$nb)
rpu = merge(rpu_fill,finess_geoloc,by="finess")





# agg_rpu = rpu[,.(passages = sum(nb),hospi_nna = .N),by='date_entree']
# agg_rpu = rpu[,.(nb=sum(nb)),by='date_entree']
# save(agg_rpu,file="data/prepared/aggr_rpu.RData")

meteo = fread("data/prepared/meteo.csv")
meteo[,day := as.Date(day,"%Y-%m-%d")]
# table(meteo$ninsee)

pollution = fread("data/prepared/pollution.csv")
pollution[,date := as.Date(date,"%Y-%m-%d")]


pollution_pos = unique(pollution[,.(ninsee,latitude,longitude)])
pollution_pos = na.omit(pollution_pos)
rpu_pos = unique(rpu[,.(finess,latitude,longitude)])
neighbors = FNN::get.knnx(pollution_pos[,.(latitude,longitude)],rpu_pos[,.(latitude,longitude)],k=1)$nn.index
rpu_pos$ninsee_nearest = pollution_pos[neighbors]$ninsee

test = F 

if (test){
  library(ggplot2)
  library(plotly)
  rpu = merge(rpu,rpu_pos[,.(finess,ninsee_nearest)],by="finess")
  rpu[,date_anticipee_lag1 := lag(date_entree,1)]
  
  rpu_agg = rpu[,.(nb=sum(nb)),by=.(ninsee_nearest,date_anticipee_lag1,categorie_age)]
  rpu_agg = merge(rpu_agg,pollution[,-c('longitude','latitude','source'),with=F],by.x=c("ninsee_nearest","date_anticipee_lag1"),by.y=c("ninsee","date"))
  rpu_agg[,c("ninsee_nearest","date_anticipee_lag1"):=NULL]
  rpu_corr = rpu_agg[,cor(nb,.SD,use = "pairwise.complete.obs",
                      # method = "kendall"
                      method = "spearman"
                      # method = "pearson"
  ),by="categorie_age"]
  rpu_corr[,var := names(rpu_agg)[-1],by="categorie_age"]
  g <- ggplot(rpu_corr[var!="nb"],aes(x=categorie_age,y=var,fill=V1))+geom_tile()
  ggplotly(g)
  # bébés sensibles à 03 et enfants-adultes sensibles à pm10 ? Vieux peu présents en IdF
}



gtrends = fread("data/prepared/gtrends.csv")
setnames(gtrends,"keywork","keyword")
gtrends = dcast(gtrends,date~keyword,value.var="hits")
gtrends[,date:=as.Date(date)]


#### APPARIEMMENT SUR LE JOUR PRECEDENT #######
rpu[,date_anticipee_lag1 := lag(date_entree,1)]

rpu = merge(rpu,meteo,by.x="date_anticipee_lag1",by.y="day")
rpu = merge(rpu,rpu_pos[,.(finess,ninsee_nearest)],by="finess")
rpu = merge(rpu,pollution[,-c('longitude','latitude','source'),with=F],by.x=c("ninsee_nearest","date_anticipee_lag1"),by.y=c("ninsee","date"))
rpu = merge(rpu,gtrends,by.x="date_anticipee_lag1",by.y="date")
rpu[,date_anticipee_lag1:=NULL]
###############################################
setorder(rpu,finess,date_entree)
add_lag = function(rpu,k){
  rpu_lagged = rpu[,.(finess,nb,date_entree,categorie_age,mode_sortie)]
  rpu_lagged[,date_entree:=date_entree+k]
  setnames(rpu_lagged,"nb",paste0("nb_lag",k))
  rpu = merge(rpu,rpu_lagged,by=c("finess","date_entree","categorie_age","mode_sortie"),all.x=T)
  return(rpu)
}
rpu = add_lag(rpu,1)
rpu = add_lag(rpu,7)
rpu = add_lag(rpu,30)
rpu = add_lag(rpu,2)


rpu_weekday = unique(rpu[,"date_entree"])
rpu_weekday$weekday = weekdays(rpu_weekday$date_entree)
rpu_weekday$top = 1
rpu_weekday = dcast(rpu_weekday,date_entree~weekday,value.var="top",fill = 0)

rpu = merge(rpu,rpu_weekday,by="date_entree")




#### Ajout des jours fériés et vacances scolaires

# https://www.data.gouv.fr/fr/datasets/vacances-scolaires-par-zones/
# download.file('https://www.data.gouv.fr/fr/datasets/r/c3781037-dffb-4789-9af9-15a955336771',"data/vacances_scolaires.csv",mode = "wb")
# https://www.data.gouv.fr/fr/datasets/jours-feries-en-france/
# download.file('https://www.data.gouv.fr/fr/datasets/r/d8d19f62-e5d5-43cf-b4cd-1d67dd208114',"data/jours-feries.csv",mode = "wb")

calendrier = fread("data/vacances_scolaires.csv",select=c("date","vacances_zone_c","nom_vacances"))
jours_feries = fread("data/jours-feries.csv")
min_year = min(year(rpu$date_entree))
calendrier[,date:=as.Date(date)]
calendrier = calendrier[year(date)>=min_year-1]
setorder(calendrier,date)

calendrier$ieme_jour_vacances[1] = calendrier$vacances_zone_c[1]*1
calendrier$distance_apres_vacances[1] = !calendrier$vacances_zone_c[1]*1
for (i in 2:nrow(calendrier)){
  calendrier$ieme_jour_vacances[i] = (calendrier$ieme_jour_vacances[i-1] + calendrier$vacances_zone_c[i]) *  calendrier$vacances_zone_c[i]
  calendrier$distance_apres_vacances[i] = (calendrier$distance_apres_vacances[i-1] + !(calendrier$vacances_zone_c[i])) *  !(calendrier$vacances_zone_c[i])
}
setorder(calendrier,-date)
calendrier$distance_avant_vacances[1] = !calendrier$vacances_zone_c[1]*1
for (i in 2:nrow(calendrier)){
  calendrier$distance_avant_vacances[i] = (calendrier$distance_avant_vacances[i-1] + !(calendrier$vacances_zone_c[i])) *  !(calendrier$vacances_zone_c[i])
}

jours_feries[,date:=as.Date(date)]
jours_feries = jours_feries[year(date)>=min_year-1]
setorder(jours_feries,date)

jours_feries$j1_apres_jour_ferie[1]=F
for(i in 2:nrow(jours_feries)){
  jours_feries$j1_apres_jour_ferie[i] = (jours_feries$est_jour_ferie[i-1])&!(jours_feries$est_jour_ferie[i])
}
nrow(rpu)
rpu = merge(rpu,calendrier,by.x="date_entree",by.y="date")
nrow(rpu)
rpu = merge(rpu, jours_feries,by.x="date_entree",by.y="date")
nrow(rpu)



fwrite(rpu,file="data/prepared/dataset.csv")


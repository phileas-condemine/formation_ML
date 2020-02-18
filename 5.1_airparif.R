# https://data-airparif-asso.opendata.arcgis.com/datasets/mes-idf-horaire-no2
library(data.table)

#### DONNEES AIRPARIF 09/2017 à AUJDH
pol_files = list.files("data/pollution/airparif.asso/")

pollution = lapply(pol_files,function(file){
  fread(paste0("data/pollution/airparif.asso/",file),colClasses = c("id_com"="character"))
})

pollution = rbindlist(pollution)

pollution[,date:=substr(date_debut,1,10)]
pollution[,date:=as.Date(date,"%Y-%m-%d")]
pollution[,heure:=substr(date_debut,12,13)]
pollution = pollution[,c("id_com","nom_station","typologie","nom_poll","valeur","x","y","date","heure")]
# summary(pollution$date)
# table(pollution$nom_poll)
table(pollution$typologie)
pollution = pollution[!typologie=="Trafic",.(valeur=mean(valeur)),by=c("id_com","date","nom_poll")]
pollution = dcast(pollution,id_com+date~nom_poll,value.var="valeur")
setnames(pollution,c("NO2","O3","PM10","id_com"),c("no2","o3","pm10","ninsee"))
pollution$source = "airparif"
pollution_airparif = pollution
uniqueN(pollution_airparif$ninsee)
communes_sources_connues = unique(pollution_airparif$ninsee)

#### DONNEES DATAGOUV 2016 (voire avant mais rpu non dispo avant) à 4/2018
pol_files = list.files("data/pollution/data.gouv/")

pollution = lapply(pol_files,function(file){
  fread(paste0("data/pollution/data.gouv/",file),colClasses = c("ninsee"="character"))
})
pollution = rbindlist(pollution)
# table(nchar(pollution$ninsee))
pollution = pollution[nchar(ninsee)==5]
pollution[,date:=as.Date(date,format="%d/%m/%Y")]
uniqueN(pollution$ninsee)
pollution = pollution[ninsee%in%communes_sources_connues]
table(pollution$ninsee)
pollution$source = "datagouv"
pollution = rbind(pollution,pollution_airparif)

# http://www.nosdonnees.fr/wiki/index.php/Fichier:EUCircos_Regions_departements_circonscriptions_communes_gps.csv.gz
com_loc = fread("data/eucircos_regions_departements_circonscriptions_communes_gps.csv",encoding = "UTF-8")
com_loc[,code_insee:=stringr::str_pad(string = code_insee,pad = "0",width = 5,side = "left")]
com_loc = com_loc[,c("code_insee","latitude","longitude")]
com_loc = unique(com_loc)
pollution = merge(pollution,com_loc,by.x="ninsee",by.y="code_insee")

setorder(pollution,source)
pollution = pollution[,.SD[1],by=.(date,ninsee)]


# imputation 
for (var in c("no2","o3","pm10")){
  print(var)
  setnames(pollution,var,"var")
  avg_per_date = pollution[,var:=mean(var,na.rm=T),by="date"]
  pollution[avg_per_date,var:=i.var,on="date"]
  setnames(pollution,"var",var)
}
lapply(pollution,function(x)sum(is.na(x)))

setorder(pollution,ninsee,date)
pollution[,no2_avg_7J := frollmean(no2,7,na.rm =T),by="ninsee"]
pollution[,o3_avg_7J := frollmean(o3,7,na.rm =T),by="ninsee"]
pollution[,pm10_avg_7J := frollmean(pm10,7,na.rm =T),by="ninsee"]

frollmax = function(var,dates,k){
  suppressWarnings({
  res = c()
  for (date in dates){
    val = max(var[dates>date-k & dates <= date],na.rm=T)
    if (is.infinite(val)) val = 0
    res = c(res,val)
  }
  return(res)
  })
}

pollution[,no2_max_3J := frollmax(no2,date,3),by="ninsee"]
pollution[,o3_max_3J := frollmax(o3,date,3),by="ninsee"]
pollution[,pm10_max_3J := frollmax(pm10,date,3),by="ninsee"]

pollution[,no2_max_7J := frollmax(no2,date,7),by="ninsee"]
pollution[,o3_max_7J := frollmax(o3,date,7),by="ninsee"]
pollution[,pm10_max_7J := frollmax(pm10,date,7),by="ninsee"]

rpu = 



fwrite(pollution,file="data/prepared/pollution.csv")


library(data.table)
library(sp)

# https://www.airparif.asso.fr/actualite/detail/id/276
pesticides = readxl::read_excel("data/pollution/phyto/pesticides_2002_2017-1.xlsx",sheet = "pesticides_2002_2017")
pesticides = data.table(pesticides)
table(pesticides$`Code INSEE`)

g <- ggplot(pesticides[,.(count=.N),by=c("xlamb93","ylamb93","Commune")],aes(x=xlamb93,y=ylamb93,size=count,label=Commune))+geom_point()
plotly::ggplotly(g)


coords = unique(pesticides[,.SD[1],by="Code INSEE",.SDcols=c("xlamb93","ylamb93")])
coordinates(coords) <- c("xlamb93", "ylamb93")
proj4string(coords) <- CRS("+init=epsg:2154") # WGS 84
CRS.new <- CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")
coords <- spTransform(coords, CRS.new)
coords <- cbind(coords@coords,coords@data)
coords <- data.table(coords)
setnames(coords,c("xlamb93","ylamb93"),c("lon","lat"))

pesticides = merge(pesticides,coords,by="Code INSEE")
pesticides = pesticides[`Concentration (ng/m3)`!=0]
table(unique(pesticides[,c("Code INSEE","Annee","Semaine")])$`Code INSEE`)
table(unique(pesticides[,c("Commune","Annee")])$`Commune`)
g <- ggplot(coords,aes(x=lon,y=lat,label=`Code INSEE`))+geom_point()
plotly::ggplotly(g)

summary(pesticides[`Code INSEE`=="75118"]$`Debut prelevement`)

one_commune = sample(unique(pesticides$Commune),1)
few_molecules = sample(unique(pesticides[Commune==one_commune])$`Substance active`,3)
data_sample = pesticides[Commune%in%one_commune&`Substance active`%in%few_molecules]
g <- ggplot(data_sample,aes(x=`Debut prelevement`,y=`Concentration (ng/m3)`,color=`Substance active`))+geom_point()+ggtitle(one_commune)
plotly::ggplotly(g)

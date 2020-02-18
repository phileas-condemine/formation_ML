library(data.table)
annee=17

finess2k_daily_respi = lapply(16:18,function(annee){
  # récupération par motif, dp ou diag à cause des pratiques de codage variables...
  print(annee)
  passage = fread(sprintf("RPU_PASSAGE%s.csv",annee),encoding = "UTF-8")
  names(passage) <- tolower(names(passage))
  
  passage = passage[,c("ident","finess","dp","motif","date_entree","age"),with=F]
  passage[,motif_respi := grepl("(J)([0-9]{3,5})",motif)]
  
  diag = fread(sprintf("RPU_DIAG%s.csv",annee))
  diag = diag[substr(diag,1,1)=="J"]
  diag = unique(diag,by="ident")
  passage = merge(passage,diag,by="ident",all.x=T)
  passage = passage[!is.na(diag) #vient de la table diag
                    | (motif_respi) # motif de la table passage
                    | substr(dp,1,1)=="J"] # dp de la table passage
  passage = unique(passage,by="ident")
  passage = passage[substr(finess,1,2)%in%c("75","77","78","91","92","93","94","95")]
  
  passage= passage[age<120]
  
  
  passage$categorie_age = cut(passage$age,breaks = c(0,2,15,50,120),include.lowest = T)
  table(passage$categorie_age)
  # sum(is.na(passage$categorie_age))
  # sum(is.na(passage$age))
  # summary(passage$age)
  
  
  flux_respi = passage[,.(nb=.N),by=c("date_entree","finess","categorie_age")]
  
  flux_respi[,sum(nb),by="finess"]
  
  # sum(flux_respi[is.na(categorie_age)]$nb)
  # sum(flux_respi[!is.na(categorie_age)]$nb)
  
  # table(substr(passage$dp,1,3))
  
  flux_respi[,eta_tot := sum(nb),by="finess"]
  flux_respi  = flux_respi[eta_tot>2000]
  nrow(flux_respi)
  quantile(flux_respi$nb,0:100/100)
  
  flux_respi[,eta_tot:=NULL]
  flux_respi
})

finess2k_daily_respi = rbindlist(finess2k_daily_respi)

fwrite(finess2k_daily_respi,file="finess2k_daily_respi.csv")

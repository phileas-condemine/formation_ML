library(data.table)
library(gtrendsR)
library(lubridate)
library(magrittr)
keywords = c("sos médecin","doctissimo","toux sèche","doliprane","codéine","grippe","bronchite","angine")


keyword = "mal de gorge"

get_gtrends = function(keyword){
  print(keyword)
  trends = list()
  start_date = as.Date("2015-08-01","%Y-%m-%d")
  my_date = start_date
  avg=NULL
  while(year(my_date)<2019){
    print(my_date)
    window = paste(my_date,my_date%m+% months(8))
    trend = gtrends(keyword,hl="fr",
                    geo="FR",time=window,onlyInterest = T)
    trend = data.table(trend$interest_over_time)
    trend$start = my_date
    # new_avg = mean(trend[date<my_date%m+% months(2)]$hits)
    # if(!is.null(avg)){
    #   trend[,hits:=hits*avg/new_avg]
    # }
    trends = rbind(trends,trend)
    my_date = my_date%m+% months(1)
    # avg = mean(trend[date>my_date]$hits)
  }
  trends = trends[,.(hits=mean(hits)),by="date"]
  trends$keyword = keyword
  fwrite(trends[date>as.Date("2016-01-01",format="%Y-%m-%d")],
         file=paste0("data/gtrends/",gsub(" ","_",keyword),".csv"))
  trends
}

all_trends = pbapply::pblapply(keywords[1:8],get_gtrends)

all_trends = lapply(list.files("data/gtrends/"),function(file){
  fread(paste0("data/gtrends/",file))
})%>%rbindlist

fwrite(all_trends,"data/prepared/gtrends.csv")

trends=get_gtrends(keyword)

library(ggplot2)
library(plotly)
g <- ggplot(data = trends, aes(x=date,y=hits)) + geom_line()
# g <- ggplot(data = trends,aes(x=date,y=hits,color=factor(start)))+geom_line()
ggplotly(g)



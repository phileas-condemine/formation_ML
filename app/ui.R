dashboardPage(
  dashboardHeader(title = "Urgences"),
  dashboardSidebar(disable = T),
  dashboardBody(
    fluidRow(
      box(width = 2,
          tags$img(src="https://www.iledefrance.ars.sante.fr/sites/default/files/styles/logo/public/2016-12/Logo_ARS-IDF_HD.jpg?itok=-RRjU73B",align="middle")),
      box(width = 6,selectInput("nm_eta","Etablissement",choices = setNames(geo_eta$finess,geo_eta$nom.service))),
      box(width = 2,selectInput("patho","Pathologie",choices = c("Toutes","Cardiologie","Traumatologie","Epidémie"))),
      box(width = 2,uiOutput("my_dates"))
    ),
    fluidRow(
      box(width = 6, plotlyOutput("current_flux")),
      box(width = 6, plotlyOutput("next_flux"))
    ),
    fluidRow(
      tags$h3("Typologie des patients - ventilation par catégorie d'âge et sévérité"),
      box(width=12, shinyWidgets::switchInput("jour_semaine","granularité",onLabel = "jour",offLabel = "semaine",size = "large"),uiOutput("ui_date_focus")),
      box(width = 4, plotlyOutput("flux_hospi")),
      box(width = 4, plotlyOutput("flux_rad")),
      box(width = 4, switchInput("mode_sortie","Sévérité",onLabel = "Hospi",offLabel = "RaD"),DTOutput("erreurs"))
    ),
    fluidRow(
      tags$h3("Cause des écarts aux valeurs de saison"),
      
      box(width=12,uiOutput("cat_age"),
          plotlyOutput("lime")
          # ,tags$img(src="https://miro.medium.com/max/777/1*IU0bE4Bp6gK91KJj1mOemg.png",align="middle"),collapsible=T,collapsed=T
      )
      
    ),
    fluidRow(
      tags$h3("Variation autour des moyennes de saison de la météo, de la pollution et des tendances de recherche Google"),
      box(width = 4, selectInput("var_meteo","Données météo",selected = c("temp_min","temp_max"),multiple = T,choices = c("temp_min","temp_max","pression","humidité","force du vent")),
          plotlyOutput("variation_meteo"),collapsible = T,collapsed = T),
      box(width = 4, selectInput("molecule","Données de pollution",selected = c("O3"),multiple=F,choices = c("CO","PM25","PM10","O3","NO2")),
          plotlyOutput("variation_pollution"),collapsible = T,collapsed = T),
      box(width = 4, selectInput("trend","Données tendances Google",selected = c("rhinopharyngite"),multiple=F,choices = vars_gtrends),
          plotlyOutput("variation_gtrends"),collapsible = T,collapsed = T)
      
    )

    
    
  ),
  title = "Urgences"
)

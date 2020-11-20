# This is a shiny dashboard

# Cargar paquetes
library(shiny)
library(shinydashboard)
library(DT)
library(dplyr)
library(lubridate)
library(readr)
library(leaflet)
library(plotly)
library(raster)
library(dashboardthemes)
library(rgdal)


# Cargar datos
load(file = 'datos.RData', .GlobalEnv)
load(file = 'medellin_map.RData', .GlobalEnv)

header <-   dashboardHeader(  ### changing logo
  title = shinyDashboardLogo(
    theme = "poor_mans_flatly",
    boldText = "Accidentalidad" ,
    mainText = "Medellin",
    badgeText = "2014-2018"
  ),
  titleWidth = 300
)

source("source_map.R")

header$children[[2]]$children[[2]] <- header$children[[2]]$children[[1]]
header$children[[2]]$children[[1]] <- tags$a(tags$img(src='transitapp.png',width = "50%", height = "50%"),
                                             target = '_blank') 
ui <- dashboardPage(title = "TRANSITApp",
                    header,
                    dashboardSidebar(
                      sidebarMenu(
                        menuItem("Inicio", tabName = "inicio", icon = icon("home")),
                        menuItem("Tabla de datos históricos", tabName = "datos", icon = icon("table")),
                        menuItem("Visualizacion", href = "https://daniel-chanci-restrepo.shinyapps.io/Visualizacion_accidentalidad/", icon = icon("chart-bar")),
                        menuItem("Accidentalidad", tabName = "acc",icon = icon("car-crash"),
                                 menuSubItem("Accidentalidad comunas",
                                             href = "https://valentina-garcia-velasquez.shinyapps.io/Prediccion_accidentalidad_comuna/"),
                                 menuSubItem("Accidentalidad barrios",
                                             href = "https://valentina-garcia-velasquez.shinyapps.io/Prediccion_accidentalidad_barrio/")),
                        menuItem("Agrupamiento", href = "https://daniel-chanci-restrepo.shinyapps.io/Agrupamiento/", icon = icon("map-marked-alt")),
                        menuItem("Equipo", tabName = "equipo", icon = icon("users"))
                      )
                    ),
                    ## Body content
                    dashboardBody(
                      ### Changing theme
                      shinyDashboardThemes(
                        theme = "poor_mans_flatly"
                      ),
                      includeCSS("www/style.css"),
                      tabItems(
                       # ,
                        tabItem(tabName = "inicio",
                                # En esta parte de inicio se pone el video
                                # la descripcion de la app
                                h1("¡Bienvenidos!",
                                   align = "center"),
                                fluidRow(HTML('<center><img src="home.png" width="30%" height="30%"></center>')),
                                h5("En esta aplicación web encontrarás información sobre los accidentes de tránsito en Medellín 
                 desde el año 2014 a 2018. Aquí podrás visualizar los datos históricos de accidentalidad, un mapa
                 donde se encuentran los barrios de Medellín agrupados de acuerdo al número de accidentes y, además,
                 nuestra herramienta te permitirá realizar predicciones de accidentes, por barrio, comuna y clase 
                 de accidente."),
                                h4(),
                                h5("A continuación un video donde se explica como utilizar las diferentes herramientas de la 
                 aplicación web"),
                                HTML('<p align = "center"><iframe width="560" height="315" src="https://www.youtube.com/embed/5qn82BmX6B8" frameborder="0" allow="accelerometer; autoplay; clipboard-write; encrypted-media; gyroscope; picture-in-picture" allowfullscreen></iframe></p>')
                                
                        ),
                        
                        tabItem(tabName = "datos",
                                h1("Datos históricos de accidentes", align = "center"),
                                box(width = 14,
                                    dateRangeInput("daterange", "Rango de Tiempo:",
                                                   start  = "2014-01-01",
                                                   end    = "2018-12-31",
                                                   min    = "2014-01-01",
                                                   max    = "2018-12-31",
                                                   format = "dd/mm/yyyy",
                                                   separator = " - ",
                                                   language = "es")
                                ),
                                fluidRow(box(DT::dataTableOutput("Data")  %>% shinycssloaders::withSpinner(color="#3498db"), 
                                             style = "height:500px; overflow-y: scroll;overflow-x: scroll;",
                                             width=12))
                        
                        ),
                        tabItem(tabName = "equipo",
                                h2("Daniel Chanci Restrepo", align = "center"),
                                h4('Ingeniería de sistemas', align = "center"),
                                br(),
                                h2("Valentina García Velásquez", align = "center"),
                                h4('Estadística', align = "center"),
                                br(),
                                h2("Jaime Andrés Molina Correa", align = "center"),
                                h4('Estadística', align = "center"),
                                br(),
                                h2("Ricardo Peñaloza Velásquez", align = "center"),
                                h4('Ingeniería de sistemas', align = "center"),
                                br(),
                                h2("Felipe Villarreal Piedrahita", align = "center"),
                                h4('Ingeniería de sistemas', align = "center"),
                                br()
                        )
                        
                      ) # tabItems
                    ) # DashboardBody
) # dashboardBody

server <- function(input, output) {

  #Datos de visualizacion
  output$Data <- DT::renderDataTable(
    DT::datatable({
      datos %>% filter(ymd(FECHA) >= input$daterange[1], ymd(FECHA) <= input$daterange[2]) %>% 
        group_by(FECHA,CLASE,GRAVEDAD,COMUNA,BARRIO) %>%
         summarise("TOTAL ACCIDENTES" = n())
    },
    options = list(lenghtMenu = list(c(7, 15, -1), c('5', '15', 'All')), pageLenght = 15),
    filter = "top",
    style = "bootstrap"
    ))

}

shinyApp(ui, server)

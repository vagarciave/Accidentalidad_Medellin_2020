# This is a shiny dashboard

# Cargar paquetes
library(shiny)
library(shinythemes)
library(shinydashboard)
library(DT)
require(dplyr)
require(tidyr)
require(lubridate)
library(readr)
library(leaflet)
library(plotly)
library(raster)
library(dashboardthemes)
library(rgdal)

 # Cargar datos 
load(file = 'barrio_dia_2019pred.RData',.GlobalEnv)
load(file = 'barrio_semana_2019pred.RData',.GlobalEnv)
load(file = 'barrio_mes_2019pred.RData',.GlobalEnv)

# listas para nombre de barrios y comunas
list_barrios <- sort(unique(barrio_dia_2019pred$BARRIO))
source('source_models.R')

header <-   dashboardHeader(  ### changing logo
  title = shinyDashboardLogo(
    theme = "poor_mans_flatly",
    boldText = "Accidentalidad" ,
    mainText = "Medellin",
    badgeText = "2014-2018"
  ),
  titleWidth = 300
)


header$children[[2]]$children[[2]] <- header$children[[2]]$children[[1]]
header$children[[2]]$children[[1]] <- tags$a(tags$img(src='transitapp.png',width = "50%", height = "50%"),
                                             target = '_blank') 
ui <- dashboardPage(title = "Predicciones barrio",
                    header,
                    dashboardSidebar(
                      sidebarMenu(
                        menuItem("Accidentalidad", tabName = "acc",icon = icon("car-crash"))
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
                       # 
                        tabItem(tabName = "acc",
                                h1("Total de accidentes de tránsito", align = 'center'),
                                h5("A continuación podrá seleccionar un periodo de tiempo para obtener el total de accidentes 
                 por día, semana y mes. Recuerde que de 2014 a 2018 se mostrarán los datos reales, a partir de 2019 se
                 realizan predicciones"),
                                fluidRow(
                                  box(width = 6,
                                      dateRangeInput("daterange_pred", "Rango de Tiempo:",
                                                     start  = "2014-01-01",
                                                     end    = "2019-12-31",
                                                     min    = "2014-01-01",
                                                     max    = "2019-12-31",
                                                     format = "dd/mm/yyyy",
                                                     separator = " - ",
                                                     language = "es")
                                  ),
                                  
                                  # Only show this panel if tipo_modelo is comuna
                                  
                                  box(width = 6,
                                        selectInput("nombre_barrio", "Nombre del barrio",
                                                    list_barrios))
                                  
                                ),
                                
                                fluidRow(
                                  box(width = 12,
                                        tabsetPanel(
                                          # Muestra el grafico de predichos por accidente diarios
                                          tabPanel("Predicción por mes",
                                                   h1(),
                                                   plotlyOutput("plotpred_mesb") %>% shinycssloaders::withSpinner(color="#3498db")
                                          ),
                                          # Muestra el grafico de predichos por accidente semanal
                                          tabPanel("Predicción por semana",
                                                   h1(),
                                                   plotlyOutput("plotpred_semanab") %>% shinycssloaders::withSpinner(color="#3498db")
                                          ),
                                          tabPanel("Predicción por dia",
                                                   h1(),
                                                   plotlyOutput("plotpred_diab") %>% shinycssloaders::withSpinner(color="#3498db")
                                                   )
                                        )
                                  )
                                )
                        

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
  # Grafica de dia
  output$plotpred_diab <- renderPlotly({
    validate(need(input$daterange_pred[1] < input$daterange_pred[2],
                  "Error: la fecha final no puede ser menor que la fecha de inicio"))
    pred_barrio_dia(fecha_inicio = input$daterange_pred[1],
                    fecha_fin = input$daterange_pred[2],
                    nombre = input$nombre_barrio)
  })
  
  # Grafica de semana
  output$plotpred_semanab <- renderPlotly({
    validate(need(input$daterange_pred[1] < input$daterange_pred[2],
                  "Error: la fecha final no puede ser menor que la fecha de inicio"))
    pred_barrio_semana(fecha_inicio = input$daterange_pred[1],
                       fecha_fin = input$daterange_pred[2],
                       nombre = input$nombre_barrio)
  })
  
  # Grafica de mes
  output$plotpred_mesb <- renderPlotly({
    validate(need(input$daterange_pred[1] < input$daterange_pred[2],
                  "Error: la fecha final no puede ser menor que la fecha de inicio"))
    pred_barrio_mes(fecha_inicio = input$daterange_pred[1],
                    fecha_fin = input$daterange_pred[2],
                    nombre = input$nombre_barrio)
  })

 
}

shinyApp(ui, server)

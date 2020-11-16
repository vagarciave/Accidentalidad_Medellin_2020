####################################
# Data Professor                   #
# http://youtube.com/dataprofessor #
# http://github.com/dataprofessor  #
####################################

# Modified from Winston Chang, 
# https://shiny.rstudio.com/gallery/shiny-theme-selector.html

# Concepts about Reactive programming used by Shiny, 
# https://shiny.rstudio.com/articles/reactivity-overview.html


# This is a shiny dashboard

# Load R packages
library(shiny)
library(shinythemes)
library(shinydashboard)
library(DT)
require(RMySQL)
require(dplyr)
require(tidyr)
require(lubridate)
library(readr)
library(leaflet)
library(readr)
library(plotly)


# Cargar valores ajustados
load(file = 'accidentes_dia_barrio.RData',.GlobalEnv)
load(file = 'accidentes_dia_comuna.RData',.GlobalEnv)

datos <- read.csv("Base_definitiva.csv", encoding = 'UTF-8', stringsAsFactors=T)
datos <- subset( datos, select = -c(DIA, MES, PERIODO, DIA_FESTIVO, SEMANA_MES ) )
# datos <- datos[sample(1:dim(datos)[1],1000),]
list_barrios <- sort(unique(datos$BARRIO))
list_comunas <- sort(unique(datos$COMUNA))

source('source_models.R')

ui <- dashboardPage(
  dashboardHeader(title = "TAE 2020-2"),
  ## Sidebar content
  dashboardSidebar(
    sidebarMenu(
      menuItem("Inicio", tabName = "inicio", icon = icon("chart-bar")),
      menuItem("Visualizacion", tabName = "visualizacion", icon = icon("table")),
      menuItem("Prediccion", tabName = "prediccion", icon = icon("car-crash")),
      menuItem("Agrupamiento", tabName = "agrupamiento", icon = icon("map-marked-alt"))
    )
  ),
  ## Body content
  dashboardBody(
    tabItems(
      # First tab content
      # First tab content
      tabItem(tabName = "inicio",
              # En esta parte de inicio se pone el video
              # la descripcion de la app
              box(
                tags$h3("Entradas:"),
                dateRangeInput("daterange", "Rango de Tiempo:",
                               start  = "2014-01-01",
                               end    = "2018-12-31",
                               min    = "2014-01-01",
                               max    = "2018-12-31",
                               format = "dd/mm/yyyy",
                               separator = " - ",
                               language = "es"),
                selectInput("tipo", "Tipo:",
                            c("Choque" = "Choque",
                              "Atropello" = "Atropello",
                              "Caida de Ocupante" = "Caida de Ocupante",
                              "Incendio" = "Incendio"))
              ),# sidebarPanel
              box(
                h1("Header 1"),
                h4("Output 1"),
                verbatimTextOutput("txtout"),
                
              ) # mainPanel
      ),

     tabItem(tabName = "visualizacion",
             "Visualizacion",
              titlePanel("Incidentes Georreferenciados"),
             fluidRow(column(DT::dataTableOutput("Data"), 
                             style = "height:500px; overflow-y: scroll;overflow-x: scroll;",
                             width=12))
      ),
      
      # Second tab content
      tabItem(tabName = "prediccion",
              fluidRow(
                tags$h3("Entradas:"),
                box(width = 4,
                  dateRangeInput("daterange_pred", "Rango de Tiempo:",
                               start  = "2014-01-01",
                               end    = "2018-12-31",
                               min    = "2014-01-01",
                               max    = "2018-12-31",
                               format = "dd/mm/yyyy",
                               separator = " - ",
                               language = "es")
                  ),
                
                box(width = 4 ,
                    selectInput("tipo_modelo", "Seleccione el tipo de modelo",
                            c(Comuna = "comuna",
                              Barrio = "barrio"))
                    ),
                # Only show this panel if tipo_modelo is comuna
                
                box(width = 4,
                    conditionalPanel(
                      condition = "input.tipo_modelo == 'comuna'",
                      selectInput("nombre_comuna", "Nombre de la comuna",
                                  list_comunas)
                    ),
                    
                    # Only show this panel if tipo_modelo is barrio
                    conditionalPanel(
                      condition = "input.tipo_modelo == 'barrio'",
                      selectInput("nombre_barrio", "Nombre del barrio",
                                  list_barrios)
                    )
                 )

              ),
              
              fluidRow(
                  box(width = 12,
                      tabsetPanel(
                        # Muestra el grafico de predichos por accidente diarios
                        tabPanel("Predicción por dia",
                                 h1(),
                                 plotlyOutput("plotpred_dia") %>% shinycssloaders::withSpinner(color="#0dc5c1"),
                                 h1(),
                                 DTOutput("dfpred_dia")),
                        # Muestra el grafico de predichos por accidente semanal
                        tabPanel("Predicción por semana",
                                 h1(),
                                 plotlyOutput("plotpred_semana") %>% shinycssloaders::withSpinner(color="#0dc5c1"),
                                 h1(),
                                 DTOutput("dfpred_semana")),
                        # Muestra el grafico de predichos por accidente mensual
                        tabPanel("Predicción por mes",
                                 h1(),
                                 plotlyOutput("plotpred_mes") %>% shinycssloaders::withSpinner(color="#0dc5c1"),
                                 h1(),
                                 DTOutput("dfpred_mes"))
                        
                      )
                    ),
              ),
              fluidRow(
                box(width = 12)
              )
      ),
      # Second tab content
      tabItem(tabName = "agrupamiento",
              titlePanel("Seleccione el tipo de incidente:"),
                fluidPage(
                  column(width = 12,
                         box(width = NULL, solidHeader = TRUE,
                             leafletOutput("mymap"),
                             p()
                         ),
                         box(width = 14,
                           h1('Acá va una tablita linda'))
                  )
              )
       )
      
    ) # tabItems
  ) # DashboardBody
) # dashboardBody

server <- function(input, output) {
  
  output$txtout <- renderText({
    paste(weekdays(ymd(input$daterange[1])),ymd(input$daterange[2]), sep = ',')
  })
  
  output$Data <- DT::renderDataTable(
    DT::datatable({
      datos
    },
    options = list(lenghtMenu = list(c(7, 15, -1), c('5', '15', 'All')), pageLenght = 15),
    filter = "top",
    selection = "multiple",
    style = "bootstrap"
    ))
  
  output$mymap <- renderLeaflet({
    leaflet() %>%
      addTiles() %>%
      addMarkers(lat = datos[1:100, "LATITUD"],
                 lng = datos[1:100, "LONGITUD"], popup = datos[1:100,"FECHA"])
  })
  
  # Funcion para cargar todos los resultado
  control_reactive_pred <- reactive({
    validate(need(input$daterange_pred[1] < input$daterange_pred[2],
           "Error: la fecha final no puede ser menor que la fecha de inicio"))
    
    control_prediction(fecha_inicio = input$daterange_pred[1],
    fecha_fin = input$daterange_pred[2],
    tipo_modelo = input$tipo_modelo,
    nombre = ifelse(input$tipo_modelo == 'comuna', input$nombre_comuna, input$nombre_barrio))
  })
  
  # Grafica de dia
  output$plotpred_dia <- renderPlotly({
    gp <- control_reactive_pred()
    gp$fig_dia
  })
  
  # Grafica de semana
  output$plotpred_semana <- renderPlotly({
    gp <- control_reactive_pred()
    gp$fig_semana
  })
  
  # Grafica de mes
  output$plotpred_mes <- renderPlotly({
    gp <- control_reactive_pred()
    gp$fig_mes
  })
  
  # datos de dia
  output$dfpred_dia <- renderDT({
    gp <- control_reactive_pred()
    DT::datatable({
      gp$df_dia
    },
    options = list(lenghtMenu = list(c(7, 15, -1), c('5', '15', 'All')), pageLenght = 15),
    filter = "top",
    selection = "multiple",
    style = "bootstrap"
    )
    
  })
  
  # datos de semana
  output$dfpred_semana <- renderDT({
    gp <- control_reactive_pred()
    DT::datatable({
      gp$df_semana
    },
    options = list(lenghtMenu = list(c(7, 15, -1), c('5', '15', 'All')), pageLenght = 15),
    filter = "top",
    selection = "multiple",
    style = "bootstrap"
    )
    
  })
  
  # datos de mes
  output$dfpred_mes <- renderDT({
    gp <- control_reactive_pred()
    DT::datatable({
      gp$df_mes
    },
    options = list(lenghtMenu = list(c(7, 15, -1), c('5', '15', 'All')), pageLenght = 15),
    filter = "top",
    selection = "multiple",
    style = "bootstrap"
    )
    
  })
  
}

shinyApp(ui, server)
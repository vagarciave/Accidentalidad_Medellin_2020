####################################
# Data Professor                   #
# http://youtube.com/dataprofessor #
# http://github.com/dataprofessor  #
####################################

# Modified from Winston Chang, 
# https://shiny.rstudio.com/gallery/shiny-theme-selector.html

# Concepts about Reactive programming used by Shiny, 
# https://shiny.rstudio.com/articles/reactivity-overview.html

# Load R packages
library(shiny)
library(shinythemes)
library(DT)
require(RMySQL)
require(dplyr)
require(tidyr)
require(lubridate)
library(readr)
library(leaflet)
library(readr)
library(plotly)
datos <- read.csv("Base_definitiva.csv", encoding = 'UTF-8', stringsAsFactors=T)
datos <- subset( datos, select = -c(DIA, MES, PERIODO, DIA_FESTIVO, SEMANA_MES ) )
# datos <- datos[sample(1:dim(datos)[1],1000),]
list_barrios <- sort(unique(datos$BARRIO))
list_comunas <- sort(unique(datos$COMUNA))


# Define UI
ui <- fluidPage(theme = shinytheme("cerulean"),
                navbarPage(
                    # theme = "cerulean",  # <--- To use a theme, uncomment this
                    "TAE 2020-2",
                    tabPanel("Inicio",
                             sidebarPanel(
                                 tags$h3("Entradas:"),
                                 dateRangeInput("daterange3", "Rango de Tiempo:",
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
                                 #,
                                 #submitButton("Visualizar", icon("car-crash")),
                                 # Esto me daña los paneles condicionales
                                 
                             ), # sidebarPanel
                             mainPanel(
                                 h1("Header 1"),
                                 h4("Output 1"),
                                 verbatimTextOutput("txtout"),
                                 
                             ) # mainPanel
                             
                    ), # Navbar 1, tabPanel
                    tabPanel("Visualizacion",
                             titlePanel("Incidentes Georreferenciados"),
                             fluidRow(column(DT::dataTableOutput("Data"), 
                                             style = "height:500px; overflow-y: scroll;overflow-x: scroll;",
                                             width=12))
                             ),
                    tabPanel("Prediccion",
                             sidebarPanel(
                               tags$h3("Entradas:"),
                               dateRangeInput("daterange_pred", "Rango de Tiempo:",
                                              start  = "2014-01-01",
                                              end    = "2018-12-31",
                                              min    = "2014-01-01",
                                              max    = "2018-12-31",
                                              format = "dd/mm/yyyy",
                                              separator = " - ",
                                              language = "es"),
                               
                               selectInput("tipo_modelo", "Seleccione el tipo de modelo",
                                           c(Comuna = "comuna",
                                             Barrio = "barrio")),
                               # Only show this panel if tipo_modelo is comuna
                               conditionalPanel(
                                 condition = "input.tipo_modelo == 'comuna'",
                                 selectInput("nombre_comuna", "Nombre de la comuna",
                                             list_comunas)
                               ),
                               
                               # Only show this panel if tipo_modelo is barrio
                               conditionalPanel(
                                 condition = "input.tipo_modelo == 'barrio'",
                                 selectInput("nombre_comuna", "Nombre del barrio",
                                             list_barrios)
                               ),
                             ),

                               mainPanel(
                                 tabsetPanel(
                                 # Muestra el grafico de predichos por accidente diarios
                                 tabPanel("Predicción por dia",plotlyOutput('plotpred')),
                                 # Muestra el grafico de predichos por accidente semanal
                                 tabPanel("Predicción por semana"),
                                 # Muestra el grafico de predichos por accidente mensual
                                 tabPanel("Predicción por mes")
                                 
                               )
                              )
                            ),
                    tabPanel("Agrupamiento", 
                             titlePanel("Seleccione el tipo de incidente:"),
                             
                             sidebarLayout(
                               
                               sidebarPanel(
                                 actionButton("runif", "Promedio de Colisiones"),
                                 actionButton("rnorm", "Colisiones con peatones"),
                                 actionButton("rnorm", "Heridos"), 
                               ),
                               
                               mainPanel(
                                 leafletOutput("mymap"),
                                 p()
                               )
                             
                             )
                    )
                    
                ) # navbarPage
) # fluidPage


# Define server function  
server <- function(input, output) {
    
    output$txtout <- renderText({
        paste(input$daterange3)
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
    
    # Funcion para obtener todas las predicciones de los modelos
    # Tambien obtiene los graficos y las tablas
    control_predictions <- function(fecha_inicio, fecha_fin, tipo = 'comuna'){
      
    } 
    
    # Funcion de prueba
    output$plotpred <- renderPlotly({
      validate(
        need(input$daterange_pred[1] < input$daterange_pred[2],
             "Error: la fecha final no puede ser menor que la fecha de inicio")
      )
      fecha_inicio <- input$daterange_pred[2]
      fecha_fin <- input$daterange_pred[1]
      plot1 <- plot_ly(
        x = rnorm(10),
        y = 1:10, 
        type = 'scatter',
        mode = 'markers')
      }
    )

    
    
} # server


# Create Shiny object
shinyApp(ui = ui, server = server)
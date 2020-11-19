# This is a shiny dashboard

# Cargar paquetes
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
library(raster)
library(dashboardthemes)
library(rgdal)

# Cargar datos 
load(file = 'data/barrio_dia_2019pred.RData',.GlobalEnv)
load(file = 'data/barrio_semana_2019pred.RData',.GlobalEnv)
load(file = 'data/barrio_mes_2019pred.RData',.GlobalEnv)
load(file = 'data/comuna_dia_2019pred.RData',.GlobalEnv)
load(file = 'data/comuna_semana_2019pred.RData',.GlobalEnv)
load(file = 'data/comuna_mes_2019pred.RData',.GlobalEnv)

# Cargar mapa
#load('maps/medellin_map.RData',.GlobalEnv)

# Cargar datos
load(file = 'datos.RData', .GlobalEnv)

# listas para nombre de barrios y comunas
list_barrios <- sort(unique(datos$BARRIO))
list_comunas <- sort(unique(datos$COMUNA))

source('source/source_models.R')


header <-   dashboardHeader(  ### changing logo
    title = HTML("Predicciòn ccidentalidad"),
    titleWidth = 450
)


header$children[[2]]$children[[2]] <- header$children[[2]]$children[[1]]
header$children[[2]]$children[[1]] <- tags$a(tags$img(src='transitapp.png',width = "50%", height = "50%"),
                                             target = '_blank') 
ui <- dashboardPage(header,
                    
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
                        tabItems(
                            
                            
                            # Second tab content
                            tabItem(tabName = "acc",
                                    h1("Total de accidentes de tránsito", align = 'center'),
                                    h5("A continuación podrá seleccionar un periodo de tiempo para obtener el total de accidentes 
                 por día, semana y mes. Recuerde que de 2014 a 2018 se mostrarán los datos reales, a partir de 2018 se
                 realizan predicciones"),
                                    fluidRow(
                                        box(width = 4,
                                            dateRangeInput("daterange_pred", "Rango de Tiempo:",
                                                           start  = "2014-01-01",
                                                           end    = "2019-12-31",
                                                           min    = "2014-01-01",
                                                           max    = "2019-12-31",
                                                           format = "dd/mm/yyyy",
                                                           separator = " - ",
                                                           language = "es")
                                        ),
                                        
                                        box(width = 4 ,
                                            selectInput("tipo_modelo", "Seleccione el tipo de modelo",
                                                        c(Comuna = "comuna"
                                                          ,Barrio = "barrio")
                                            )
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
                                            conditionalPanel(
                                                condition = "input.tipo_modelo == 'comuna'",
                                                tabsetPanel(
                                                    # Muestra el grafico de predichos por accidente diarios
                                                    tabPanel("Accidentalidad por dia",
                                                             h1(),
                                                             plotlyOutput("plotpred_dia") %>% shinycssloaders::withSpinner(color="#3498db"),
                                                             h1(),
                                                             DTOutput("dfpred_dia")),
                                                    # Muestra el grafico de predichos por accidente semanal
                                                    tabPanel("Accidentalidad por semana",
                                                             h1(),
                                                             plotlyOutput("plotpred_semana") %>% shinycssloaders::withSpinner(color="#3498db"),
                                                             h1(),
                                                             DTOutput("dfpred_semana")),
                                                    # Muestra el grafico de predichos por accidente mensual
                                                    tabPanel("Accidentalidad por mes",
                                                             h1(),
                                                             plotlyOutput("plotpred_mes") %>% shinycssloaders::withSpinner(color="#3498db"),
                                                             h1(),
                                                             DTOutput("dfpred_mes"))
                                                    
                                                )
                                            ),
                                            conditionalPanel(
                                                condition = "input.tipo_modelo == 'barrio'",
                                                tabsetPanel(
                                                    # Muestra el grafico de predichos por accidente diarios
                                                    tabPanel("Predicción por dia",
                                                             h1(),
                                                             plotlyOutput("plotpred_diab") %>% shinycssloaders::withSpinner(color="#3498db"),
                                                             h1(),
                                                             DTOutput("dfpred_diab")),
                                                    # Muestra el grafico de predichos por accidente semanal
                                                    tabPanel("Predicción por semana",
                                                             h1(),
                                                             plotlyOutput("plotpred_semanab") %>% shinycssloaders::withSpinner(color="#3498db"),
                                                             h1(),
                                                             DTOutput("dfpred_semanab")),
                                                    # Muestra el grafico de predichos por accidente mensual
                                                    tabPanel("Predicción por mes",
                                                             h1(),
                                                             plotlyOutput("plotpred_mesb") %>% shinycssloaders::withSpinner(color="#3498db"),
                                                             h1(),
                                                             DTOutput("dfpred_mesb"))
                                                    
                                                )
                                            )
                                            
                                        ),
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
    
    comuna_pred_dia <- reactive({
        validate(need(input$daterange_pred[1] < input$daterange_pred[2],
                      "Error: la fecha final no puede ser menor que la fecha de inicio"))
        pred_comuna_dia(fecha_inicio = input$daterange_pred[1],
                        fecha_fin = input$daterange_pred[2],
                        nombre = input$nombre_comuna)
    })
    
    # Funcion para predicciones de semana
    comuna_pred_semana <- reactive({
        validate(need(input$daterange_pred[1] < input$daterange_pred[2],
                      "Error: la fecha final no puede ser menor que la fecha de inicio"))
        pred_comuna_semana(fecha_inicio = input$daterange_pred[1],
                           fecha_fin = input$daterange_pred[2],
                           nombre = input$nombre_comuna)
    })
    # Funcion para predicciones de dia
    comuna_pred_mes <- reactive({
        validate(need(input$daterange_pred[1] < input$daterange_pred[2],
                      "Error: la fecha final no puede ser menor que la fecha de inicio"))
        pred_comuna_mes(fecha_inicio = input$daterange_pred[1],
                        fecha_fin = input$daterange_pred[2],
                        nombre = input$nombre_comuna)
    })
    # Funcion para predicciones de dia
    barrio_pred_dia <- reactive({
        validate(need(input$daterange_pred[1] < input$daterange_pred[2],
                      "Error: la fecha final no puede ser menor que la fecha de inicio"))
        pred_barrio_dia(fecha_inicio = input$daterange_pred[1],
                        fecha_fin = input$daterange_pred[2],
                        nombre = input$nombre_comuna)
    })
    # Funcion para predicciones de dia
    barrio_pred_semana <- reactive({
        validate(need(input$daterange_pred[1] < input$daterange_pred[2],
                      "Error: la fecha final no puede ser menor que la fecha de inicio"))
        pred_barrio_semana(fecha_inicio = input$daterange_pred[1],
                           fecha_fin = input$daterange_pred[2],
                           nombre = input$nombre_comuna)
    })
    # Funcion para predicciones de dia
    barrio_pred_mes <- reactive({
        validate(need(input$daterange_pred[1] < input$daterange_pred[2],
                      "Error: la fecha final no puede ser menor que la fecha de inicio"))
        pred_barrio_mes(fecha_inicio = input$daterange_pred[1],
                        fecha_fin = input$daterange_pred[2],
                        nombre = input$nombre_comuna)
    })
    
    # Grafica de dia
    output$plotpred_dia <- renderPlotly({
        gp <- comuna_pred_dia()
        gp$fig
    })
    
    # Grafica de semana
    output$plotpred_semana <- renderPlotly({
        gp <- comuna_pred_semana()
        gp$fig
    })
    
    # Grafica de mes
    output$plotpred_mes <- renderPlotly({
        gp <- comuna_pred_mes()
        gp$fig
    })
    
    # datos de dia
    output$dfpred_dia <- renderDT({
        gp <- comuna_pred_dia()
        DT::datatable({
            gp$df
        },
        options = list(lenghtMenu = list(c(7, 15, -1), c('5', '15', 'All')), pageLenght = 15),
        filter = "top",
        selection = "multiple",
        style = "bootstrap"
        )
        
    })
    
    # datos de semana
    output$dfpred_semana <- renderDT({
        gp <- comuna_pred_semana()
        DT::datatable({
            gp$df
        },
        options = list(lenghtMenu = list(c(7, 15, -1), c('5', '15', 'All')), pageLenght = 15),
        filter = "top",
        selection = "multiple",
        style = "bootstrap"
        )
        
    })
    
    # datos de mes
    output$dfpred_mes <- renderDT({
        gp <- comuna_pred_mes()
        DT::datatable({
            gp$df
        },
        options = list(lenghtMenu = list(c(7, 15, -1), c('5', '15', 'All')), pageLenght = 15),
        filter = "top",
        selection = "multiple",
        style = "bootstrap"
        )
        
    })
    
    # Grafica de dia
    output$plotpred_diab <- renderPlotly({
        gp <- barrio_pred_dia()
        gp$fig
    })
    
    # Grafica de semana
    output$plotpred_semanab <- renderPlotly({
        gp <- barrio_pred_semana()
        gp$fig
    })
    
    # Grafica de mes
    output$plotpred_mesb <- renderPlotly({
        gp <- barrio_pred_mes()
        gp$fig
    })
    
    # datos de dia
    output$dfpred_diab <- renderDT({
        gp <- barrio_pred_dia()
        DT::datatable({
            gp$df
        },
        options = list(lenghtMenu = list(c(7, 15, -1), c('5', '15', 'All')), pageLenght = 15),
        filter = "top",
        selection = "multiple",
        style = "bootstrap"
        )
        
    })
    
    # datos de semana
    output$dfpred_semanab <- renderDT({
        gp <- barrio_pred_semana()
        DT::datatable({
            gp$df
        },
        options = list(lenghtMenu = list(c(7, 15, -1), c('5', '15', 'All')), pageLenght = 15),
        filter = "top",
        selection = "multiple",
        style = "bootstrap"
        )
        
    })
    
    # datos de mes
    output$dfpred_mesb <- renderDT({
        gp <- barrio_pred_mes()
        DT::datatable({
            gp$df
        },
        options = list(lenghtMenu = list(c(7, 15, -1), c('5', '15', 'All')), pageLenght = 15),
        filter = "top",
        selection = "multiple",
        style = "bootstrap"
        )
        
    })
}

shinyApp(ui, server)
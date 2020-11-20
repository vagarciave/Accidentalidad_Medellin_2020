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
load(file = 'comuna_dia_2019pred.RData',.GlobalEnv)
load(file = 'comuna_semana_2019pred.RData',.GlobalEnv)
load(file = 'comuna_mes_2019pred.RData',.GlobalEnv)
load(file = 'historicos.RData',.GlobalEnv)

# Cargar mapa
load('medellin_map.RData',.GlobalEnv)

# Cargar datos
load(file = 'datos.RData', .GlobalEnv)

# listas para nombre de barrios y comunas
list_barrios <- sort(unique(datos$BARRIO))
list_comunas <- sort(unique(datos$COMUNA))

source('source_models.R')
source('source_map.R')


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
ui <- dashboardPage(header,

                    dashboardSidebar(
                      sidebarMenu(
                        menuItem("Inicio", tabName = "inicio", icon = icon("home")),
                        menuItem("Tabla de datos históricos", tabName = "datos", icon = icon("table")),
                        menuItem("Visualizacion", tabName = "visualizacion", icon = icon("chart-bar")),
                        menuItem("Accidentalidad", tabName = "acc",icon = icon("car-crash")),
                        menuItem("Agrupamiento", tabName = "agrupamiento", icon = icon("map-marked-alt")),
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
                        
                        tabItem(tabName = "visualizacion",
                                h1("Visualización de datos",align = "center"),
                                fluidRow(
                                  box(width = 12,
                                      dateRangeInput("daterange2", "Rango de Tiempo:",
                                                     start  = "2014-01-01",
                                                     end    = "2018-12-31",
                                                     min    = "2014-01-01",
                                                     max    = "2018-12-31",
                                                     format = "dd/mm/yyyy",
                                                     separator = " - ",
                                                     language = "es")
                                  )
                                ),
                                
                                fluidRow(
                                  box(width = 4,
                                      plotlyOutput("chart_clase")%>% shinycssloaders::withSpinner(color="#3498db")),
                                  box(width = 4,
                                      plotlyOutput("chart_gravedad")%>% shinycssloaders::withSpinner(color="#3498db")),
                                  box(width = 4,
                                      plotlyOutput("chart_diseno")%>% shinycssloaders::withSpinner(color="#3498db"))
                                  
                                ),
                                fluidRow(
                                  box(width = 6,
                                      plotlyOutput("chart_comuna")%>% shinycssloaders::withSpinner(color="#3498db")),
                                  box(width = 6,
                                      plotlyOutput("chart_barrio")%>% shinycssloaders::withSpinner(color="#3498db"))
                                )
                                
                        ),
                        
                      
                        # Second tab content
                        tabItem(tabName = "acc",
                                h1("Total de accidentes de tránsito", align = 'center'),
                                h5("A continuación podrá seleccionar un periodo de tiempo para obtener el total de accidentes 
                 por día, semana y mes. Recuerde que de 2014 a 2018 se mostrarán los datos reales, a partir de 2019 se
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
                                          # Muestra el grafico de predichos por accidente mensual
                                          tabPanel("Accidentalidad por mes",
                                                   h1(),
                                                   plotlyOutput("plotpred_mes") %>% shinycssloaders::withSpinner(color="#3498db")
                                          ),
                                          # Muestra el grafico de predichos por accidente semanal
                                          tabPanel("Accidentalidad por semana",
                                                   h1(),
                                                   plotlyOutput("plotpred_semana") %>% shinycssloaders::withSpinner(color="#3498db")
                                                  ),
                                          
                                          tabPanel("Accidentalidad por dia",
                                                   h1(),
                                                   plotlyOutput("plotpred_dia") %>% shinycssloaders::withSpinner(color="#3498db")
                                                  )
                                        )
                                      ),
                                      conditionalPanel(
                                        condition = "input.tipo_modelo == 'barrio'",
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

                                  ),
                                )
                                
                                
                        ),
                        # Second tab content
                        tabItem(tabName = "agrupamiento",
                                h1("Agrupamiento de barrios en Medellín"),
                                h5("El siguiente mapa muestra los barrios de Medellín agrupados por similaridades, cada grupo tiene características
                                diferentes de acuerdo al número de accidentes que se presentaron del 2014 al 2018."),
                                h5("Puede seleccionar un barrio en el mapa y obtener información histórica del mismo, además se presenta una tabla 
                                   con la información de cada grupo."),
                                fluidPage(
                                  column(width = 12,
                                         box(width = NULL, solidHeader = TRUE,
                                             leafletOutput("mymap")  %>% shinycssloaders::withSpinner(color="#3498db"),
                                             p()
                                         ),
                                         box(width = NULL,
                                             uiOutput("clusters_table") %>% shinycssloaders::withSpinner(color="#3498db"))
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
  output$plotpred_dia <- renderPlotly({
    validate(need(input$daterange_pred[1] < input$daterange_pred[2],
                  "Error: la fecha final no puede ser menor que la fecha de inicio"))
    pred_comuna_dia(fecha_inicio = input$daterange_pred[1],
                    fecha_fin = input$daterange_pred[2],
                    nombre = input$nombre_comuna)
  })
  
  # Grafica de semana
  output$plotpred_semana <- renderPlotly({
    validate(need(input$daterange_pred[1] < input$daterange_pred[2],
                  "Error: la fecha final no puede ser menor que la fecha de inicio"))
    pred_comuna_semana(fecha_inicio = input$daterange_pred[1],
                       fecha_fin = input$daterange_pred[2],
                       nombre = input$nombre_comuna)
  })
  
  # Grafica de mes
  output$plotpred_mes <- renderPlotly({
    validate(need(input$daterange_pred[1] < input$daterange_pred[2],
                  "Error: la fecha final no puede ser menor que la fecha de inicio"))
    pred_comuna_mes(fecha_inicio = input$daterange_pred[1],
                    fecha_fin = input$daterange_pred[2],
                    nombre = input$nombre_comuna)
  })
  

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

  #Datos de visualizacion
  output$Data <- DT::renderDataTable(
    DT::datatable({
      datos %>% filter(ymd(FECHA) >= input$daterange[1], ymd(FECHA) <= input$daterange[2]) %>%
        group_by(FECHA,CLASE,GRAVEDAD,COMUNA,BARRIO) %>%
        summarise("TOTAL ACCIDENTES" = n())
    },
    options = list(lenghtMenu = list(c(7, 15, -1), c('5', '15', 'All')), pageLenght = 15),
    filter = "top",
    selection = "multiple",
    style = "bootstrap"
    ))
  
  # Mapa
  output$mymap <- renderLeaflet({
    create_map()
  })
################Tabla para el mapa########
  output$clusters_table <- renderUI({
    dirColors <-c("1"="#EE3E32", "2"="#F68838", "3"="#FBB021", "4"="#1B8A5A", "5"="#D0C7C7")
    
    # Create a Bootstrap-styled table
    tags$table(class = "table",
               tags$thead(tags$tr(
                 tags$th("Color"),
                 tags$th("Nombre"),
                 tags$th("Promedio de muertos"),
                 tags$th("Promedio de heridos"),
                 tags$th("Promedio de solo daños")
                 
               )),
               tags$tbody(
                 tags$tr(
                   tags$td(span(style = sprintf(
                     "width:1.1em; height:1.1em; background-color:%s; display:inline-block;",
                     dirColors[1]
                   ))),
                   tags$td("Accidentalidad alta"),
                   tags$td('23.44'),
                   tags$td('1773.11'),
                   tags$td('2149.11')
                 ),
                 tags$tr(
                   tags$td(span(style = sprintf(
                     "width:1.1em; height:1.1em; background-color:%s; display:inline-block;",
                     dirColors[2]
                   ))),
                   tags$td("Accidentalidad media"),
                   tags$td('12.35'),
                   tags$td('1050.35'),
                   tags$td('1142.87')
                 ),
                 tags$tr(
                   tags$td(span(style = sprintf(
                     "width:1.1em; height:1.1em; background-color:%s; display:inline-block;",
                     dirColors[3]
                   ))),
                   tags$td("Accidentalidad moderada"),
                   tags$td('5.3'),
                   tags$td('550.34'),
                   tags$td('382.37')
                 ),
                 tags$tr(
                   tags$td(span(style = sprintf(
                     "width:1.1em; height:1.1em; background-color:%s; display:inline-block;",
                     dirColors[4]
                   ))),
                   tags$td("Accidentalidad baja"),
                   tags$td('1.71'),
                   tags$td('171.6'),
                   tags$td('104.78')
                 ),
                 tags$tr(
                   tags$td(span(style = sprintf(
                     "width:1.1em; height:1.1em; background-color:%s; display:inline-block;",
                     dirColors[5]
                   ))),
                   tags$td("Sin información"),
                   tags$td("Sin información"),
                   tags$td("Sin información"),
                   tags$td("Sin información")
                 )
               )
    )
  })
########### Para gráficos del dash ########
  output$chart_clase <- renderPlotly({
    datos %>% filter(ymd(FECHA) >= input$daterange2[1],
                     ymd(FECHA) <= input$daterange2[2]) %>%
      group_by(CLASE) %>% 
      summarise(TOTAL = n())  %>% arrange(desc(TOTAL)) %>%
      plot_ly(x = ~ CLASE, y = ~ TOTAL,color = ~CLASE, type = 'bar')   %>%
      layout(
        title = "Total de accidentes por Clase",
        xaxis = list(title = "",
                     categoryorder = "array",
                     categoryarray = ~CLASE),
        yaxis = list(title = "Total")
      )
  })
  # Grafico gravedad
  output$chart_gravedad<- renderPlotly({
    datos %>% filter(ymd(FECHA) >= input$daterange2[1],
                     ymd(FECHA) <= input$daterange2[2]) %>%
      group_by(GRAVEDAD) %>% 
      summarise(TOTAL = n())  %>% arrange(desc(TOTAL)) %>%
      plot_ly(labels = ~GRAVEDAD, values = ~TOTAL, type = 'pie') %>% 
      layout(title = "Total de accidentes por gravedad",
             xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
             yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
  })
  #Grafico comuna
  output$chart_comuna <- renderPlotly({
    datos %>% filter(ymd(FECHA) >= input$daterange2[1],
                     ymd(FECHA) <= input$daterange2[2]) %>%
      group_by(COMUNA) %>% 
      summarise(TOTAL = n())  %>% arrange(desc(TOTAL)) %>%
      plot_ly(x = ~ COMUNA, y = ~ TOTAL,color = ~COMUNA, type = 'bar')  %>%
      layout(
        title = "Total de accidentes por Comuna",
        xaxis = list(title = "",
                     categoryorder = "array",
                     categoryarray = ~COMUNA),
        yaxis = list(title = "Total")
      )
  })
  
  #Grafico comuna
  output$chart_barrio <- renderPlotly({
    datos %>% filter(ymd(FECHA) >= input$daterange2[1],
                     ymd(FECHA) <= input$daterange2[2]) %>%
      group_by(BARRIO) %>% 
      summarise(TOTAL = n())  %>% arrange(desc(TOTAL)) %>%
      top_n(10) %>%
      plot_ly(x = ~ BARRIO, y = ~ TOTAL,color = ~BARRIO, type = 'bar')  %>%
      layout(
        title = "Top 10 barrios con más accidentes",
        xaxis = list(title = "",
                     categoryorder = "array",
                     categoryarray = ~BARRIO),
        yaxis = list(title = "Total")
      )
  })
  
  # Grafico diseño
  output$chart_diseno <- renderPlotly(({
    df_diseno <- datos %>% filter(ymd(FECHA) >= input$daterange2[1],
                        ymd(FECHA) <= input$daterange2[2]) %>%
      group_by(DISENO) %>% 
      summarise(TOTAL = n())  %>% arrange(desc(TOTAL)) %>%
      arrange(desc(TOTAL))
    fig <- plot_ly(
      type = 'table',
      header = list(
        values = c('<b>DISEÑO</b>', '<b>Total</b>'),
        line = list(color = '#506784'),
        fill = list(color = '#119DFF'),
        align = c('left','center'),
        font = list(color = 'white', size = 12)
      ),
      cells = list(
        values = t(df_diseno),
        line = list(color = '#506784'),
        fill = list(color = c('#d0eeec', 'white')),
        align = c('left', 'center'),
        font = list(color = c('black'), size = 12)
      ))
    fig
  }))
  
}

shinyApp(ui, server)

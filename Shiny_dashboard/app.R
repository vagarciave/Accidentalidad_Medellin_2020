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
library(raster)
library(dashboardthemes)

youtube_video <- '<iframe width="560" height="315" src="https://www.youtube.com/watch?v=iX-QaNzd-0Y" frameborder="0" allow="accelerometer; autoplay; encrypted-media; gyroscope; picture-in-picture" allowfullscreen></iframe>'

# Cargar valores ajustados
load(file = 'accidentes_dia_barrio.RData',.GlobalEnv)
load(file = 'accidentes_dia_comuna.RData',.GlobalEnv)

datos <- read.csv("Base_definitiva.csv", encoding = 'UTF-8', stringsAsFactors=T)
datos <- subset( datos, select = -c(DIA, MES, PERIODO, DIA_FESTIVO, SEMANA_MES ) )

# listas para nombre de barrios y comunas
list_barrios <- sort(unique(datos$BARRIO))
list_comunas <- sort(unique(datos$COMUNA))

source('source_models.R')
source('source_map.R')


ui <- dashboardPage(
                    
  dashboardHeader(  ### changing logo
    title = shinyDashboardLogo(
      theme = "poor_mans_flatly",
      boldText = "Accidentalidad" ,
      mainText = "Medellin",
      badgeText = "2014-2018"
    ),
    titleWidth = 350
    ),
  ## Sidebar content
  dashboardSidebar(
    sidebarMenu(
      menuItem("Inicio", tabName = "inicio", icon = icon("home")),
      menuItem("Tabla de datos históricos", tabName = "datos", icon = icon("table")),
      menuItem("Visualizacion", tabName = "visualizacion", icon = icon("chart-bar")),
      menuItem("Prediccion", tabName = "prediccion", icon = icon("car-crash")),
      menuItem("Agrupamiento", tabName = "agrupamiento", icon = icon("map-marked-alt")),
      menuItem("Equipo", tabName = "equipo", icon = icon("users"))
    )
  ),
  ## Body content
  dashboardBody(
    ### changing theme
    shinyDashboardThemes(
      theme = "poor_mans_flatly"
    ),
    tabItems(
      # First tab content
      # First tab content
      tabItem(tabName = "inicio",
              # En esta parte de inicio se pone el video
              # la descripcion de la app
              h1("¡Bienvenidos!",
                 align = "center"),
              h3("En esta aplicación web encontrarás información histórica de accidentes de tránsito en Medellín 
                 desde el año 2014 a 2018. Aquí podrás visualizar los datos históricos de accidentalidad, un mapa
                 donde se encuentran los barrios de Medellín agrupados de acuerdo al número de accidentes, además
                 nuestra herramienta te permitirá realizar predicciones de accidentes, por barrio, comuna y clase 
                 de accidente."),
              br(),
              h3("A continuación un video donde se explica como utilizar las diferentes herramientas de la 
                 aplicación web"),
              tags$div(class = "col-md-10  col-md-offset-1 videoWrapper",HTML(youtube_video))
      ),

     tabItem(tabName = "datos",
             h1("Incidentes Georreferenciados", align = "center"),
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
             fluidRow(box(DT::dataTableOutput("Data")  %>% shinycssloaders::withSpinner(color="#0dc5c1"), 
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
                   plotlyOutput("chart_clase")),
               box(width = 4,
                   plotlyOutput("chart_gravedad")),
               box(width = 4,
                   plotlyOutput("chart_diseno"))
               
             ),
             fluidRow(
               box(width = 6,
                   plotlyOutput("chart_comuna")),
               box(width = 6,
                   plotlyOutput("chart_barrio"))
             )

     ),
     
      # Second tab content
      tabItem(tabName = "prediccion",
              h1("Predicción de accidentes de tránsito", align = 'center'),
              h4("A continuación podrá seleccionar un periodo de tiempo para obtener el total de accidentes 
                 por día, semana y mes. Recuerde que de 2014 a 2018 se mostrarán los datos reales, a partir de 2018 se
                 realizan predicciones"),
              fluidRow(
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
              )
      ),
      # Second tab content
      tabItem(tabName = "agrupamiento",
              h1("Agrupamiento de barrios en Medellín"),
              h4("El siguiente mapa muestra los barrios de Medellín divididos en clusters,
                 cada cluster tiene carácteristicas diferentes de acuerdo al número de accidentes
                 que se presentaron de 2014 a 2018."),
              h4("Puede seleccionar un barrio en el mapa y obtener la información histórica del mismo,
                 además se presenta una tabla con la información de cada cluster"),
                fluidPage(
                  column(width = 12,
                         box(width = NULL, solidHeader = TRUE,
                             leafletOutput("mymap"),
                             p()
                         ),
                         box(width = NULL,
                          uiOutput("clusters_table"))
                  )
              )
       ),
     tabItem(tabName = "equipo",
             h2("Daniel Chanci Restrepo", align = "center"),
             h4('Ingeniería de sistemas', align = "center"),
             br(),
             h2("Valentina Garcia Velásquez", align = "center"),
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
  
  output$txtout <- renderText({
    paste(weekdays(ymd(input$daterange[1])),ymd(input$daterange[2]), sep = ',')
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
  
  #Tabla para el mapa
  output$clusters_table <- renderUI({
    dirColors <-c("1"="#EE3E32", "2"="#F68838", "3"="#FBB021", "4"="#1B8A5A")
    
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
                 )
          )
     )
  })
  # Para gráficos del dash
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
        title = "Total de accidentes \n Top 5 barrios con más accidentes",
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
      summarise(TOTAL = n())  %>% arrange(desc(TOTAL)) 
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
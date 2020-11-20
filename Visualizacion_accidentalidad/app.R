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

load(file = 'datos.RData', .GlobalEnv)


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
ui <- dashboardPage(title = "Visualizazcion accidentalidad",
                    header,
                    
                    dashboardSidebar(
                        sidebarMenu(
                            menuItem("Visualizacion", tabName = "visualizacion", icon = icon("chart-bar"))
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
                                    
                            )
                            
                        ) # tabItems
                    ) # DashboardBody
) # dashboardBody

server <- function(input, output) {


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

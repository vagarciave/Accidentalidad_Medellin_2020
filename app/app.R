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
                                               "Incendio" = "Incendio")),
                                 submitButton("Visualizar", icon("car-crash")),
                                 
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
                                             width=12))
                             ),
                    tabPanel("Prediccion", "This panel is intentionally left blank"),
                    tabPanel("Agrupamiento", "This panel is intentionally left blank")
                    
                ) # navbarPage
) # fluidPage




datos <- read.csv("Base_Definiva.csv", encoding = 'UTF-8', stringsAsFactors=T)
datos$FECHA <- as.Date(datos$FECHA) 
datos <- subset( datos, select = -c(DIA, MES, PERIODO, HORA) )
# datos <- datos[sample(1:dim(datos)[1],1000),]

# Define server function  
server <- function(input, output) {
    
    output$txtout <- renderText({
        paste( input$txt1, input$txt2, sep = " " )
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
    
} # server


# Create Shiny object
shinyApp(ui = ui, server = server)
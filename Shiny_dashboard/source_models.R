# Source code for model prediction

# Esta funcion retorna los accidentes observados y/o predichos
control_prediction <- function(fecha_inicio, fecha_fin, tipo_modelo){
  if(tipo_modelo == 'comuna'){
    filter_data <- accidentes_dia_comuna %>% filter(COMUNA == nombre,
                                                    ymd(FECHA) >= fecha_inicio,
                                                    ymd(FECHA) <= fecha_fin) 
    filter_data_dia <- filter_data %>% 
      group_by(COMUNA,CLASE,FECHA) %>% 
      summarise(Total = sum(ACCIDENTALIDAD))
    
    filter_data_semana <- filter_data %>% 
      group_by(COMUNA,CLASE,SEMANA) %>% 
      summarise(Total = sum(ACCIDENTALIDAD))
    
  }else if(tipo_modelo == 'barrio'){
    filter_data <- accidentes_dia_barrio %>% filter(BARRIO == nombre_barrio,
                                                    ymd(FECHA) >= fecha_inicio,
                                                    ymd(FECHA) <= fecha_fin) 
    filter_data_dia <- filter_data %>% 
      group_by(BARRIO,CLASE,FECHA) %>% 
      summarise(Total = sum(ACCIDENTALIDAD))
    
    filter_data_semana <- filter_data %>% 
      group_by(BARRIO,CLASE,SEMANA) %>% 
      summarise(Total = sum(ACCIDENTALIDAD))
  }
  pron <- ifelse(tipo_modelo=='comuna','la','el')
  titulo <- paste('Total de accidentes para',pron, tipo_modelo, nombre)
  fig_clase_dia <- filter_data_dia %>% 
    plot_ly(x = ~FECHA, y = ~ Total,color = ~CLASE, type = 'scatter', mode = 'lines+markers')   %>%
    layout(title = titulo,
      xaxis = list(title = "",
                   categoryorder = "array",
                   categoryarray = ~CLASE),
      yaxis = list(title = "Total")
    )
  
  fig_clase_semana <- filter_data_semana %>% 
    plot_ly(x = ~SEMANA, y = ~ Total,color = ~CLASE, type = 'scatter', mode = 'lines+markers')   %>%
    layout(title = titulo,
           xaxis = list(title = "",
                        categoryorder = "array",
                        categoryarray = ~CLASE),
           yaxis = list(title = "Total")
    )
  return(fig_clase_dia)
}

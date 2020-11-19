# Source code for model prediction

# Comuna dia
pred_comuna_dia <- function(fecha_inicio, fecha_fin,nombre){
  df <- comuna_dia_2019pred[,c('COMUNA','CLASE','FECHA','ACCIDENTALIDAD')] %>%
    filter(COMUNA == nombre,ymd(FECHA) >= fecha_inicio,ymd(FECHA) <= fecha_fin) 
  titulo <- paste('Accidentalidad en la comuna',nombre)
  fig <- df %>% ungroup() %>%
    plot_ly(x = ~FECHA, y = ~ ACCIDENTALIDAD,
            color = ~CLASE,
            type = 'scatter',
            mode = 'lines+markers')   %>%
    layout(title = titulo,
           xaxis = list(title = "",
                        categoryorder = "array",
                        categoryarray = ~CLASE),
           yaxis = list(title = "ACCIDENTALIDAD")
    )
  return(list(df = df, fig = fig))
}

#Comuna semana

pred_comuna_semana <- function(fecha_inicio, fecha_fin,nombre){
  inicio <- week(fecha_inicio)
  fin <- week(fecha_fin)
  yinicio <- year(fecha_inicio)
  yfin <- year(fecha_fin)
  df <- comuna_semana_2019pred %>%
    filter(COMUNA == nombre,
           NSEMANA >= inicio,
           NSEMANA <= fin,
           PERIODO >= yinicio,
           PERIODO <= yfin) 
  
  df <- df[,c('COMUNA','CLASE','SEMANA','ACCIDENTALIDAD')]
  titulo <- paste('Accidentalidad en la comuna',nombre)
  fig <- df %>% ungroup() %>%
    plot_ly(x = ~SEMANA, y = ~ ACCIDENTALIDAD,
            color = ~CLASE,
            type = 'scatter', mode = 'lines+markers')   %>%
    layout(title = titulo,
           xaxis = list(title = "",
                        categoryorder = "array",
                        categoryarray = ~CLASE),
           yaxis = list(title = "ACCIDENTALIDAD")
    )
  return(list(df = df, fig = fig))
}

# comuna mes
pred_comuna_mes <- function(fecha_inicio, fecha_fin, nombre){
  inicio <- month(fecha_inicio)
  fin <- month(fecha_fin)
  yinicio <- year(fecha_inicio)
  yfin <- year(fecha_fin)
  df <- comuna_mes_2019pred %>%
    filter(COMUNA == nombre,
           NMES >= inicio,
           NMES <= fin,
           PERIODO >= yinicio,
           PERIODO <= yfin) 
  
  df <- df[,c('COMUNA','CLASE','MES','ACCIDENTALIDAD')]
  titulo <- paste('Accidentalidad en la comuna',nombre)
  fig <- df %>% ungroup() %>% 
    plot_ly(x = ~MES, y = ~ ACCIDENTALIDAD,color = ~CLASE, type = 'scatter', mode = 'lines+markers')   %>%
    layout(title = titulo,
           xaxis = list(title = "",
                        categoryorder = "array",
                        categoryarray = ~CLASE),
           yaxis = list(title = "ACCIDENTALIDAD")
    )
  return(list(df = df, fig = fig))
}

# barrio dia
pred_barrio_dia <- function(fecha_inicio, fecha_fin, nombre){
  df <- barrio_dia_2019pred %>%
    filter(BARRIO == nombre,ymd(FECHA) >= fecha_inicio,ymd(FECHA) <= fecha_fin) 
  df <- df[,c('BARRIO','CLASE','FECHA','ACCIDENTALIDAD')] 
  titulo <- paste('Accidentalidad en el barrio',nombre)
  fig <- df %>% ungroup() %>%
    plot_ly(x = ~FECHA, y = ~ ACCIDENTALIDAD,color = ~CLASE, type = 'scatter', mode = 'lines+markers')   %>%
    layout(title = titulo,
           xaxis = list(title = "",
                        categoryorder = "array",
                        categoryarray = ~CLASE),
           yaxis = list(title = "ACCIDENTALIDAD")
    )
  return(list(df = df, fig = fig))
}

# barrio semana 
pred_barrio_semana <- function(fecha_inicio, fecha_fin, nombre){
  
  inicio <- week(fecha_inicio)
  fin <- week(fecha_fin)
  yinicio <- year(fecha_inicio)
  yfin <- year(fecha_fin)
  df <- barrio_semana_2019pred %>%
    filter(BARRIO == nombre,
           NSEMANA >= inicio,
           NSEMANA <= fin,
           PERIODO >= yinicio,
           PERIODO <= yfin) 
  
  df <- df[,c('BARRIO','CLASE','SEMANA','ACCIDENTALIDAD')]
  titulo <- paste('Accidentalidad en el barrio',nombre)
  fig <- df %>% ungroup() %>%
    plot_ly(x = ~SEMANA, y = ~ ACCIDENTALIDAD,color = ~CLASE, type = 'scatter', mode = 'lines+markers')   %>%
    layout(title = titulo,
           xaxis = list(title = "",
                        categoryorder = "array",
                        categoryarray = ~CLASE),
           yaxis = list(title = "ACCIDENTALIDAD")
    )
  return(list(df = df, fig = fig))
  
}


# Barrio mes
pred_barrio_mes <- function(fecha_inicio, fecha_fin, nombre){
  inicio <- month(fecha_inicio)
  fin <- month(fecha_fin)
  yinicio <- year(fecha_inicio)
  yfin <- year(fecha_fin)
  df <- barrio_mes_2019pred %>%
    filter(BARRIO == nombre,
           NMES >= inicio,
           NMES <= fin,
           PERIODO >= yinicio,
           PERIODO <= yfin) 
  titulo <- paste('Accidentalidad en el barrio',nombre)
  df <- df[,c('BARRIO','CLASE','MES','ACCIDENTALIDAD')]
  
  fig <- df %>% ungroup() %>% 
    plot_ly(x = ~MES, y = ~ ACCIDENTALIDAD,color = ~CLASE, type = 'scatter', mode = 'lines+markers')   %>%
    layout(title = titulo,
           xaxis = list(title = "",
                        categoryorder = "array",
                        categoryarray = ~CLASE),
           yaxis = list(title = "ACCIDENTALIDAD")
    )
  return(list(df = df, fig = fig))
  
}

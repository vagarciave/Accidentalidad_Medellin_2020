create_map <- function(){
  # Leer datos
  medellin_map_location <-"Barrio_Vereda.shp"
  # Datos con los clusters
  df_coloring <- read.csv("clusters_final-2.csv", header = TRUE, fileEncoding = "UTF-8")[,-1]
  barrios_med <- shapefile(medellin_map_location,
                           encoding="UTF-8",
                           use_iconv=TRUE)
  
  colnames(df_coloring)[2] <- "NOMBRE"
  id_x <- match(barrios_med@data$NOMBRE,df_coloring$NOMBRE)
  # Agregar seguridad 
  barrios_med@data$NOMBRE_CLUSTER <- df_coloring$NOMBRE_CLUSTER[id_x]
  barrios_med@data$HERIDOS <- df_coloring$HERIDOS[id_x]
  barrios_med@data$MUERTOS <- df_coloring$MUERTOS[id_x]
  barrios_med@data$SOLO_DAÑOS <- df_coloring$SOLO_DAÑOS[id_x]
  
  pal <- colorFactor(c("red","#ED683C","yellow","green","#D0C7C7"), 
                     levels = c("Accidentalidad Alta",
                                "Accidentalidad Media",
                                "Accidentalidad moderada",
                                "Accidentalidad Baja",
                                NA))
  
  popup <- paste(barrios_med@data$NOMBRE,
                 barrios_med@data$NOMBRE_CLUSTER,
                 paste('Muertos: ',barrios_med@data$MUERTOS),
                 paste('Heridos: ',barrios_med@data$HERIDOS),
                 paste('Solo daños: ',barrios_med@data$SOLO_DAÑOS),
                 sep="<br/>")
  # Crear mapa
  accidentes_barrios <- leaflet(barrios_med)
  accidentes_barrios <- addProviderTiles(accidentes_barrios,provider="OpenStreetMap.Mapnik")
  accidentes_barrios <- addPolygons(accidentes_barrios,
                                    popup=popup,
                                    fillColor = ~pal(barrios_med@data$NOMBRE_CLUSTER),
                                    color= 'black',
                                    weight = 1.5, fillOpacity = 0.6)
  accidentes_barrios
}
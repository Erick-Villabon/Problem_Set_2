#_____________________________________________________________________________#
#                                                                             #
#                      Problem Set 2: Making Money with ML?                       #
#                                                                             #
#_____________________________________________________________________________#

#   Autores: - Erick Villabon                                                  
#            -   
#            - 
#            - 
#
#  Fecha: 22/09/2023 


# - Limpiar espacio de trabajo

rm(list = ls())

# - Librerias y paquetes 

library(pacman)
p_load(rvest, tidyverse, ggplot2, robotstxt, psych, stargazer, boot, plotly, openxlsx)
p_load(tidyverse, # Manipular dataframes
       rio, # Import data easily
       plotly, # Gráficos interactivos
       leaflet, # Mapas interactivos
       rgeos, # Calcular centroides de un poligono
       tmaptools, # geocode_OSM()
       sf, # Leer/escribir/manipular datos espaciales
       osmdata, # Get OSM's data 
       tidymodels) #para modelos de ML

# 1. Actualizatr espacio de trabajo 
setwd("C:/Users/Erick/Desktop/aaa")
getwd()
list.files()

# 1. Importar los Datos
test <- read.csv("test.csv")

train <- read.csv("train.csv")

#analizamos estrujctura de la base
glimpse(test)
glimpse(train)

#revisamos los NAs
sapply(test,function(x) sum(is.na(x)))
sapply(train,function(x) sum(is.na(x)))

#imputamos los datos faltantes con la media
test$rooms %>%
  table(useNA = "ifany") %>%
  prop.table() %>%
  round(3)*100

test$rooms[is.na(test$rooms)] <- mean(test$rooms, na.rm = TRUE)

test$bathrooms %>%
  table(useNA = "ifany") %>%
  prop.table() %>%
  round(3)*100

test$bathrooms[is.na(test$bathrooms)] <-  mean(test$bathrooms, na.rm = TRUE)

#creación de variable dummy

tipo_vivienda <- c("Apartamento", "Casa") 
table(test$property_type)

test$property_type_dicotomica <- ifelse(test$property_type == "Apartamento", 1, 0)

#observamos la locación
leaflet() %>%
  addTiles() %>%
  addCircles(lng = test$lon,
             lat = test$lat)

# Ahora solo nos quedaremos con las observaciones que efectivamente están dentro de Chapinero
limites <- getbb("Localidad Chapinero Bogotá")
test <- test %>%
  filter(
    between(lon, limites[1, "min"], limites[1, "max"]) & 
      between(lat, limites[2, "min"], limites[2, "max"])
  )


test <- test %>%
  mutate(color = case_when(property_type == "Apartamento" ~ "#2A9D8F",
                           property_type == "Casa" ~ "#F4A261"))


# Encontram,os el queremos que sea el centro del mapa 
latitud_central <- mean(test$lat)
longitud_central <- mean(test$lon)

# Creamos el plot
leaflet() %>%
  addTiles() %>%
  setView(lng = longitud_central, lat = latitud_central, zoom = 12) %>%
  addCircles(lng = test$lon, 
             lat = test$lat, 
             col = test$color,
             fillOpacity = 1,
             opacity = 1)



# Primero normalizaremos todo el texto
# Todo en minuscula
test <- test %>%
  mutate(description = str_to_lower(description))
# Eliminamos tildes
test <- test %>%
  mutate(description = iconv(description, from = "UTF-8", to = "ASCII//TRANSLIT"))
# Eliminamos caracteres especiales
test <- test %>%
  mutate(description = str_replace_all(description, "[^[:alnum:]]", " "))
# Eliminamos espacios extras
test <- test %>%
  mutate(description = str_trim(gsub("\\s+", " ", description)))


#Creamos variables a partir de la descripcion
test <- test %>%
  mutate(parqueadero = as.numeric(grepl("parqueadero", test$description)))



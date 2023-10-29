#_____________________________________________________________________________#
#                                                                             #
#                      Problem Set 2: Making Money with ML?                   #
#                                                                             #
#_____________________________________________________________________________#

#   Autores: - Erick Villabon                                                  
#            - Juan Diego Duarte
#
#  Fecha: 30/10/2023 


#___________________________________________________________
#
#                LIMPIEZA DE LAS BASES
#
#___________________________________________________________

# - Limpiar espacio de trabajo

rm(list = ls())

# - Librerias y paquetes 

library(pacman)
p_load(rvest, tidyverse, ggplot2, robotstxt, psych, stargazer, boot, plotly, openxlsx, glmnet,
       rio, leaflet, rgeos, tmaptools, sf, osmdata, tidymodels, writexl, 
       units, randomForest, rattle, spatialsample)

# - Actualizar espacio de trabajo 
#setwd("/Users/juandiego/Desktop/GitHub/Problem_Set_2/stores")
#setwd("C:/Users/Erick/Desktop/Problem_Set_2/stores")
setwd("E:/Problem_Set_2/stores")

getwd()
list.files()

# 1. Importar los Datos
test <- read.csv("test.csv")

train <- read.csv("train.csv")

submission_template <- read.csv("submission_template.csv")

#revisamos los NAs
sapply(test,function(x) sum(is.na(x)))


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

#______________________________________________________________
#imputamos los datos faltantes según la descripción y el título 

# 1. Baños

for (i in 1:nrow(test)) {
  match <- str_match(test$description[i], "(\\d+)(\\s?bano|banos|bao|baos)")
  if (!is.na(match[1])) {
    test$n_banos[i] <- as.integer(match[2])
  } else {
    test$n_banos[i] <- 1
  }
}

test$bathrooms <- ifelse(is.na(test$bathrooms), test$n_banos, test$bathrooms)
summary(test$bathrooms)

#corregimos el 0.1% más alto dado que son valores atípicos

Q1 <- quantile(test$bathrooms, 0.01)
Q3 <- quantile(test$bathrooms, 0.99)
Q <- Q3 - Q1
tr <- Q3 + 1.5 * Q

#Identificar los valores atípicos en el 0.1% más alto

outliers <- test$bathrooms > (Q3 + 1.5 * Q)

# Calcular la mediana de baños de acuerdo al número de habitaciones

median_banos <- tapply(test$bathrooms, test$bedrooms,median , na.rm = TRUE)


#Reemplazar los valores atípicos por la mediana según el número de habitaciones

test$bathrooms[outliers] <- median_banos[test$bedrooms[outliers]]

# Verificar que los valores se hayan reemplazado correctamente

summary(test$bathrooms)



# 2. Cuartos
# Definimos las palabras clave y el patrón de extracción.
sinonimos <- c("habitacion", "habitaciones", "alcoba", "alcobas", "cuarto", "cuartos")

numeros_escritos <- c("uno|primero", "dos|segundo","tres|tercero", "cuatro|cuarto", "cinco|quinto", 
                      "seis|sexto", "siete|septimo", "ocho|octavo", "nueve|noveno", "diez|decimo")

numeros <- as.character(1:10)

# Recorremos las filas del dataset
for (i in 1:nrow(test)) {
  
  for (sinonimo in sinonimos) {
    patron_busqueda <- paste0("(\\d+)\\s?", sinonimo)
    match <- str_match(test$description[i], patron_busqueda)
    if (!is.na(match[1])) {
      numero_match <- tolower(match[2])
      numero_match <- str_replace_all(numero_match, setNames(numeros, numeros_escritos))
      numero <- as.integer(numero_match)
      if (!is.na(numero)) {
        test$n_cuartos[i] <- numero
      }
    }
  }
}


test$rooms <- ifelse(is.na(test$rooms), test$n_cuartos, test$rooms)
summary(test$rooms)

Q1 <- quantile(test$rooms, 0.01, na.rm = TRUE)
Q3 <- quantile(test$rooms, 0.99, na.rm = TRUE)
Q <- Q3 - Q1

#Identificar los valores atípicos en el 0.1% más alto

outliers <- test$rooms > (Q3 + 1.5 * Q)

# Calcular la mediana de baños de acuerdo al número de habitaciones

median_rooms <- tapply(test$rooms, test$bedrooms,median , na.rm = TRUE)


#Reemplazar los valores atípicos por la mediana según el número de habitaciones
for (i in which(outliers | is.na(test$rooms))) {
  if (!is.na(test$bedrooms[i]) && test$bedrooms[i] %in% names(median_rooms)) {
    test$rooms[i] <- median_rooms[[as.character(test$bedrooms[i])]]
  }
}
# Verificar que los valores se hayan reemplazado correctamente

summary(test$rooms)


# 3. Metros cuadrados

#imputamos los datos para obtener el area de la vivienda
test$surface_total <- replace(test$surface_total, is.na(test$surface_total), test$surface_covered[is.na(test$surface_total)])
test <- test %>%
  mutate(surface_total = ifelse(is.na(surface_covered) | is.na(surface_total), surface_total, ifelse(surface_covered > surface_total, surface_covered, surface_total)))
#
for (i in 1:nrow(test)) {
  match <- str_match(test$description[i], "(\\d+)\\s?(m2|mts2|mts|m²|metros cuadrados|metros|mtrs|mtrs2|mt|mt2|m2?|metro cuadrados)")
  if (!is.na(match[1]) && !is.na(match[2])) {
    test$n_surface_total[i] <- as.integer(match[2])
  } else {
    test$n_surface_total[i] <- NA
  }
}
test$surface_total <- ifelse(is.na(test$surface_total), test$n_surface_total, test$surface_total)
summary(test$surface_total)

test <- test %>%
  mutate(surface_total = case_when(surface_total > 1000 ~ surface_total/1000 , TRUE ~ surface_total))


p1  <- quantile(test$surface_total, 0.01, na.rm = TRUE)
p99 <- quantile(test$surface_total, 0.99, na.rm = TRUE)
p10 <- quantile(test$surface_total, 0.10, na.rm = TRUE)
p90 <- quantile(test$surface_total, 0.90, na.rm = TRUE)

# Calcular las medias
mean_low <- mean(test$surface_total[test$surface_total <= p10], na.rm = TRUE)
mean_high <- mean(test$surface_total[test$surface_total >= p90], na.rm = TRUE)

# Reemplazar los valores en el 1% más bajo y más alto
test$surface_total[test$surface_total < p1] <- mean_low
test$surface_total[test$surface_total > p99] <- mean_high

# Calcular la media de surface_total por total_rooms
mean_surface <- tapply(test$surface_total, test$bedrooms, mean, na.rm = TRUE)

# Imputar la media correspondiente a NA en surface_total
test$surface_total[is.na(test$surface_total)] <- mean_surface[test$bedrooms[is.na(test$surface_total)]]

summary(test$surface_total)



#observamos la locación
leaflet() %>%
  addTiles() %>%
  addCircles(lng = test$lon,
             lat = test$lat)


#Creamos variables a partir de la descripcion
test <- test %>%
  mutate(parqueadero = as.numeric(grepl("parqueadero", test$description)))

test <- test %>%
  mutate(conjunto = as.numeric(grepl("conjunto", test$description)))

test <- test %>%
  mutate(piscina = as.numeric(grepl("piscina", test$description)))

test <- test %>%
  mutate(nuevo = as.numeric(grepl("estrenar|nuevo|nueva", test$description)))
summary(test$nuevo)

test <- test %>%
  mutate(duplex = as.numeric(grepl("duplex", test$description)))
summary(test$duplex)


# Creamos variable pisos

sinonimos <- c("piso", "pisos")

numeros_escritos <- c("uno|primero1primer", "dos|segundo","tres|tercero1tercer", "cuatro|cuarto", "cinco|quinto", 
                      "seis|sexto", "siete|septimo", "ocho|octavo", "nueve|noveno", "diez|decimo")

numeros <- as.character(1:10)

for (i in 1:nrow(test)) {
  
  for (sinonimo in sinonimos) {
    patron_busqueda <- paste0("(\\d+)\\s?", sinonimo)
    match <- str_match(test$description[i], patron_busqueda)
    if (!is.na(match[1])) {
      numero_match <- tolower(match[2])
      numero_match <- str_replace_all(numero_match, setNames(numeros, numeros_escritos))
      numero <- as.integer(numero_match)
      if (!is.na(numero)) {
        test$n_pisos[i] <- numero
      }
    }
  }
}

summary(test$n_pisos)

p95 <- quantile(test$n_pisos, 0.95, na.rm = TRUE)

mean_high <- mean(test$n_pisos[test$n_pisos >= p95], na.rm = TRUE)

test$n_pisos[test$n_pisos > p95] <- mean_high


moda_n_pisos <- test %>%
  group_by(property_type) %>%
  summarise(moda_n_pisos = as.numeric(names(which.max(table(n_pisos)))))

test <- test %>%
  left_join(moda_n_pisos, by = "property_type")

test$n_pisos[is.na(test$n_pisos)] <- test$moda_n_pisos[is.na(test$n_pisos)]

test <- test %>%
  mutate(n_pisos = round(n_pisos))

summary(test$n_pisos)


test <- test %>%
  mutate(color = case_when(property_type == "Apartamento" ~ "#2A9D8F",
                           property_type == "Casa" ~ "#F4A261"))

#Indicador de base
test <- test %>%
  mutate(base = c(0))



#________________________________________________________
#     TRAIN 

table(train$city)

#revisamos los NAs
sapply(train,function(x) sum(is.na(x)))

# Primero normalizaremos todo el texto
# Todo en minuscula
train <- train %>%
  mutate(description = str_to_lower(description))
# Eliminamos tildes
train <- train %>%
  mutate(description = iconv(description, from = "UTF-8", to = "ASCII//TRANSLIT"))
# Eliminamos caracteres especiales
train <- train %>%
  mutate(description = str_replace_all(description, "[^[:alnum:]]", " "))
# Eliminamos espacios extras
train <- train %>%
  mutate(description = str_trim(gsub("\\s+", " ", description)))

#______________________________________________________________
#imputamos los datos faltantes según la descripción y el título 

# 1. Baños

for (i in 1:nrow(train)) {
  match <- str_match(train$description[i], "(\\d+)(\\s?bano|banos|bao|baos)")
  if (!is.na(match[1])) {
    train$n_banos[i] <- as.integer(match[2])
  } else {
    train$n_banos[i] <- 1
  }
}

train$bathrooms <- ifelse(is.na(train$bathrooms), train$n_banos, train$bathrooms)
summary(train$bathrooms)

#corregimos el 0.1% más alto dado que son valores atípicos

Q1 <- quantile(train$bathrooms, 0.01)
Q3 <- quantile(train$bathrooms, 0.99)
Q <- Q3 - Q1
tr <- Q3 + 1.5 * Q

#Identificar los valores atípicos en el 0.1% más alto

outliers <- train$bathrooms > (Q3 + 1.5 * Q)

# Calcular la mediana de baños de acuerdo al número de habitaciones

median_banos <- tapply(train$bathrooms, train$bedrooms,median , na.rm = TRUE)


#Reemplazar los valores atípicos por la mediana según el número de habitaciones

train$bathrooms[outliers] <- median_banos[train$bedrooms[outliers]]

# Verificar que los valores se hayan reemplazado correctamente

summary(train$bathrooms)



# 2. Cuartos
# Definimos las palabras clave y el patrón de extracción.
sinonimos <- c("habitacion", "habitaciones", "alcoba", "alcobas", "cuarto", "cuartos")

numeros_escritos <- c("uno|primero", "dos|segundo","tres|tercero", "cuatro|cuarto", "cinco|quinto", 
                      "seis|sexto", "siete|septimo", "ocho|octavo", "nueve|noveno", "diez|decimo")

numeros <- as.character(1:10)

# Recorremos las filas del dataset
# Inicializa un vector n_cuartos con NA
train$n_cuartos <- rep(NA, nrow(train))

# Recorremos las filas del dataset
for (i in 1:nrow(train)) {
  for (sinonimo in sinonimos) {
    patron_busqueda <- paste0("(\\d+)\\s?", sinonimo)
    match <- str_match(train$description[i], patron_busqueda)
    if (!is.na(match[1])) {
      numero_match <- tolower(match[2])
      numero_match <- str_replace_all(numero_match, setNames(numeros, numeros_escritos))
      numero <- as.integer(numero_match)
      if (!is.na(numero)) {
        train$n_cuartos[i] <- numero
      }
    }
  }
}


train$rooms <- ifelse(is.na(train$rooms), train$n_cuartos, train$rooms)
summary(train$rooms)

Q1 <- quantile(train$rooms, 0.01, na.rm = TRUE)
Q3 <- quantile(train$rooms, 0.99, na.rm = TRUE)
Q <- Q3 - Q1

#Identificar los valores atípicos en el 0.1% más alto

outliers <- train$rooms > (Q3 + 1.5 * Q)

# Calcular la mediana de baños de acuerdo al número de habitaciones

median_rooms <- tapply(train$rooms, train$bedrooms,median , na.rm = TRUE)


#Reemplazar los valores atípicos por la mediana según el número de habitaciones
for (i in which(outliers | is.na(train$rooms))) {
  if (!is.na(train$bedrooms[i]) && train$bedrooms[i] %in% names(median_rooms)) {
    train$rooms[i] <- median_rooms[[as.character(train$bedrooms[i])]]
  }
}
# Verificar que los valores se hayan reemplazado correctamente

summary(train$rooms)


# 3. Metros cuadrados

#imputamos los datos para obtener el area de la vivienda
train$surface_total <- replace(train$surface_total, is.na(train$surface_total), train$surface_covered[is.na(train$surface_total)])
train <- train %>%
  mutate(surface_total = ifelse(is.na(surface_covered) | is.na(surface_total), surface_total, ifelse(surface_covered > surface_total, surface_covered, surface_total)))
#
for (i in 1:nrow(train)) {
  match <- str_match(train$description[i], "(\\d+)\\s?(m2|mts2|mts|m²|metros cuadrados|metros|mtrs|mtrs2|mt|mt2|m2?|metro cuadrados)")
  if (!is.na(match[1]) && !is.na(match[2])) {
    train$n_surface_total[i] <- as.integer(match[2])
  } else {
    train$n_surface_total[i] <- NA
  }
}
train$surface_total <- ifelse(is.na(train$surface_total), train$n_surface_total, train$surface_total)
summary(train$surface_total)

train <- train %>%
  mutate(surface_total = case_when(surface_total > 1000 ~ surface_total/1000 , TRUE ~ surface_total))


p1  <- quantile(train$surface_total, 0.01, na.rm = TRUE)
p99 <- quantile(train$surface_total, 0.99, na.rm = TRUE)
p10 <- quantile(train$surface_total, 0.10, na.rm = TRUE)
p90 <- quantile(train$surface_total, 0.90, na.rm = TRUE)

# Calcular las medias
mean_low <- mean(train$surface_total[train$surface_total <= p10], na.rm = TRUE)
mean_high <- mean(train$surface_total[train$surface_total >= p90], na.rm = TRUE)

# Reemplazar los valores en el 1% más bajo y más alto
train$surface_total[train$surface_total < p1] <- mean_low
train$surface_total[train$surface_total > p99] <- mean_high

# Calcular la media de surface_total por total_rooms
mean_surface <- tapply(train$surface_total, train$bedrooms, mean, na.rm = TRUE)

# Imputar la media correspondiente a NA en surface_total
train$surface_total[is.na(train$surface_total)] <- mean_surface[train$bedrooms[is.na(train$surface_total)]]

summary(train$surface_total)


#observamos la locación
leaflet() %>%
  addTiles() %>%
  addCircles(lng = train$lon,
             lat = train$lat)

##############
#Nos quedamos con las localidades que queremos

#limites_b <- getbb("Localidad Teusaquillo, Bogotá")
#limites_c <- getbb("Localidad Barrios Unidos, Bogotá")
#limites_d <- getbb("Localidad Usaquén, Bogotá")
#limites_e <- getbb("Localidad Santa Fé, Bogotá") 

# Filtrar los datos para incluir las coordenadas dentro de los límites de ambas localidades
#train <- train %>%
 # filter(
  #  (between(lon, limites_b[1, "min"], limites_b[1, "max"]) &
   #    between(lat, limites_b[2, "min"], limites_b[2, "max"])) |
    #  (between(lon, limites_c[1, "min"], limites_c[1, "max"]) &
     #    between(lat, limites_c[2, "min"], limites_c[2, "max"]))|
      #    (between(lon, limites_d[1, "min"], limites_d[1, "max"]) &
       #    between(lat, limites_d[2, "min"], limites_d[2, "max"])) |
        #    (between(lon, limites_e[1, "min"], limites_e[1, "max"]) &
         #      between(lat, limites_e[2, "min"], limites_e[2, "max"]))
#  )



train <- train %>%
  mutate(color = case_when(property_type == "Apartamento" ~ "#2A9D8F",
                           property_type == "Casa" ~ "#F4A261"))


# Encontramos el queremos que sea el centro del mapa 
latitud_central <- mean(train$lat)
longitud_central <- mean(train$lon)

# Creamos el plot
leaflet() %>%
  addTiles() %>%
  setView(lng = longitud_central, lat = latitud_central, zoom = 12) %>%
  addCircles(lng = train$lon, 
             lat = train$lat, 
             col = train$color,
             fillOpacity = 1,
             opacity = 1)


#Creamos variables a partir de la descripcion
train <- train %>%
  mutate(parqueadero = as.numeric(grepl("parqueadero", train$description)))

train <- train %>%
  mutate(conjunto = as.numeric(grepl("conjunto", train$description)))

train <- train %>%
  mutate(piscina = as.numeric(grepl("piscina", train$description)))

train <- train %>%
  mutate(nuevo = as.numeric(grepl("estrenar|nuevo|nueva", train$description)))
summary(train$nuevo)

train <- train %>%
  mutate(duplex = as.numeric(grepl("duplex", train$description)))
summary(train$duplex)

# Creamos variable pisos

sinonimos <- c("piso", "pisos")

numeros_escritos <- c("uno|primero1primer", "dos|segundo","tres|tercero1tercer", "cuatro|cuarto", "cinco|quinto", 
                      "seis|sexto", "siete|septimo", "ocho|octavo", "nueve|noveno", "diez|decimo")

numeros <- as.character(1:10)

for (i in 1:nrow(train)) {
  
  for (sinonimo in sinonimos) {
    patron_busqueda <- paste0("(\\d+)\\s?", sinonimo)
    match <- str_match(train$description[i], patron_busqueda)
    if (!is.na(match[1])) {
      numero_match <- tolower(match[2])
      numero_match <- str_replace_all(numero_match, setNames(numeros, numeros_escritos))
      numero <- as.integer(numero_match)
      if (!is.na(numero)) {
        train$n_pisos[i] <- numero
      }
    }
  }
}

summary(train$n_pisos)

p95 <- quantile(train$n_pisos, 0.95, na.rm = TRUE)

mean_high <- mean(train$n_pisos[train$n_pisos >= p95], na.rm = TRUE)

train$n_pisos[train$n_pisos > p95] <- mean_high


moda_n_pisos <- train %>%
  group_by(property_type) %>%
  summarise(moda_n_pisos = as.numeric(names(which.max(table(n_pisos)))))

train <- train %>%
  left_join(moda_n_pisos, by = "property_type")

train$n_pisos[is.na(train$n_pisos)] <- train$moda_n_pisos[is.na(train$n_pisos)]

train <- train %>%
  mutate(n_pisos = round(n_pisos))

summary(train$n_pisos)

train <- train %>%
  mutate(base = c(1))

#Union ambas bases
db <- rbind(test, train)


#Revision variables property type
db <- db %>%
  mutate(property_type_2 = ifelse(grepl("casa", description), "casa", property_type))

db <- db %>%
  mutate(property_type_2 = ifelse(grepl("apto|apartamento", description), "apartamento", property_type_2)) %>%
  select(-property_type)

db <- db %>%
  mutate(property_type_2 = ifelse(grepl("casa", title), "casa", property_type_2))

db <- db %>%
  mutate(property_type_2 = ifelse(grepl("apto|apartamento", title), "apartamento", property_type_2)) %>%
  select(-property_type)


#___________________________________________________________
#
#                VARIABLES ESPACIALES
#
#___________________________________________________________


#   Variables espaciales

train_sf <- st_as_sf(train, coords = c("lon", "lat"), crs=4326)
test_sf <- st_as_sf(test, coords = c("lon", "lat"), crs=4326)

db_sf <- st_as_sf(db, coords = c("lon", "lat"), crs=4326)

#.................................
##Cercania a Universidades
universidades <- opq(bbox = getbb('Bogotá Colombia')) %>%
  add_osm_feature(key='amenity', value= 'university')

universidades_sf <- osmdata_sf(universidades)

universidades_geometria <- universidades_sf$osm_polygons %>%
  select(osm_id, name)

centroides <- gCentroid(as(universidades_geometria$geometry, "Spatial"), byid = T)

leaflet() %>%
  addTiles() %>%
  setView(lng = longitud_central, lat = latitud_central, zoom = 12) %>%
  addPolygons( data= universidades_geometria, col = "red",weight = 10,
               opacity = 0.8, popup = universidades_geometria$name) %>%
  addCircles(lng = train$lon, 
             lat = train$lat, 
             col = train$color,
             fillOpacity = 1,
             opacity = 1)

centroides_sf <- st_as_sf(centroides, coords = c("x", "y"))

dist_matrix_universidades <- st_distance(x=db_sf,y=centroides_sf)

dist_min_universidades <- apply(dist_matrix_universidades, 1, min)

db <- db %>% mutate(distancia_universidades = dist_min_universidades)


posicion <- apply(dist_matrix_universidades, 1, function(x) which(min(x) == x))

areas_universidades <- st_area(universidades_geometria)

db <- db %>%
  mutate(area_universidades = as.numeric(areas_universidades[posicion]))



#.................................
##Cercania a estaciones de transmilenio (variable cuadratica)
parada_de_bus <- opq(bbox = getbb("Bogotá Colombia")) %>%
  add_osm_feature(key ='amenity' , value = 'bus_station') 

parada_de_bus_sf <- osmdata_sf(parada_de_bus)

parada_de_bus_geometria <- parada_de_bus_sf$osm_polygons %>% 
  select(osm_id, name)

centroides <-st_centroid(parada_de_bus_geometria$geometry)

leaflet() %>%
  addTiles() %>%
  setView(lng = longitud_central, lat = latitud_central, zoom = 12) %>%
  addPolygons(data = parada_de_bus_geometria, col = "#9BCD9B",weight = 10,
              opacity = 0.8, popup = parada_de_bus_geometria$name) %>%
  addCircles(data=centroides,col = '#698B69' , opacity = 0.5, radius = 1)


centroides_sf <- do.call(rbind, st_geometry(centroides)) %>% 
  as_tibble() %>% setNames(c("lon","lat")) 
centroides_sf <- st_as_sf(centroides_sf, coords = c("lon", "lat"), crs=4326)
  
nearest <- st_nearest_feature(db_sf,centroides_sf)

db<- db %>% mutate(distancia_bus=st_distance(x = db_sf, y = centroides_sf[nearest,], by_element=TRUE))



#.................................
##Cercania a teatros
teatros <- opq(bbox = getbb("Bogotá Colombia")) %>%
  add_osm_feature(key ='amenity' , value = 'theatre') 

teatros_sf <- osmdata_sf(teatros)

teatros_geometria <- teatros_sf$osm_polygons %>% 
  select(osm_id, name)

centroides <-st_centroid(teatros_geometria$geometry)

leaflet() %>%
  addTiles() %>%
  setView(lng = longitud_central, lat = latitud_central, zoom = 12) %>%
  addPolygons(data = teatros_geometria, col = "#9BCD9B",weight = 10,
              opacity = 0.8, popup = teatros_geometria$name) %>%
  addCircles(data=centroides,col = '#698B69' , opacity = 0.5, radius = 1)


centroides_sf <- do.call(rbind, st_geometry(centroides)) %>% 
  as_tibble() %>% setNames(c("lon","lat")) 
centroides_sf <- st_as_sf(centroides_sf, coords = c("lon", "lat"), crs=4326)

nearest <- st_nearest_feature(db_sf,centroides_sf)

db<- db %>% mutate(distancia_teatros=st_distance(x = db_sf, y = centroides_sf[nearest,], by_element=TRUE))


#.................................
##Cercania a policia
policia <- opq(bbox = getbb("Bogotá Colombia")) %>%
  add_osm_feature(key ='amenity' , value = 'police') 

policia_sf <- osmdata_sf(policia)

policia_geometria <- policia_sf$osm_polygons %>% 
  select(osm_id, name)

centroides <-st_centroid(policia_geometria$geometry)

leaflet() %>%
  addTiles() %>%
  setView(lng = longitud_central, lat = latitud_central, zoom = 12) %>%
  addPolygons(data = policia_geometria, col = "#9BCD9B",weight = 10,
              opacity = 0.8, popup = policia_geometria$name) %>%
  addCircles(data=centroides,col = '#698B69' , opacity = 0.5, radius = 1)


centroides_sf <- do.call(rbind, st_geometry(centroides)) %>% 
  as_tibble() %>% setNames(c("lon","lat")) 
centroides_sf <- st_as_sf(centroides_sf, coords = c("lon", "lat"), crs=4326)

nearest <- st_nearest_feature(db_sf,centroides_sf)

db<- db %>% mutate(distancia_policia=st_distance(x = db_sf, y = centroides_sf[nearest,], by_element=TRUE))


#.................................
##Cercania a concesionarios
concesionarios <- opq(bbox = getbb("Bogotá Colombia")) %>%
  add_osm_feature(key ='shop' , value = 'car') 

concesionarios_sf <- osmdata_sf(concesionarios)

concesionarios_geometria <- concesionarios_sf$osm_polygons %>% 
  select(osm_id, name)

centroides <-st_centroid(concesionarios_geometria$geometry)

leaflet() %>%
  addTiles() %>%
  setView(lng = longitud_central, lat = latitud_central, zoom = 12) %>%
  addPolygons(data = concesionarios_geometria, col = "#9BCD9B",weight = 10,
              opacity = 0.8, popup = concesionarios_geometria$name) %>%
  addCircles(data=centroides,col = '#698B69' , opacity = 0.5, radius = 1)


centroides_sf <- do.call(rbind, st_geometry(centroides)) %>% 
  as_tibble() %>% setNames(c("lon","lat")) 
centroides_sf <- st_as_sf(centroides_sf, coords = c("lon", "lat"), crs=4326)

nearest <- st_nearest_feature(db_sf,centroides_sf)

db<- db %>% mutate(distancia_concesionarios=st_distance(x = db_sf, y = centroides_sf[nearest,], by_element=TRUE))


#.................................
##Cercania a bancos
banco <- opq(bbox = getbb("Bogotá Colombia")) %>%
  add_osm_feature(key ='amenity' , value = 'bank') 

banco_sf <- osmdata_sf(banco)

banco_geometria <- banco_sf$osm_polygons %>% 
  select(osm_id, name)

centroides <-st_centroid(banco_geometria$geometry)

leaflet() %>%
  addTiles() %>%
  setView(lng = longitud_central, lat = latitud_central, zoom = 12) %>%
  addPolygons(data = banco_geometria, col = "#9BCD9B",weight = 10,
              opacity = 0.8, popup = banco_geometria$name) %>%
  addCircles(data=centroides,col = '#698B69' , opacity = 0.5, radius = 1)


centroides_sf <- do.call(rbind, st_geometry(centroides)) %>% 
  as_tibble() %>% setNames(c("lon","lat")) 
centroides_sf <- st_as_sf(centroides_sf, coords = c("lon", "lat"), crs=4326)

nearest <- st_nearest_feature(db_sf,centroides_sf)

db<- db %>% mutate(distancia_banco=st_distance(x = db_sf, y = centroides_sf[nearest,], by_element=TRUE))


#.................................
##Cercania a bombas de gasolina
gasolina <- opq(bbox = getbb("Bogotá Colombia")) %>%
  add_osm_feature(key ='amenity' , value = 'fuel') 

gasolina_sf <- osmdata_sf(gasolina)

gasolina_geometria <- gasolina_sf$osm_polygons %>% 
  select(osm_id, name)

centroides <-st_centroid(gasolina_geometria$geometry)

leaflet() %>%
  addTiles() %>%
  setView(lng = longitud_central, lat = latitud_central, zoom = 12) %>%
  addPolygons(data = gasolina_geometria, col = "#9BCD9B",weight = 10,
              opacity = 0.8, popup = gasolina_geometria$name) %>%
  addCircles(data=centroides,col = '#698B69' , opacity = 0.5, radius = 1)


centroides_sf <- do.call(rbind, st_geometry(centroides)) %>% 
  as_tibble() %>% setNames(c("lon","lat")) 
centroides_sf <- st_as_sf(centroides_sf, coords = c("lon", "lat"), crs=4326)

nearest <- st_nearest_feature(db_sf,centroides_sf)

db<- db %>% mutate(distancia_gasolina=st_distance(x = db_sf, y = centroides_sf[nearest,], by_element=TRUE))


#.................................
##Cercania a centros comerciales
centro_comercial <- opq(bbox = getbb('Bogotá Colombia')) %>%
  add_osm_feature(key='shop', value= 'mall')

centro_comercial_sf <- osmdata_sf(centro_comercial)

centro_comercial_geometria <- centro_comercial_sf$osm_polygons %>%
  select(osm_id, name)

centroides <- gCentroid(as(centro_comercial_geometria$geometry, "Spatial"), byid = T)

leaflet() %>%
  addTiles() %>%
  setView(lng = longitud_central, lat = latitud_central, zoom = 12) %>%
  addPolygons( data= centro_comercial_geometria, col = "red",weight = 10,
               opacity = 0.8, popup = centro_comercial_geometria$name) %>%
  addCircles(lng = train$lon, 
             lat = train$lat, 
             col = train$color,
             fillOpacity = 1,
             opacity = 1)

centroides_sf <- st_as_sf(centroides, coords = c("x", "y"))

dist_matrix_comercial <- st_distance(x=db_sf,y=centroides_sf)

dist_min_comercial <- apply(dist_matrix_comercial, 1, min)

db <- db %>% mutate(distancia_comercial = dist_min_comercial)


posicion <- apply(dist_matrix_comercial, 1, function(x) which(min(x) == x))

areas_comercial <- st_area(centro_comercial_geometria)

db <- db %>%
  mutate(area_comercial = as.numeric(areas_comercial[posicion]))



#.................................
##Cercania a talleres
talleres <- opq(bbox = getbb("Bogotá Colombia")) %>%
  add_osm_feature(key ='shop' , value = 'car_repair') 

talleres_sf <- osmdata_sf(talleres)

talleres_geometria <- talleres_sf$osm_polygons %>% 
  select(osm_id, name)

centroides <-st_centroid(talleres_geometria$geometry)

leaflet() %>%
  addTiles() %>%
  setView(lng = longitud_central, lat = latitud_central, zoom = 12) %>%
  addPolygons(data = talleres_geometria, col = "#9BCD9B",weight = 10,
              opacity = 0.8, popup = talleres_geometria$name) %>%
  addCircles(data=centroides,col = '#698B69' , opacity = 0.5, radius = 1)


centroides_sf <- do.call(rbind, st_geometry(centroides)) %>% 
  as_tibble() %>% setNames(c("lon","lat")) 
centroides_sf <- st_as_sf(centroides_sf, coords = c("lon", "lat"), crs=4326)

nearest <- st_nearest_feature(db_sf,centroides_sf)

db<- db %>% mutate(distancia_talleres=st_distance(x = db_sf, y = centroides_sf[nearest,], by_element=TRUE))


#.................................
##Parques
parques <- opq(bbox = getbb('Bogotá Colombia')) %>%
  add_osm_feature(key='leisure', value= 'park')

parques_sf <- osmdata_sf(parques)

parques_geometria <- parques_sf$osm_polygons %>%
  select(osm_id, name)

centroides <- gCentroid(as(parques_geometria$geometry, "Spatial"), byid = T)

leaflet() %>%
  addTiles() %>%
  setView(lng = longitud_central, lat = latitud_central, zoom = 12) %>%
  addPolygons( data= parques_geometria, col = "red",weight = 10,
               opacity = 0.8, popup = parques_geometria$name) %>%
  addCircles(lng = train$lon, 
             lat = train$lat, 
             col = train$color,
             fillOpacity = 1,
             opacity = 1)

centroides_sf <- st_as_sf(centroides, coords = c("x", "y"))

dist_matrix_parques <- st_distance(x=db_sf,y=centroides_sf)

dist_min_parques <- apply(dist_matrix_parques, 1, min)

db <- db %>% mutate(distancia_parque = dist_min_parques)


posicion <- apply(dist_matrix_parques, 1, function(x) which(min(x) == x))


areas_parques <- st_area(parques_geometria)

db <- db %>%
  mutate(area_parques = as.numeric(areas_parques[posicion]))


#.................................
##Estratos
Data_censo <- read_rds("output/mgn_censo_2018.rds") 
Data_censo$MANZ_CCNCT <- NULL
Data_censo$med_H_NRO_CUARTOS <- NULL
Data_censo$sum_HA_TOT_PER <- NULL
Data_censo$med_V_TOT_HOG <- NULL

Data_censo <- subset(Data_censo, subset= !is.na(Data_censo$med_VA1_ESTRATO))
summary(Data_censo)

Data_censo_sf <-st_transform(Data_censo,4686)

db_sf_4686 <-st_transform(db_sf,4686)


cestratos <- gCentroid(as(Data_censo_sf$geometry, "Spatial"), byid = T)

cest_sf <- st_as_sf(cestratos, coords = c("x", "y"))

cest_sf <-st_transform(cest_sf,4686)

estratos_nearest_train<-st_nearest_feature(db_sf_4686,cest_sf)

db<-db %>% 
  mutate(estrato = Data_censo_sf$med_VA1_ESTRATO[estratos_nearest_train])


leaflet() %>% 
  addTiles() %>% 
  setView(lng = longitud_central, lat = latitud_central, zoom = 12) %>%
  addPolygons( data= Data_censo,weight = 10,
               opacity = 0.8, popup = Data_censo$geometry) %>%
  addCircles(lng = train$lon, 
             lat = train$lat, 
             col = train$color,
             fillOpacity = 1,
             opacity = 1)


#for (i in 1:nrow(test)) {
#  match <- str_match(db$description[i], "(\\d+)(\\s?estrato|estrat)")
#  if (!is.na(match[1])) {
#    db$estrato_text[i] <- as.integer(match[2])
#  } else {
#    db$estrato_text[i] <- NA
#  }
#}

#db$estrato_text[db$estrato_text > 6] <- NA

#summary(db$estrato_text)

#db$estrato <- ifelse(is.na(db$estrato), db$estrato_text, db$estrato)
#summary(db$estrato)

#___________________________________________________________
db <- subset(db, select = -n_banos)
db <- subset(db, select = -n_cuartos)
db <- subset(db, select = -n_surface_total)
#db <- subset(db, select = -estrato_text)
db <- subset(db, select = -color)
db <- subset(db, select = -moda_n_pisos)
db <- subset(db, select = -n_pisos)

                            
#___________________________________________________________
#Dividir otra vez las bases con los datos espaciales
test_2 <- db[db$base == 0, ]
train_2 <- db[db$base == 1, ]

table(db$estrato)

write_xlsx(test_2, "test_2.xlsx")
write_xlsx(db, "db.xlsx")
write_xlsx(train_2, "train_2.xlsx")




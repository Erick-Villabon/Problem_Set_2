#_____________________________________________________________________________#
#                                                                             #
#                      Problem Set 2: Making Money with ML?                       #
#                                                                             #
#_____________________________________________________________________________#

#   Autores: - Erick Villabon                                                  
#            - Juan Diego Duarte
#            - 
#            - 
#
#  Fecha: 22/09/2023 


# - Limpiar espacio de trabajo

rm(list = ls())

# - Librerias y paquetes 

library(pacman)
p_load(rvest, tidyverse, ggplot2, robotstxt, psych, stargazer, boot, plotly, openxlsx, glmnet,
       rio, leaflet, rgeos, tmaptools, sf, osmdata, tidymodels,
       units, # unidades
       randomForest, # Modelos de bosque aleatorio
       rattle, # Interfaz gráfica para el modelado de datos
       spatialsample)

# 1. Actualizatr espacio de trabajo 
setwd("/Users/juandiego/Desktop/GitHub/Problem_Set_2/stores")
#setwd("C:/Users/Erick/Desktop/Problem_Set_2/stores")
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




#__________________________________________________
#imputamos los datos faltantes según la descripción 

for (i in 1:nrow(test)) {
  match <- str_match(test$description[i], "(\\d+)\\s?banos")
  if (!is.na(match[1])) {
    test$n_banos[i] <- as.integer(match[2])
  } else {
    test$n_banos[i] <- 1
  }
}

test$bathrooms <- ifelse(is.na(test$bathrooms), test$n_banos, test$bathrooms)
summary(test$bathrooms)


Q1 <- quantile(test$bathrooms, 0.25)
Q3 <- quantile(test$bathrooms, 0.75)
Q <- Q3 - Q1

Tr <- 1.5 * Q
MNO <- median(test$bathrooms[test$bathrooms >= Q1 - Q & test$bathrooms <= Q3 + Q])

test$bathrooms[test$bathrooms < Q1 - Q | test$bathrooms > Q3 + Q] <- MNO

summary(test$bathrooms)

test$bathrooms %>%
  table(useNA = "ifany") %>%
  prop.table() %>%
  round(4)*100


#

for (i in 1:nrow(test)) {
  match <- str_match(test$description[i], "(\\d+)\\s?(alcoba|alcobas|habitacion|habitaciones|cuarto|cuartos)")
  if (!is.na(match[1])) {
    test$n_cuartos[i] <- as.integer(match[2])
  } else {
    test$n_cuartos[i] <- 1
  }
}

test$rooms <- ifelse(is.na(test$rooms), test$n_cuartos, test$rooms)
summary(test$rooms)

Q1 <- quantile(test$rooms, 0.25)
Q3 <- quantile(test$rooms, 0.75)
Q <- Q3 - Q1

Tr <- 1.5 * Q
MNO <- median(test$rooms[test$rooms >= Q1 - Q & test$rooms <= Q3 + Q])

test$rooms[test$rooms < Q1 - Q | test$rooms > Q3 + Q] <- MNO

summary(test$rooms)

test$rooms %>%
  table(useNA = "ifany") %>%
  prop.table() %>%
  round(4)*100

#


#creación de variable dummy

tipo_vivienda <- c("Apartamento", "Casa") 
table(test$property_type)

test$property_type_dicotomica <- ifelse(test$property_type == "Apartamento", 1, 0)

#observamos la locación
leaflet() %>%
  addTiles() %>%
  addCircles(lng = test$lon,
             lat = test$lat)


#Creamos variables a partir de la descripcion
test <- test %>%
  mutate(parqueadero = as.numeric(grepl("parqueadero", test$description)))
test <- test %>%
  mutate(color = case_when(property_type == "Apartamento" ~ "#2A9D8F",
                           property_type == "Casa" ~ "#F4A261"))









#imputamos los datos para obtener el area de la vivienda
test$surface_total <- replace(test$surface_total, is.na(test$surface_total), test$surface_covered[is.na(test$surface_total)])
test <- test %>%
  mutate(surface_total = ifelse(is.na(surface_covered) | is.na(surface_total), surface_total, ifelse(surface_covered > surface_total, surface_covered, surface_total)))
#
for (i in 1:nrow(test)) {
  match <- str_match(test$description[i], "(\\d+)\\s?(mts|m2|metros)")
  if (!is.na(match[1]) && !is.na(match[2])) {
    test$n_surface_total[i] <- as.integer(match[2])
  } else {
    test$n_surface_total[i] <- NA
  }
}
test$surface_total <- ifelse(is.na(test$surface_total), test$n_surface_total, test$surface_total)
summary(test$surface_total)

test <- test %>%
  mutate(total_rooms = rooms + bathrooms)


p1  = quantile(test$surface_total, 0.01, na.rm = TRUE)
p99 = quantile(test$surface_total, 0.99, na.rm = TRUE)
p10 = quantile(test$surface_total, 0.10, na.rm = TRUE)
p90 = quantile(test$surface_total, 0.90, na.rm = TRUE)

# Calcular las medias
mean_low = mean(test$surface_total[test$surface_total <= p10])
mean_high = mean(test$surface_total[test$surface_total >= p90])

# Reemplazar los valores en el 1% más bajo y más alto
test$surface_total[test$surface_total < p1] <- mean_low
test$surface_total[test$surface_total > p99] <- mean_high


# Calcular la media de surface_total por total_rooms
mean_surface <- tapply(test$surface_total, test$total_rooms, mean, na.rm = TRUE)

# Imputar la media correspondiente a NA en surface_total
test$surface_total[is.na(test$surface_total)] <- mean_surface[test$total_rooms[is.na(test$surface_total)]]

summary(test$surface_total)

# Calcular la media de surface_total
mean_s <- mean(test$surface_total, na.rm = TRUE)

# Imputar la media a los valores NA
test$surface_total[is.na(test$surface_total)] <- mean_s

summary(test$surface_total)






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

#__________________________________________________
#imputamos los datos faltantes según la descripción 

for (i in 1:nrow(train)) {
  match <- str_match(train$description[i], "(\\d+)\\s?banos")
  if (!is.na(match[1])) {
    train$n_banos[i] <- as.integer(match[2])
  } else {
    train$n_banos[i] <- 1
  }
}

train$bathrooms <- ifelse(is.na(train$bathrooms), train$n_banos, train$bathrooms)
summary(train$bathrooms)


Q1 <- quantile(train$bathrooms, 0.25)
Q3 <- quantile(train$bathrooms, 0.75)
Q <- Q3 - Q1

Tr <- 1.5 * Q
MNO <- median(train$bathrooms[train$bathrooms >= Q1 - Q & train$bathrooms <= Q3 + Q])

train$bathrooms[train$bathrooms < Q1 - Q | train$bathrooms > Q3 + Q] <- MNO

summary(train$bathrooms)

train$bathrooms %>%
  table(useNA = "ifany") %>%
  prop.table() %>%
  round(4)*100


#

for (i in 1:nrow(train)) {
  match <- str_match(train$description[i], "(\\d+)\\s?(alcoba|alcobas|habitacion|habitaciones|cuarto|cuartos)")
  if (!is.na(match[1])) {
    train$n_cuartos[i] <- as.integer(match[2])
  } else {
    train$n_cuartos[i] <- 1
  }
}

train$rooms <- ifelse(is.na(train$rooms), train$n_cuartos, train$rooms)
summary(train$rooms)

Q1 <- quantile(train$rooms, 0.25)
Q3 <- quantile(train$rooms, 0.75)
Q <- Q3 - Q1

Tr <- 1.5 * Q
MNO <- median(train$rooms[train$rooms >= Q1 - Q & train$rooms <= Q3 + Q])

train$rooms[train$rooms < Q1 - Q | train$rooms > Q3 + Q] <- MNO

summary(train$rooms)

train$rooms %>%
  table(useNA = "ifany") %>%
  prop.table() %>%
  round(4)*100


#

#imputamos los datos para obtener el area de la vivienda
train$surface_total <- replace(train$surface_total, is.na(train$surface_total), train$surface_covered[is.na(train$surface_total)])
train <- train %>%
  mutate(surface_total = ifelse(is.na(surface_covered) | is.na(surface_total), surface_total, ifelse(surface_covered > surface_total, surface_covered, surface_total)))
#
for (i in 1:nrow(train)) {
  match <- str_match(train$description[i], "(\\d+)\\s?(mts|m2|metros)")
  if (!is.na(match[1]) && !is.na(match[2])) {
    train$n_surface_total[i] <- as.integer(match[2])
  } else {
    train$n_surface_total[i] <- NA
  }
}
train$surface_total <- ifelse(is.na(train$surface_total), train$n_surface_total, train$surface_total)
summary(train$surface_total)

train <- train %>%
  mutate(total_rooms = rooms + bathrooms)


p1  = quantile(train$surface_total, 0.01, na.rm = TRUE)
p99 = quantile(train$surface_total, 0.99, na.rm = TRUE)
p10 = quantile(train$surface_total, 0.10, na.rm = TRUE)
p90 = quantile(train$surface_total, 0.90, na.rm = TRUE)

# Calcular las medias
mean_low = mean(train$surface_total[train$surface_total <= p10])
mean_high = mean(train$surface_total[train$surface_total >= p90])

# Reemplazar los valores en el 1% más bajo y más alto
train$surface_total[train$surface_total < p1] <- mean_low
train$surface_total[train$surface_total > p99] <- mean_high


# Calcular la media de surface_total por total_rooms
mean_surface <- tapply(train$surface_total, train$total_rooms, mean, na.rm = TRUE)

# Imputar la media correspondiente a NA en surface_total
train$surface_total[is.na(train$surface_total)] <- mean_surface[train$total_rooms[is.na(train$surface_total)]]

summary(train$surface_total)

# Calcular la media de surface_total
mean_s <- mean(train$surface_total, na.rm = TRUE)

# Imputar la media a los valores NA
train$surface_total[is.na(train$surface_total)] <- mean_s

summary(train$surface_total)









#creación de variable dummy

tipo_vivienda <- c("Apartamento", "Casa") 
table(train$property_type)

train$property_type_dicotomica <- ifelse(train$property_type == "Apartamento", 1, 0)

#observamos la locación
leaflet() %>%
  addTiles() %>%
  addCircles(lng = train$lon,
             lat = train$lat)

##############
#Nos quedamos con las localidades que queremos

limites_b <- getbb("Localidad Teusaquillo, Bogotá")
limites_c <- getbb("Localidad Barrios Unidos, Bogotá")
limites_d <- getbb("Localidad Usaquén, Bogotá")
limites_e <- getbb("Localidad Santa Fé, Bogotá") 

# Filtrar los datos para incluir las coordenadas dentro de los límites de ambas localidades
train <- train %>%
  filter(
    (between(lon, limites_b[1, "min"], limites_b[1, "max"]) &
       between(lat, limites_b[2, "min"], limites_b[2, "max"])) |
      (between(lon, limites_c[1, "min"], limites_c[1, "max"]) &
         between(lat, limites_c[2, "min"], limites_c[2, "max"]))|
          (between(lon, limites_d[1, "min"], limites_d[1, "max"]) &
           between(lat, limites_d[2, "min"], limites_d[2, "max"])) |
            (between(lon, limites_e[1, "min"], limites_e[1, "max"]) &
               between(lat, limites_e[2, "min"], limites_e[2, "max"]))
  )



train <- train %>%
  mutate(color = case_when(property_type == "Apartamento" ~ "#2A9D8F",
                           property_type == "Casa" ~ "#F4A261"))


# Encontram,os el queremos que sea el centro del mapa 
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
  mutate(base = c(1))
#___________________________________________________________
#___________________________________________________________
#   Variables espaciales
db <- rbind(test, train)

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



#___________________________________________________________
#Dividir otra vez las bases con los datos espaciales
test_2 <- db[db$base == 0, ]
train_2 <- db[db$base == 1, ]


#___________________________________________________________
#   prediccion
nrow(test_2)/nrow(train_2)

#Toca validar

##Modelo
lambda = .7

# Ridge
ridge_spec <- linear_reg(penalty = lambda, mixture = 1) %>%
  set_engine("glmnet")

# Lasso
lasso_spec <- linear_reg(penalty = lambda, mixture = 0) %>%
  set_engine("glmnet")

# Elastic Net (se especifica el parámetro de mixture entre 0 y 1)
# Tomemos un valor de 0.5 para empezar
elastic_net_spec <- linear_reg(penalty = lambda, mixture = .5) %>%
  set_engine("glmnet")


# Primera receta
rec_1 <- recipe(price ~ total_rooms +surface_total + bathrooms + bedrooms + property_type + distancia_universidades +
                  distancia_bus  + distancia_policia + distancia_concesionarios + distancia_parque , data = db) %>%
  step_interact(terms = ~ total_rooms:bedrooms+bathrooms:property_type) %>% 
  step_novel(all_nominal_predictors()) %>% 
  step_dummy(all_nominal_predictors()) %>% 
  step_zv(all_predictors()) %>% 
  step_normalize(all_predictors())

# Segunda receta 
rec_2 <- recipe(price ~ total_rooms + surface_total + bathrooms + bedrooms + property_type + area_universidades + 
                  area_comercial + area_parques + distancia_bus  +
                  distancia_bus + distancia_policia , data = db) %>%
  step_interact(terms = ~ total_rooms:bedrooms+bathrooms:property_type) %>% 
  step_novel(all_nominal_predictors()) %>% 
  step_dummy(all_nominal_predictors()) %>% 
  step_zv(all_predictors()) %>% 
  step_normalize(all_predictors())

##Preguntas_______
train_2 <- as.data.frame(lapply(train_2, as.double))
test_2 <- as.data.frame(lapply(test_2, as.double))


# Crear un flujo de trabajo que incluye la receta de preprocesamiento y el modelo
workflow_1.1 <- workflow() %>%
  add_recipe(rec_1) %>%
  add_model(ridge_spec)

workflow_1.2 <- workflow() %>%
  add_recipe(rec_1) %>%
  add_model(lasso_spec)

workflow_1.3 <- workflow() %>%
  add_recipe(rec_1) %>%
  add_model(elastic_net_spec)


workflow_2.1 <- workflow() %>%
  add_recipe(rec_2) %>%
  add_model(ridge_spec)

workflow_2.2 <- workflow() %>%
  add_recipe(rec_2) %>%
  add_model(lasso_spec)

workflow_2.3 <- workflow() %>%
  add_recipe(rec_2) %>%
  add_model(elastic_net_spec)

# Entrenamos el primer modelo con los datos de train 
fit_1.1 <- workflow_1.1 %>%
  fit(data = train_2)

fit_1.2 <- workflow_1.2 %>%
  fit(data = train_2)

fit_1.3 <- workflow_1.3 %>%
  fit(data = train_2)


fit_2.1 <- workflow_1.1 %>%
  fit(data = train_2)

fit_2.2 <- workflow_2.2 %>%
  fit(data = train_2)

fit_2.3 <- workflow_1.1 %>%
  fit(data = train_2)


# Sacamos las predicciones sobre los datos de test 
predictiones_1.1 <- predict(fit_1.1 , new_data = test_2)

predictiones_1.2 <- predict(fit_1.2 , new_data = test_2)

predictiones_1.3 <- predict(fit_1.3, new_data = test_2)

predictiones_2.1 <- predict(fit_2.1 , new_data = test_2)

predictiones_2.2 <- predict(fit_2.2, new_data = test_2)

predictiones_2.3 <- predict(fit_2.3, new_data = test_2)

submission_template$ID <- 1:nrow(submission_template)

predictiones_1.1$ID <- 1:nrow(predictiones_1.1)
predictiones_1.2$ID <- 1:nrow(predictiones_1.2)
predictiones_1.3$ID <- 1:nrow(predictiones_1.3)
predictiones_2.1$ID <- 1:nrow(predictiones_2.1)
predictiones_2.2$ID <- 1:nrow(predictiones_2.2)
predictiones_2.3$ID <- 1:nrow(predictiones_2.3)

subida <- merge(submission_template,predictiones_2.3, by="ID")

subidafinal = subset(subida, select = -c(ID,price) )

colnames(subidafinal)[2]="price"

write.csv(subidafinal,file='subida25.csv', row.names=FALSE)


####################
##Arboles###########
####################

#___________________________________________________________
#   prediccion
train_2 <- as.data.frame(lapply(train_2, as.double))
test_2 <- as.data.frame(lapply(test_2, as.double))

# Tune grid aleatorio para el modelo de boost
tune_grid_boost <- grid_random(
  trees(range = c(400, 700)),
  min_n(range = c(1, 4)),
  learn_rate(range = c(0.001, 0.01)), size = 20
)

# Especificación del modelo boost_tree en tidymodels
boost_spec <- boost_tree(
  trees = tune(),
  min_n = tune(),
  learn_rate = tune()
) %>%
  set_mode("regression")  #

# Primera receta
rec_1 <- recipe(price ~ total_rooms +surface_total + bathrooms + bedrooms + property_type + distancia_universidades +
                  distancia_bus  + distancia_policia + distancia_concesionarios + distancia_parque + area_parques + area_universidades, data = db) %>%
  step_novel(all_nominal_predictors()) %>% 
  step_dummy(all_nominal_predictors()) %>% 
  step_zv(all_predictors()) 

workflow_1.3 <- workflow() %>%
  add_recipe(rec_1) %>%
  add_model(boost_spec)

train_sff <- st_as_sf(
  train_2,
  # "coords" is in x/y order -- so longitude goes first!
  coords = c("lon", "lat"),
  # Set our coordinate reference system to EPSG:4326,
  # the standard WGS84 geodetic coordinate reference system
  crs = 4326
)
# aplicamos la funcion spatial_block_cv
set.seed(123)
block_folds <- spatial_block_cv(train_sff, v = 5)

autoplot(block_folds)

p_load("purrr")

walk(block_folds$splits, function(x) print(autoplot(x)))

tune_boost <- tune_grid(
  workflow_1.3,
  resamples = block_folds, 
  grid = tune_grid_boost,
  metrics = metric_set(mae)
)

# Utilizar 'select_best' para seleccionar el mejor valor.
best_parms_boost <- select_best(tune_boost, metric = "mae")
best_parms_boost

boost_final <- finalize_workflow(workflow_1.3, best_parms_boost)

# Ajustar el modelo  utilizando los datos de entrenamiento
boost_final_fit <- fit(boost_final, data = test_2)

predictiones_1.3 <- predict(boost_final_fit, new_data = test_2)

submission_template$ID <- 1:nrow(submission_template)

predictiones_1.3$ID <- 1:nrow(predictiones_1.3)

subida <- merge(submission_template,predictiones_1.3, by="ID")

subidafinal = subset(subida, select = -c(ID,price) )

colnames(subidafinal)[2]="price"

write.csv(subidafinal,file='subida26.csv', row.names=FALSE)


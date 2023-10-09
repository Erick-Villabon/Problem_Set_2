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
p_load(rvest, tidyverse, ggplot2, robotstxt, psych, stargazer, boot, plotly, openxlsx, glmnet,
       rio, leaflet, rgeos, tmaptools, sf, osmdata, tidymodels)

# 1. Actualizatr espacio de trabajo 
setwd("C:/Users/Erick/Desktop/Problem_Set_2/stores")
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





#________________________________________________________
#     TRAIN 

table(train$city)

#revisamos los NAs
sapply(train,function(x) sum(is.na(x)))

#imputamos los datos faltantes con la media
train$rooms %>%
  table(useNA = "ifany") %>%
  prop.table() %>%
  round(3)*100

train$rooms[is.na(train$rooms)] <- mean(train$rooms, na.rm = TRUE)

train$bathrooms %>%
  table(useNA = "ifany") %>%
  prop.table() %>%
  round(3)*100

train$bathrooms[is.na(train$bathrooms)] <-  mean(train$bathrooms, na.rm = TRUE)

#creación de variable dummy

tipo_vivienda <- c("Apartamento", "Casa") 
table(train$property_type)

train$property_type_dicotomica <- ifelse(train$property_type == "Apartamento", 1, 0)

#observamos la locación
leaflet() %>%
  addTiles() %>%
  addCircles(lng = train$lon,
             lat = train$lat)

# Ahora solo nos quedaremos con las observaciones que efectivamente están dentro de Chapinero
limites_b <- getbb("Localidad Teusaquillo, Bogotá")
limites_c <- getbb("Localidad Barrios Unidos, Bogotá")

# Filtrar los datos para incluir las coordenadas dentro de los límites de ambas localidades
train <- train %>%
  filter(
    (between(lon, limites_b[1, "min"], limites_b[1, "max"]) &
       between(lat, limites_b[2, "min"], limites_b[2, "max"])) |
      (between(lon, limites_c[1, "min"], limites_c[1, "max"]) &
         between(lat, limites_c[2, "min"], limites_c[2, "max"]))
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




#Creamos variables a partir de la descripcion
train <- train %>%
  mutate(parqueadero = as.numeric(grepl("parqueadero", train$description)))


#___________________________________________________________
#   prediccion

nrow(test)/nrow(train)

#### aplicar cross validation

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
rec_1 <- recipe(price ~ bedrooms + rooms + bathrooms + property_type_dicotomica + parqueadero, data = train) %>%
  step_novel(all_nominal_predictors()) %>% 
  step_dummy(all_nominal_predictors()) %>% 
  step_zv(all_predictors()) %>% 
  step_normalize(all_predictors())


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


# Entrenamos el primer modelo con los datos de train 
fit_1.1 <- workflow_1.1 %>%
  fit(data = train)

fit_1.2 <- workflow_1.2 %>%
  fit(data = train)


fit_1.3 <- workflow_1.3 %>%
  fit(data = train)


predictiones_1.1 <- predict(fit_1.1 , new_data = test)

predictiones_1.2 <- predict(fit_1.2 , new_data = test)

predictiones_1.3 <- predict(fit_1.3, new_data = test)








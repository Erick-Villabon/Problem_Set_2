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

for (i in 1:nrow(test)) {
  match <- str_match(test$description[i], "(\\d+)\\s?(mts|m2)")
  if (!is.na(match[1])) {
    test$n_surface_total[i] <- as.integer(match[2])
  }
}
summary(test$n_surface_total)

##Tenemos mas del 50% de NA


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
#limites <- getbb("Localidad Chapinero Bogotá")
#test <- test %>%
 # filter(
  #  between(lon, limites[1, "min"], limites[1, "max"]) & 
   #   between(lat, limites[2, "min"], limites[2, "max"])
  #)


#test <- test %>%
 # mutate(color = case_when(property_type == "Apartamento" ~ "#2A9D8F",
  #                         property_type == "Casa" ~ "#F4A261"))


# Encontramos el queremos que sea el centro del mapa 
#latitud_central <- mean(test$lat)
#longitud_central <- mean(test$lon)

# Creamos el plot
#leaflet() %>%
 # addTiles() %>%
  #setView(lng = longitud_central, lat = latitud_central, zoom = 12) %>%
  #addCircles(lng = test$lon, 
   #          lat = test$lat, 
    #         col = test$color,
     #        fillOpacity = 1,
      #       opacity = 1)


#Creamos variables a partir de la descripcion
test <- test %>%
  mutate(parqueadero = as.numeric(grepl("parqueadero", test$description)))
test <- test %>%
  mutate(color = case_when(property_type == "Apartamento" ~ "#2A9D8F",
                           property_type == "Casa" ~ "#F4A261"))





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

for (i in 1:nrow(train)) {
  match <- str_match(train$description[i], "(\\d+)\\s?(mts|m2)")
  if (!is.na(match[1])) {
    train$n_surface_total[i] <- as.integer(match[2])
  }
}
summary(train$n_surface_total)

##Tenemos mas del 50% de NA


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
limites_d <- getbb("Localidad Usaquén, Bogotá")

# Filtrar los datos para incluir las coordenadas dentro de los límites de ambas localidades
train <- train %>%
  filter(
    (between(lon, limites_b[1, "min"], limites_b[1, "max"]) &
       between(lat, limites_b[2, "min"], limites_b[2, "max"])) |
      (between(lon, limites_c[1, "min"], limites_c[1, "max"]) &
         between(lat, limites_c[2, "min"], limites_c[2, "max"]))|
          (between(lon, limites_d[1, "min"], limites_d[1, "max"]) &
           between(lat, limites_d[2, "min"], limites_d[2, "max"]))
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


#___________________________________________________________
#   prediccion
nrow(test)/nrow(train)

db <- rbind(test, train)

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
rec_1 <- recipe(price ~ rooms + bathrooms + bedrooms + property_type, data = db) %>%
  step_interact(terms = ~ bathrooms:property_type+bedrooms:property_type) %>% 
  step_novel(all_nominal_predictors()) %>% 
  step_dummy(all_nominal_predictors()) %>% 
  step_zv(all_predictors()) %>% 
  step_normalize(all_predictors())

# Segunda receta 
rec_2 <- recipe(price ~ rooms + bathrooms + bedrooms + parqueadero, data = db) %>%
  step_interact(terms = ~ bathrooms:rooms+bedrooms:rooms) %>% 
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
  fit(data = train)

fit_1.2 <- workflow_1.2 %>%
  fit(data = train)

fit_1.3 <- workflow_1.3 %>%
  fit(data = train)


fit_2.1 <- workflow_1.1 %>%
  fit(data = train)

fit_2.2 <- workflow_2.2 %>%
  fit(data = train)

fit_2.3 <- workflow_1.1 %>%
  fit(data = train)


# Sacamos las predicciones sobre los datos de test 
predictiones_1.1 <- predict(fit_1.1 , new_data = test)

predictiones_1.2 <- predict(fit_1.2 , new_data = test)

predictiones_1.3 <- predict(fit_1.3, new_data = test)

predictiones_2.1 <- predict(fit_2.1 , new_data = test)

predictiones_2.2 <- predict(fit_2.2, new_data = test)

predictiones_2.3 <- predict(fit_2.3, new_data = test)

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

write.csv(subidafinal,file='subida9.csv', row.names=FALSE)

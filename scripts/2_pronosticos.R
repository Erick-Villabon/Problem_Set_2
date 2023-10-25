#___________________________________________________________
#
#                PREDUCCIONES
#
#___________________________________________________________
#___________________________________________________________
# - Limpiar espacio de trabajo

rm(list = ls())

# - Librerias y paquetes 

library(pacman)
p_load(rvest, tidyverse, ggplot2, robotstxt, psych, stargazer, boot, plotly, openxlsx, glmnet,
       rio, leaflet, rgeos, tmaptools, sf, osmdata, tidymodels, writexl, 
       units, randomForest, rattle, spatialsample)

# - Revisar el espacio de trabajo

#setwd("/Users/juandiego/Desktop/GitHub/Problem_Set_2/stores")
setwd("C:/Users/Erick/Desktop/Problem_Set_2/stores")
getwd()
list.files()

# 1. Importar las bases de datos ya preparadas enteriormente

test <- read.xlsx("test_2.xlsx")

train <- read.xlsx("train_2.xlsx")

submission_template <- read.csv("submission_template.csv")

db <- read.xlsx("db.xlsx") 



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

write.csv(subidafinal,file='subida33.csv', row.names=FALSE)










####################
##Arboles###########
####################

#___________________________________________________________
#   prediccion
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
rec_1 <- recipe(price ~ total_rooms + surface_total + bathrooms + bedrooms + property_type + area_universidades + 
                  area_comercial + area_parques + distancia_bus  +
                  distancia_bus + distancia_policia , data = db) %>%
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

##Preguntas_______
##train_2 <- as.data.frame(lapply(train_2, as.double))
##test_2 <- as.data.frame(lapply(test_2, as.double))

boost_final_fit <- boost_final %>%
  fit(data = train_2)

# Ajustar el modelo  utilizando los datos de entrenamiento
#boost_final_fit <- fit(boost_final, data = test_2)

predictiones_1.3 <- predict(boost_final_fit, new_data = test_2)

submission_template$ID <- 1:nrow(submission_template)

predictiones_1.3$ID <- 1:nrow(predictiones_1.3)

subida <- merge(submission_template,predictiones_1.3, by="ID")

subidafinal = subset(subida, select = -c(ID,price) )

colnames(subidafinal)[2]="price"

write.csv(subidafinal,file='subida32.csv', row.names=FALSE)


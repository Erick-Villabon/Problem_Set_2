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
       rio, leaflet, rgeos, modeldata, vtable, tmaptools, sf, osmdata, tidymodels, writexl, 
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

sapply(db,function(x) sum(is.na(x)))

db<- db %>% mutate(areaxparques=area_parques*distancia_parque)
train<- train %>% mutate(areaxparques=area_parques*distancia_parque)
test<- test %>% mutate(areaxparques=area_parques*distancia_parque)

db<- db %>% mutate(areaxuniversidades=area_universidades*distancia_universidades)
train<- train %>% mutate(areaxuniversidades=area_universidades*distancia_universidades)
test<- test %>% mutate(areaxuniversidades=area_universidades*distancia_universidades)

db<- db %>% mutate(areaxcomerciales=area_comercial*distancia_comercial)
train<- train %>% mutate(areaxcomerciales=area_comercial*distancia_comercial)
test<- test %>% mutate(areaxcomerciales=area_comercial*distancia_comercial)

db<- db %>% mutate(bedroomxbathroom =bathrooms*bedrooms)
train<- train %>% mutate(bedroomxbathroom =bathrooms*bedrooms)
test<- test %>% mutate(bedroomxbathroom =bathrooms*bedrooms)

db<- db %>% mutate(distancia_bus_2 =distancia_bus*distancia_bus)
train<- train %>% mutate(distancia_bus_2 =distancia_bus*distancia_bus)
test<- test %>% mutate(distancia_bus_2 =distancia_bus*distancia_bus)

##________________________________________________________________________
#
#                            Modelo Líneal
#
##________________________________________________________________________

#Modelo 1

rec_1 <- recipe(price ~ surface_total + bathrooms + bedrooms + property_type + 
                  distancia_universidades + distancia_bus  + distancia_policia + 
                  distancia_concesionarios + distancia_parque + estrato, data = db)%>% 
  step_dummy(all_nominal_predictors())

reglineal<-linear_reg()


workf_1<-workflow() %>% 
  add_recipe(rec_1) %>% 
  add_model(reglineal)


fit_1 <- workf_1 %>% 
  fit(data=train) 


y1 <- predict(fit_1, new_data = test) %>% 
  bind_cols(test) 

y1<- y1 %>%
  select(property_id, .pred)

colnames(y1)[".pred"] <- "price"

y1$price <- y1$.pred

y1 <- y1[, -which(names(y1) == ".pred")]

write.table(y1, file = "ML_1.csv", sep = ",", row.names = FALSE, col.names = TRUE)




##________________________________________________________________________
#
#                            Modelo Líneal
#
##________________________________________________________________________


reg1 <- lm(price ~ surface_total + bathrooms + bedrooms + property_type + 
                 distancia_universidades + distancia_bus  + distancia_policia + 
                 distancia_concesionarios + distancia_parque + estrato, data = db)
stargazer(reg1,type="text")


# encontrar el lambda optimo
  
train_fold <- vfold_cv(train, v = 5)

# Primera receta

rec_1 <- recipe(price ~ surface_total + bathrooms + bedrooms + property_type + distancia_universidades +
                  distancia_bus  + distancia_policia + distancia_concesionarios + distancia_parque + estrato , data = db) %>%
  step_interact(terms = ~ estrato:bedrooms+bathrooms:property_type) %>% 
  step_novel(all_nominal_predictors()) %>% 
  step_dummy(all_nominal_predictors()) %>% 
  step_zv(all_predictors()) %>% 
  step_normalize(all_predictors())


# Ridge
ridge_spec <- linear_reg(penalty = tune(), mixture = 1) %>%
  set_mode("regression") %>%
  set_engine("glmnet")

# Lasso
lasso_spec <- linear_reg(penalty = tune(), mixture = 0) %>%
  set_mode("regression") %>%
  set_engine("glmnet")

# Elastic Net
elastic_net_spec <- linear_reg(penalty = tune(), mixture = .5) %>%
  set_mode("regression") %>%
  set_engine("glmnet")

  # Workflows
workflow_1.1 <- workflow() %>%
  add_recipe(rec_1) %>%
  add_model(ridge_spec)

workflow_1.2 <- workflow() %>%
  add_recipe(rec_1) %>%
  add_model(lasso_spec)

workflow_1.3 <- workflow() %>%
  add_recipe(rec_1) %>%
  add_model(elastic_net_spec)

  # Penalización

  penalty_grid <- grid_regular(penalty(range = c(-4, 4)), levels = 30)
  penalty_grid

  tune_res1 <- tune_grid(
    workflow_1.1,         
    resamples = train_fold,  
    grid = penalty_grid,        
    metrics = metric_set(rmse)
  )
  
  tune_res2 <- tune_grid(
    workflow_1.2,         
    resamples = train_fold,  
    grid = penalty_grid,        
    metrics = metric_set(rmse)
  )

  tune_res3 <- tune_grid(
    workflow_1.3,         
    resamples = train_fold,  
    grid = penalty_grid,        
    metrics = metric_set(rmse)
  )

  # Seleccionar el mejor valor de penalización
  best_penalty1 <- select_best(tune_res1, metric = "rmse")
  best_penalty2 <- select_best(tune_res2, metric = "rmse")
  best_penalty3 <- select_best(tune_res3, metric = "rmse")
  
  modelo_1.1 <- finalize_workflow(workflow_1.1, best_penalty1)
  modelo_1.2 <- finalize_workflow(workflow_1.2, best_penalty2)
  modelo_1.3 <- finalize_workflow(workflow_1.3, best_penalty3)
  
  # Entrenamos el primer modelo con los datos de train 
  
  fit_1.1 <- fit(modelo_1.1, data = train)
  fit_1.2 <- fit(modelo_1.2, data = train)
  fit_1.3 <- fit(modelo_1.3, data = train)
  
  # Evaluacipon del modelo
  
  augment(fit_1.1, new_data = train) %>%
    mae(truth = price, estimate = .pred)
  
  augment(fit_1.2, new_data = train) %>%
    mae(truth = price, estimate = .pred)
  
  augment(fit_1.3, new_data = train) %>%
    mae(truth = price, estimate = .pred)

  # Sacamos las predicciones sobre los datos de test 
  
  predictiones_1.1 <- predict(fit_1.1 , new_data = test)
  
  subida <- data.frame(
    property_id = submission_template$property_id, 
    .price = predictiones_1.1
  )
  colnames(subida)[2]<-"price"
  
  write.csv(subida,file='Ridge.csv', row.names=FALSE)
  
  
  predictiones_1.2 <- predict(fit_1.2 , new_data = test)
  
  subida <- data.frame(
    property_id = submission_template$property_id, 
    .price = predictiones_1.2
  )
  colnames(subida)[2]<-"price"
  
  write.csv(subidafinal,file='Lasso.csv', row.names=FALSE)
  
  
    
  predictiones_1.3 <- predict(fit_1.3, new_data = test)
  
  subida <- data.frame(
    property_id = submission_template$property_id, 
    .price = predictiones_1.3
  )
  colnames(subida)[2]<-"price"

  write.csv(subidafinal,file='Elastic_Net.csv', row.names=FALSE)
  
summary(predictiones_1.1)
summary(predictiones_1.2)
summary(predictiones_1.3)





#   prediccion
nrow(test)/nrow(train)

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
rec_1 <- recipe(price ~ surface_total + bathrooms + bedrooms + property_type + distancia_universidades +
                  distancia_bus  + distancia_policia + distancia_concesionarios + distancia_parque + estrato , data = db) %>%
  step_interact(terms = ~ estrato:bedrooms+bathrooms:property_type) %>% 
  step_novel(all_nominal_predictors()) %>% 
  step_dummy(all_nominal_predictors()) %>% 
  step_zv(all_predictors()) %>% 
  step_normalize(all_predictors())

# Segunda receta 
rec_2 <- recipe(price ~ surface_total + bathrooms + bedrooms + property_type + area_universidades + 
                  area_comercial + area_parques + distancia_bus  +
                  distancia_bus + distancia_policia + estrato , data = db) %>%
  step_interact(terms = ~ estrato:bedrooms+bathrooms:property_type) %>% 
  step_novel(all_nominal_predictors()) %>% 
  step_dummy(all_nominal_predictors()) %>% 
  step_zv(all_predictors()) %>% 
  step_normalize(all_predictors())

##Preguntas_______
train <- as.data.frame(lapply(train, as.double))
test <- as.data.frame(lapply(test, as.double))


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
#rec_1 <- recipe(price ~ total_rooms + surface_total + bathrooms + bedrooms + property_type + area_universidades + 
#                  area_comercial + area_parques + distancia_bus  +
#                  distancia_bus + distancia_policia , data = db) %>%
#  step_novel(all_nominal_predictors()) %>% 
#  step_dummy(all_nominal_predictors()) %>% 
#  step_zv(all_predictors()) 

rec_1 <- recipe(price ~ rooms + bathrooms + bedrooms + parqueadero + property_type + 
                  area_comercial + distancia_bus + areaxparques + distancia_bus_2 + surface_total 
                + parqueadero + nuevo + distancia_parque + distancia_universidades + estrato, data = db) %>%
  step_interact(terms = ~ bathrooms:rooms+bedrooms:rooms) %>%
  step_novel(all_nominal_predictors()) %>% 
  step_dummy(all_nominal_predictors()) %>% 
  step_zv(all_predictors()) 

workflow_1.3 <- workflow() %>%
  add_recipe(rec_1) %>%
  add_model(boost_spec)

train_sff <- st_as_sf(
  train,
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

boost_final_fit <- boost_final %>%
  fit(data = train)

# Ajustar el modelo  utilizando los datos de entrenamiento
#boost_final_fit <- fit(boost_final, data = test)

predictiones_1.3 <- predict(boost_final_fit, new_data = test)

submission_template$ID <- 1:nrow(submission_template)

predictiones_1.3$ID <- 1:nrow(predictiones_1.3)

subida <- merge(submission_template,predictiones_1.3, by="ID")

subidafinal = subset(subida, select = -c(ID,price) )

colnames(subidafinal)[2]="price"

write.csv(subidafinal,file='subida40.csv', row.names=FALSE)










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

# 1. Actualizatr espacio de trabajo 
setwd("C:/Users/Erick/Desktop/Problem_Set_2/stores")
getwd()
list.files()

# 1. Importar los Datos
submission_template <- read.csv("submission_template.csv")

test <- read.csv("test.csv")

train <- read.csv("train.csv")

# 2. Limpieza de la base 
dim(submission_template)
dim(test)
dim(train)

test %>%
  count(property_type)

test %>%
  summarise_all(~sum(is.na(.))) %>% transpose()

#vamos a imputar la mediana a los valores faltantes de habitación y baños

test %>%
  count(rooms) %>% head() 

test %>%
  count(bathrooms)

# Calcular la mediana
median(test$rooms)
median(test$bathrooms)
mediana_sup_cubierta <- median(test$surface_covered, na.rm = TRUE)
mediana_sup_total<- median(test$surface_total, na.rm = TRUE)

# Imputar datos faltantes
test <- test %>%
  mutate(rooms = replace_na(rooms, 3),
         bathrooms = replace_na(bathrooms, 2),
         surface_covered = replace_na(surface_covered, mediana_sup_cubierta),
         surface_total = replace_na(surface_total, mediana_sup_total))

test_data <- left_join(test, submission_template,by = "property_id")
test_data$price.x<-NULL

test_data <- test_data %>%
  rename(price = price.y)

#ver la distribución de los precios de los immuebles
summary(test_data$price) %>%
  as.matrix() %>%
  as.data.frame() %>%
  mutate(V1 = scales::dollar(V1))

#calculamos valor del metro cuadrado
test_data <- test_data %>%
  mutate(precio_por_mt2 = round(price / surface_total, 0))

summary(test_data$precio_por_mt2) %>%
  as.matrix() %>%
  as.data.frame() %>%
  mutate(V1 = scales::dollar(V1))

#Filtramos ese outlier que resulta no ser real
test_data <- test_data %>%
  filter(between(precio_por_mt2, 100000,  30e6))


# Visualicemos la distribución de nuestra variable de interés
p <- ggplot(test_data, aes(x = price)) +
  geom_histogram(fill = "darkblue", alpha = 0.4) +
  labs(x = "Valor de venta (log-scale)", y = "Cantidad") +
  scale_x_log10(labels = scales::dollar) +
  theme_bw()
ggplotly(p)










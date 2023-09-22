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
p_load(rvest, tidyverse, ggplot2, robotstxt, psych, stargazer, boot, openxlsx)

# 1. Actualizatr espacio de trabajo 
setwd("C:/Users/Erick/Desktop/Problem_Set_2/stores")
getwd()
list.files()

# 1. Importar los Datos
submission_template <- read.csv("submission_template.csv")

test <- read.csv("test.csv")

train <- read.csv("train.csv")




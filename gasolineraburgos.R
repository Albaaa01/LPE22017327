

options(max.print=10000)
pacman::p_load(httr,tidyverse,leaflet,janitor,readr,sparklyr)
url<-"https://sedeaplicaciones.minetur.gob.es/ServiciosRESTCarburantes/PreciosCarburantes/EstacionesTerrestres/"
install.packages('sparklyr')
install.packages('dplyr')
install.packages('stringr')
install.packages('readxk')
install.packages('leaflet')
library(sparklyr)
library(dplyr)
library(tidyverse)
library(stringr)
library(readxl)
library(leaflet)
library(tidyverse)

ds22017327_33c%>%select(rotulo, latitud, longitud_wgs84, precio_gasoleo_a, localidad, direccion) %>%top_n(-20, precio_gasoleo_a)%>%
  leaflet()%>% addTiles()%>%addCircleMarkers(lng= ~longitud_wgs84, lat = ~latitud, popup = ~rotulo, label = ~precio_gasoleo_a)


ds22017327_34c%>%filter(provincia=='TOLEDO')%>%select(rotulo, latitud, longitud_wgs84, precio_gasoleo_a, provincia, direccion)%>%top_n(10, precio_gasoleo_a) %>% 
  leaflet() %>% addTiles() %>% addCircleMarkers(lng=~longitud_wgs84,lat=~latitud,popup=~rotulo,label= ~precio_gasoleo_a)



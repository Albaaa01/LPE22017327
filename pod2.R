##Lenguajes de programaci�n estadistica
##Profesor: Christian Sucuzhanay Arevalo
##Alumno: Alba M� Laguna Moraleda, EXP:21017327
## POD 2

library(tidyverse,httr)
df4<-httr::GET('https://www.kaggle.com/code/barbaramoreno/proyecto-con-db-employee-future-prediction/data?select=Employee.csv')
xml2::read_xml(df4$content)
glimpse(df4)
view(df4)
url_<-'https://www.kaggle.com/code/barbaramoreno/proyecto-con-db-employee-future-prediction/data?select=Employee.csv'

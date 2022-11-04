


library(tidyverse)
url<-'https://sedeaplicaciones.minetur.gob.es/ServiciosRESTCarburantes/PreciosCarburantes/EstacionesTerrestres/'
gln<-httr::GET('https://sedeaplicaciones.minetur.gob.es/ServiciosRESTCarburantes/PreciosCarburantes/EstacionesTerrestres/')
#Clasificamos por gasolineras baratas y no baratas 
f_raw<-jsonlite::fromJSON(url)

gln_source<-f_raw$ListaEESSPrecio %>% glimpse()
df2<-gln_source %>% janitor::clean_names() %>% type_convert(locale=locale(decimal_mark=","))
df2%>%mutate(expensive=rotulo %in%c("CEPSA","REPSOL","BP","SHELL"))%>%view()
ds22017327_33c<-df2%>%mutate(expensive=!rotulo %in%c("CEPSA","REPSOL","BP","SHELL")) #no caras
df_low%>%select(precio_gasoleo_a,idccaa,rotulo,expensive)%>%drop_na()%>%group_by(idccaa,expensive)%>%summarise(mean(precio_gasoleo_a))%>%data.frame("Comunidades"= c("Andalucia","Aragón","Principado de Asturias","Islas Baleares","Canarias","Cantabria","CastillaLeon","CastillaLaMancha","Cataluña","ComunidadValenciana","Extremadura","Galicia","Madrid","Murcia","Navarra","Paisvasco","Rioja","Ceuta","Melilla"))%>%view()

ds22017327_34c<-ds22017327_33c%>%mutate(Comunidades=case_when(idccaa=='01'~'Andalucía',
                                                              idccaa=='02'~'Aragón',
                                                              idccaa=='03'~'Asturias',
                                                              idccaa=='04'~'Islas Baleares',
                                                              idccaa=='05'~'Islas Canarias',
                                                              idccaa=='06'~'Cantabria',
                                                              idccaa=='07'~'Castilla y León',
                                                              idccaa=='08'~'Castilla La Mancha',
                                                              idccaa=='09'~'Cataluña',
                                                              idccaa=='10'~'Comunidad Valenciana',
                                                              idccaa=='11'~'Extremadura',
                                                              idccaa=='12'~'Galicia',
                                                              idccaa=='13'~'Comunidad de Madrid',
                                                              idccaa=='14'~'Region de Murcia',
                                                              idccaa=='15'~'Navarra',
                                                              idccaa=='16'~'País Vasco',
                                                              idccaa=='17'~'La Rioja',
                                                              idccaa=='18'~'Ceuta',
                                                              idccaa=='19'~'Melilla'))%>%view()   


ds22017327_34c%>%tidyr::extract(rotulo,c("franquicias"),"\\b(REPSOL|CEPSA|GALP|SHELL|BP|BALLENOIL|PETRONOR|AVIA|CARREFOUR|PLENOIL|PETROPRIX|Q8|CAMPSA|BONAREA|ESCLATOIL|AGLA|VALCARCE|ALCAMPO|MEROIL|EROSKI|PETRONOR)\\b", remove=F)%>%view()
#NA DE LA COLUMNA FRANQUICIAS INDICA LA MARCA

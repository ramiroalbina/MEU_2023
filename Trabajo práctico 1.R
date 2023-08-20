library(tidyverse)

espacios_culturales <- read.csv("https://cdn.buenosaires.gob.ar/datosabiertos/datasets/ministerio-de-cultura/espacios-culturales/espacios-culturales.csv")

#Voy a quedarme únicamente con los datos correspondientes a la categoría "ESPACIOS ESCÉNICOS".

espacio_escenico <- espacios_culturales %>%
  filter(FUNCION_PRINCIPAL == "ESPACIO ESCENICO")

unique(espacio_escenico$FUNCION_PRINCIPAL)

#En una primera limpieza voy a reducir la cantidad total de variables para quedarme únicamente con aquellas que sean de interés.

colnames(espacio_escenico)

espacio_escenico <- espacio_escenico %>%
  select(FUNCION_PRINCIPAL,ESTABLECIMIENTO,BARRIO,COMUNA,CAPACIDAD_TOTAL)

#En segundo lugar, voy a incorporar una nueva columna que establezca un corte entre aquellos espacios escénicos cuya capacidad supera o igual a la media y aquellos que están por debajo. 

summary(espacio_escenico$CAPACIDAD_TOTAL)

espacio_escenico<-espacio_escenico %>%
  filter(!is.na(CAPACIDAD_TOTAL))

espacio_escenico <- espacio_escenico %>%
  mutate(capacidad_total_rangos=case_when(CAPACIDAD_TOTAL<250~"Menor a 250",
                                          CAPACIDAD_TOTAL>=250~"Mayor a 250"))


#PRIMER PREGUNTA: Ranking de 10 espacios escénicos con mayor capacidad

Top10_Capacidad <- espacio_escenico %>%
  arrange(desc(CAPACIDAD_TOTAL))%>%
  head(10)

print(Top10_Capacidad) 

#SEGUNDA PREGUNTA: ¿Cuántos espacios escénicos tiene cada comuna en total, y cuántos con una capacidad igual o superior a 250

Agrupado_Comuna <- espacio_escenico %>%
  group_by(COMUNA)%>%
  summarise(conteo=n(),
            conteo_2=sum(capacidad_total_rangos=="Mayor a 250"))

print(Agrupado_Comuna)

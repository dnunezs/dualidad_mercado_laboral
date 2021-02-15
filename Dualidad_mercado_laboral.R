#######################
# Librerias       #####
#######################
library(readr)
library(reshape)
library(ggfortify)
library("plyr")

#######################
# Temporales      #####
#######################

# Ocupados 1987 - 1995
ocu8795 <- read.csv2("https://www.ine.es/jaxi/files/_px/es/csv_bdsc/t22/e308/meto_02/rde/px/l0/03048.csv_bdsc?nocab=1")
tem8795 <- subset(ocu8795, ocu8795$"ï..Tipo.de.contrato.o.relaciÃ³n.laboral" == "- Temporal" & ocu8795$"Grupos.de.edad..aÃ.os." == "total")
tem8795 <- tem8795[,-1:-2]

# Ocupados 1996 - 2001
ocu9601 <- read.csv2("https://www.ine.es/jaxi/files/_px/es/csv_bdsc/t22/e308/meto_05/meto_05_bis/rde/px/l0/03048.csv_bdsc?nocab=1")
temp9601 <- subset(ocu9601, ocu9601$"ï..Tipo.de.contrato.o.relaciÃ³n.laboral" == "- Temporal" & ocu9601$"Grupos.de.edad..aÃ.os." == "total")
temp9601 <- temp9601[,-1:-2]

# Ocupados 2002 - 2020
ocu0220 <- read.csv2("https://www.ine.es/jaxiT3/files/t/es/csv_bdsc/4238.csv?nocab=1")
temp0220 <- subset(ocu0220, ocu0220$"Tipo.de.contrato.o.relaciÃ³n.laboral" == "Temporal: Total" & ocu0220$Edad == "Total" & ocu0220$"ï..Sexo" == "Ambos sexos"	)
temp0220 <- temp0220[,-1:-3]
temp0220 = rename(temp0220, c("X" ="Total"))

temporales <- rbind(temp0220, temp9601, tem8795  )
temporales = rename(temporales, c("Total" = "Temporales")) 

#######################
# Parados   ###########
#######################

# Parados 1987 - 1995
par8795 <- read.csv2("https://www.ine.es/jaxi/files/_px/es/csv_bdsc/t22/e308/meto_02/rde/px/l0/04001.csv_bdsc?nocab=1")
parados8795 <- subset(par8795, par8795$"ï..Grupos.de.edad...aÃ.os." == "total" & par8795$Sexo == "ambos sexos")
parados8795 <- parados8795[,-1:-2]

# Parados 1996 - 2001
par9601 <- read.csv2("https://www.ine.es/jaxi/files/_px/es/csv_bdsc/t22/e308/meto_05/meto_05_bis/rde/px/l0/04001.csv_bdsc?nocab=1")
parados9601 <- subset(par9601, par9601$"ï..Grupos.de.edad...aÃ.os." == "total" & par9601$Sexo == "ambos sexos")
parados9601 <- parados9601[,-1:-2]

# Parados 2002 - 2020
par0220 <- read.csv2("https://www.ine.es/jaxiT3/files/t/es/csv_bdsc/4084.csv?nocab=1")
parados0220 <- subset(par0220, par0220$"ï..Edad" == "Total" & par0220$Sexo == "Ambos sexos" & par0220$Unidad == "Valor absoluto")
parados0220 <- parados0220[,-1:-3]

parados <- rbind(parados0220, parados9601, parados8795)
parados = rename(parados, c("Total" ="Parados"))

#######################
# Activos   ###########
#######################

# Activos 1976 - 1995
actv7695 <- read.csv2("https://www.ine.es/jaxi/files/_px/es/csv_bd/t22/e308/meto_02/pae/px/l0/01001.csv_bd?nocab=1", sep = "\t")
Activos7695 <- subset(actv7695, actv7695$"Grupos.de.edad..4." == "Total"& actv7695$"RelaciÃ³n.con.la.actividad.econÃ³mica" == "Activos" & actv7695$"ï..Sexo" == "Ambos sexos"  )
Activos7695 <- Activos7695[,-1:-3]

# Activos 1996 - 2001
actv9601 <- read.csv2("https://www.ine.es/jaxi/files/_px/es/csv_bd/t22/e308/meto_05/meto_05_bis/pae/px/l0/01001.csv_bd?nocab=1", sep = "\t")
Activos9601 <- subset(actv9601, actv9601$"Grupos.de.edad..4." == "Total"& actv9601$"RelaciÃ³n.con.la.actividad.econÃ³mica" == "Activos" & actv9601$"ï..Sexo" == "Ambos sexos"  )
Activos9601 <- Activos9601[,-1:-3]

# Activos 2002 - 2020
actv0220 <- read.csv2("https://www.ine.es/jaxiT3/files/t/es/csv_bd/4049.csv?nocab=1", sep = "\t")
Activos0220 <- subset(actv0220, actv0220$"ï..Edad" == "Total" & actv0220$Sexo == "Ambos sexos" & actv0220$Unidad == "Valor absoluto")
Activos0220 <- Activos0220[,-1:-3]

activos <- rbind(Activos7695, Activos9601, Activos0220)
activos = rename(activos, c("Total" ="Activos"))


#######################
# DF junto   ##########
#######################

Total <- merge(temporales, parados,  by = "Periodo", name.x = "Temporales" )
Total <- merge(Total, activos, by = "Periodo", name.y = "Activos" )

# Pasamos los datos a numerico
Total$Temporales <- gsub("\\.", "", Total$Temporales)
Total$Temporales <- as.numeric(gsub("\\,", ".", Total$Temporales))
Total$Parados <- gsub("\\.", "", Total$Parados)
Total$Parados <- as.numeric(gsub("\\,", ".", Total$Parados))
Total$Activos <- gsub("\\.", "", Total$Activos)
Total$Activos <- as.numeric(gsub("\\,", ".", Total$Activos))

# Calculamos las tasas de parados y temporales
Total$TasaParo <- Total$Parados / Total$Activos *100
Total$TasaTemporalidad <- Total$Temporales / Total$Activos *100

Total$TasaPrecariedad <- Total$TasaParo + Total$TasaTemporalidad

#######################
# Visualización   #####
#######################
library(tidyr)
library(dplyr)

# Modificamos el formato de las fechas que ofrece la EPA
Total$date <- Total$Periodo
Total$date <- gsub("IV", "-10-01", Total$date)
Total$date <- gsub("III", "-07-01", Total$date)
Total$date <- gsub("II", "-04-01", Total$date)
Total$date <- gsub("I", "-01-01", Total$date)
Total$date <- gsub("T4", "-10-01", Total$date)
Total$date <- gsub("T3", "-07-01", Total$date)
Total$date <- gsub("T2", "-04-01", Total$date)
Total$date <- gsub("T1", "-01-01", Total$date)
Total$date <- gsub("T", "", Total$date)
Total$date <- as.Date(Total$date, format="%Y-%m-%d")

# Creamos un df con solo dos variables para su visualizacion
df <- Total %>%
  select(date, TasaParo, TasaTemporalidad) %>%
  gather(key = "variable", value = "value", -date)

# Visualizamos
ggplot(df, aes(x = date, y = value)) +
  geom_line(aes(color = variable), size = 2) +
  scale_color_manual(values=c("red","blue"))+
  theme_minimal() +
  labs(
    x = "Años",             
    y = "%",   
    title = "Evolución de la tasa de paro y temporalidad en España",   
    color = ""   
  )
  
  
df <- Total %>%
  select(date, TasaPrecariedad, TasaParo, TasaTemporalidad) %>%
  gather(key = "variable", value = "value", -date)

ggplot(df, aes(x = date, y = value)) +
  geom_line(aes(color = variable), size = 2) +
  scale_color_manual(values=c("red", "grey","blue"))+
  theme_minimal() +
  labs(
    x = "Años",             
    y = "%",   
    title = "Evolución de la tasa de paro, temporalidad y precariedad en España",   
    color = ""   
  )


#Pregunta 1 
#A 
once<- formatC(1:9999, width=4, flag= "0")
print(once) #Respuesta
#B
#Funciones
digitsum <- function(x) {
  sum(floor(x / 10^(0:(nchar(x) - 1))) %% 10)
}
getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}
#Aplicar funciones
once_numericos<- as.numeric(once)
once_numericos<-sapply(once_numericos, digitsum)
summary(once_numericos)
respuesta<- getmode(once_numericos)
respuesta #Respuesta

#Pregunta 2

#A
#Utiliza el path que sea necesario
CodCCAA <- read.delim("~/Master Big Data/Tarea programación R/Datos/Cod_19/CodCCAA.csv", header=TRUE)
datos_provincias <- read.csv("~/Master Big Data/Tarea programación R/Datos/Cod_19/datos_provincias.csv",header=TRUE)
CodProv <- read.csv("~/Master Big Data/Tarea programación R/Datos/Cod_19/CodProv.txt",header=TRUE)
datos_provincias$Código <- Map(paste, 'ES-', datos_provincias$provincia_iso)
datos_provincias$Código <- gsub(" ", "", as.character(datos_provincias$Código))
datos_provincias <- merge(datos_provincias, CodProv, by="Código")
datos_provincias <- datos_provincias[-2]
datos_provincias$CodCCAA <- Map(paste, 'ES-', datos_provincias$Comunidad.autónoma)
datos_provincias$CodCCAA <- gsub(" ", "", as.character(datos_provincias$CodCCAA))
datos_provincias <- datos_provincias[-9] #Respuesta

#B
05334161%%17 #Asturias
Asturias <- datos_provincias[datos_provincias$CodCCAA == "ES-AS",] #Respuesta

#C
str(Asturias)
Asturias$fecha <- as.Date(Asturias$fecha) #Cambiamos las fechas a datos de fecha para crear mejores gráficos
#Esta librería es muy buena para hacer gráficos ya que dispone de procedimientos gráficos más avanzados para la construcción de figuras
library(ggplot2)
#Este grafico de barras muestra muy bien la evolucion y los picos a lo largo del 2020 mostrando dos oleadas donde el numero de casos crece.
ggplot(Asturias, aes(x=fecha, y=num_casos)) + geom_bar(stat="identity", width=0.1) +
  theme_classic() +
  labs(title = "Evolución de los casos nuevos en Asturias", x= "2020", y= "Número de casos") +
  theme(plot.title = element_text(hjust = 0.5)) #Respuesta

#D
#Este gráfico no tiene mucho sentido ya que todos los datos de Asturias son recogidos con pruebas PCR con lo cual las líneas se superponen para el total de casos y las prueba por PCR
#El resto de columnas son iguales a cero con lo cual también se superponen
ggplot(Asturias, aes(fecha)) + 
  geom_line(aes(y = num_casos_prueba_pcr, colour = "PCR")) +
  geom_line(aes(y = num_casos_prueba_test_ac, colour = "AC")) +
  geom_line(aes(y = num_casos_prueba_desconocida, colour = "Desconocida")) +
  geom_line(aes(y = num_casos_prueba_otras, colour = "Otras pruebas")) +
  labs(title = "Evolución de los casos nuevos en Asturias", x= "2020", y= "Número de casos") +
  theme(plot.title = element_text(hjust = 0.5))#Respuesta
# Voy a probar el mismo gráfico pero con otros datos para demostrar que si que funiona bien
Valencia <- datos_provincias[datos_provincias$CodCCAA == "ES-VC",]
Valencia$fecha <- as.Date(Valencia$fecha) #Cambiamos las fechas a datos de fecha para crear mejores gráficos
ggplot(Valencia, aes(fecha)) + 
  geom_line(aes(y = num_casos_prueba_pcr, colour = "PCR")) +
  geom_line(aes(y = num_casos_prueba_test_ac, colour = "AC")) +
  geom_line(aes(y = num_casos_prueba_desconocida, colour = "Desconocida")) +
  geom_line(aes(y = num_casos_prueba_otras, colour = "Otras pruebas")) +
  labs(title = "Evolución de los casos nuevos", x= "2020", y= "Número de casos") +
  theme(plot.title = element_text(hjust = 0.5))
  
  
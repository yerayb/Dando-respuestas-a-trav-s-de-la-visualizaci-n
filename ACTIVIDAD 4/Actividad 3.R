####################################################################
# Configurando el entorno de trabajo
####################################################################
rm(list=ls())
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
getwd()
dir()
graphics.off()
cat("\014")

###################################################################
#Algunos Paquetes necesarios
###################################################################


library(tm) # Minería de datoss
library(SnowballC) 
library(wordcloud) #Sacar graficos de nubes de palabras
library(ggplot2) #Hacer Graficos
library(dplyr) # Manipular y Transformar datos
library(readr) # Leer y escribir documentos
library(cluster) # Analisis de grupos
library(stringr) 

##----  RETO 1 ---- ##

#Importar dataset
datos1 <- read.csv("cotoarticles20160603.csv",header = TRUE , row.names = NULL) 

#Data cleaning
# Eliminamos duplicados
datos1 = datos1[!duplicated(datos1), ]
str(datos1)
datos1 = na.omit(datos1)

#GRAFICO
require(plyr)
datosprueba = ddply(datos1, .(categories),summarise,price=mean(price)) 
max(datosprueba$price)
categoriaMedia <- data.frame(datosprueba$categories, datosprueba$price ) #columna precio X 100 gr añadida
ggplot(datosprueba, aes(x=datosprueba$categories, y = datosprueba$price)) + geom_col()

##----  RETO 2 ---- ##

#Importar dataset
datos2 <- read.csv("jumboarticles20160603.csv",header = TRUE , row.names = NULL) 

#Data cleaning
# Eliminamos duplicados
datos2 = datos2[!duplicated(datos2), ]
str(datos2)
datos2 = na.omit(datos2)

#Articulos que son de mermelada
mermeladaArt <- datos2 %>% filter(str_detect(name, "ermela"))
precioMermelada <- mermeladaArt$unit_price

#precio por cada 100 gramos
precio_100gramos <- str_sub(precioMermelada,-5,-1)
precio_100gramos <- as.numeric(precio_100gramos)/10
mermeladaArt <- data.frame(mermeladaArt, precio_100gramos ) #columna precio X 100 gr añadida

##----  RETO 3 ---- ##
coto <- read.csv("cotoarticles20160603.csv",header = TRUE , row.names = NULL) 
jumbo <- read.csv("jumboarticles20160603.csv",header = TRUE , row.names = NULL) 

#Data cleaning
# Eliminamos duplicados
coto = coto[!duplicated(coto), ]
str(coto)
coto = na.omit(coto)

jumbo = jumbo[!duplicated(jumbo), ]
str(jumbo)
jumbo = na.omit(jumbo)

#Galletas dataframeCoto
cotoGalletas <- coto %>% filter(str_detect(name, "alleti"))
cotoGalletasPrecio <- as.numeric(cotoGalletas$price)
cotoGalletas <- data.frame(cotoGalletas$name, cotoGalletasPrecio)

#Galletas dataFrameJumbo
jumboGalletas <- jumbo %>% filter(str_detect(name, "alleti"))
jumboGalletasPrecio <- as.numeric(jumboGalletas$price)
jumboGalletas <- data.frame(jumboGalletas$name, jumboGalletasPrecio)

#Visualizacion
boxplot <- boxplot(cotoGalletasPrecio, jumboGalletasPrecio, main="Precio galletitas")

##----  RETO 4 ---- ##
coto <- read.csv("cotoarticles20160603.csv",header = TRUE , row.names = NULL) 
jumbo <- read.csv("jumboarticles20160603.csv",header = TRUE , row.names = NULL) 

#Data cleaning
# Eliminamos duplicados
coto = coto[!duplicated(coto), ]
str(coto)
coto = na.omit(coto)

jumbo = jumbo[!duplicated(jumbo), ]
str(jumbo)
jumbo = na.omit(jumbo)

#Dataframe con media precio por categoria
cat <- jumbo$categories
precio <- as.numeric(jumbo$price)
jumboCatyPrec <- data.frame(aggregate(precio, list(cat), mean)) #nuevo dataframe

#Quantiles
Q  <- quantile(jumboCatyPrec$x)
Q1     <- Q[2]    
Q3     <- Q[4] 
minQ <- Q[1]    
maxQ <- Q[5]    
Med     <- Q[3]  
inf = Q1 - 3 * (Q3-Q1) 
sup = Q3 + 3 * (Q3-1) 
prec.outlier <- data.frame(jumboCatyPrec, abs(jumboCatyPrec$x) > sup| abs(scale(jumboCatyPrec$x)) < inf)
plot(prec.outlier$Group.1, as.character(prec.outlier$x), main="Estudio de Outliers", xlab="Categorias", ylab="Precio", pch=1, col = ifelse(prec.outlier$abs.jumboCatyPrec.x....sup...abs.scale.jumboCatyPrec.x.....inf == TRUE, "red","black"))
media <- mean(precio)
abline(h = mean(precio), col = "brown")

##----  RETO 5 ---- ##

#NOTA: EL CSV DE VEARTICLES NO CONSIGO QUE LO LEA
coto <- read.csv("cotoarticles20160603.csv",header = TRUE , row.names = NULL) 
jumbo <- read.csv("jumboarticles20160603.csv",header = TRUE , row.names = NULL) 

#Data cleaning
# Eliminamos duplicados
coto = coto[!duplicated(coto), ]
str(coto)
coto = na.omit(coto)

jumbo = jumbo[!duplicated(jumbo), ]
str(jumbo)
jumbo = na.omit(jumbo)

#LECHE DE COTO Y JUMBO
lecheCoto <- coto %>% filter(str_detect(name, "^Leche"))
precioLecheCoto <- as.numeric(lecheCoto$price)
lecheCoto <- data.frame(lecheCoto$name, precioLecheCoto)

lecheJumbo <- jumbo %>% filter(str_detect(name, "^Leche"))
precioLecheJumbo <- as.numeric(lecheJumbo$price)
lecheJumbo <- data.frame(lecheJumbo$name, precioLecheJumbo)
boxplotLeche <- boxplot(precioLecheCoto, precioLecheJumbo)





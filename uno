#Lectura de paquetes
ipak <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(new.pkg)) 
    install.packages(new.pkg, dependencies = TRUE)
  sapply(pkg, require, character.only = TRUE)
}

packages <- c("metafor", "esc", "readxl")
ipak(packages)

#InformaciÃ³n clave: http://www.metafor-project.org/doku.php

#Carga de nuestro directorio de trabajo
setwd('C:/Users/al/Desktop/Chupitos')

#Carga de la base de datos si tenemos cada VD en una
#hoja distinta de excel
baseMA <- read_excel('BaseMARiesgoReinci.xlsx',
                      #hay que cambiar en sheet cada hoja 
                     #de la VD.
                      sheet = 'Reincidencia', col_names = TRUE)

#Carga de la base de datos si tenemos una sola hoja excel
#y una variable con las VD del estudio
baseMA <- read_excel('BaseMARiesgoReinci.xlsx')
baseMAreinci<-baseMA[baseMA$VD=='Reincidencia',]

#Ejemplo uso en mi base
baseMAWoS<-baseMA[baseMA$Buscador=='WoS',]


#MA simple usando la d y el error estÃ¡ndar manualmente
resultMA<-rma(yi=baseMA$D, vi=baseMA$V, 
               method="DL",
              weighted=TRUE, 
              level=95, 
               digits=4)

#PresentaciÃ³n de principales resultados:
summary.rma(resultMA)

#Intervalos de confianza para los Ã­ndices de heterogeneidad:
confint(resultMA)

#moderador continuo
ModeradorDuracion<-rma(baseMA$D, vi=baseMA$V, 
                       mods =~ baseMA$N_Sesiones, 
                       method="DL", 
                       weighted=TRUE, 
                       level=95, 
                       digits=4)
summary.rma(ModeradorDuracion)

# Moderador categÃ³ricos:
#Para ver la d de cada una de las categorÃ­as
ModeradorFormato<-rma(baseMA$D, vi=baseMA$V, 
                   mods=~factor(baseMA$Formato)-1, 
                   method="DL", 
                   weighted=TRUE, 
                   level=95, 
                   digits=4)
summary.rma(ModeradorFormato) 

#K en cada categorÃ­a
table(baseMA$Formato)

#para ver si hay efecto del moderador en la d global:
ModeradorFormato2<-rma(baseMA$D, vi=baseMA$V, 
                      mods=~factor(baseMA$Formato), 
                      method="DL", 
                      weighted=TRUE, 
                      level=95, 
                      digits=4)
summary.rma(ModeradorFormato2)

#Test de Egger
regtest(resultMA, model="lm")

#Rosenthal.
fsn(yi=baseMA$D, vi=baseMA$V,type = "Rosenthal")

#AnÃ¡lisis de sensitividad:
leave1out(resultMA)
influenciado<-influence.rma.uni(resultMA)
influenciado$is.infl

# plotting package
library(ggplot2)
# piping / chaining
library(magrittr)
# modern dataframe manipulations
library(dplyr)
library(plyr)
library (tidyverse)
library(plotrix)

#leemos el archivo CSV
basketball_teams <- read.csv("proyectociencia/basketball_teams.csv")
View(basketball_teams) 

#sacar nuevo dataset con los datos que necesitamos.
datasetnuevo <- select(basketball_teams, -(confID),-(confRank),-(playoff),-(o_3pm),
                         -(o_3pa),-(o_oreb),-(o_dreb),-(o_reb),-(o_stl),-(o_to),
                         -(o_blk),-(d_fgm),-(d_fga),-(d_ftm),-(d_fta),-(d_3pm),
                         -(d_3pa),-(d_oreb),-(d_dreb),-(d_reb),-(d_asts),-(d_pf),
                         -(d_stl),-(d_to),-(d_blk),-(o_tmRebound),-(d_tmRebound),
                         -(neutWon),-(neutLoss),-(confWon),-(confLoss),-(pace),-(divID),
                         -(attendance))

write.csv(datasetnuevo,"proyectociencia/datasetnuevo.csv")
View(datasetnuevo)

#ligas y su cantidad de equipos la NBA es la mejor liga de basquetbol
#cantidad de equipos en la NBA

nba<-subset(datasetnuevo,datasetnuevo$lgID=="NBA")
nba<-nba %>%
  distinct(nba$name)
NBA<-length(nba$`nba$name`)
NBA

#cantidad de equipos en la ABA
nba<-subset(datasetnuevo,datasetnuevo$lgID=="ABA")
nba<-nba %>%
  distinct(nba$name)
ABA<-length(nba$`nba$name`)
ABA

#cantidad de equipos en la ABL1
nba<-subset(datasetnuevo,datasetnuevo$lgID=="ABL1")
nba<-nba %>%
  distinct(nba$name)
ABL1<-length(nba$`nba$name`)
ABL1

#cantidad de equipos en la NBL
nba<-subset(datasetnuevo,datasetnuevo$lgID=="NBL")
nba<-nba %>%
  distinct(nba$name)
NBL<-length(nba$`nba$name`)
NBL

#cantidad de equipos en la NPBL
nba<-subset(datasetnuevo,datasetnuevo$lgID=="NPBL")
nba<-nba %>%
  distinct(nba$name)
NPBL<-length(nba$`nba$name`)
NPBL

#cantidad de equipos en la liga PBLA
nba<-subset(datasetnuevo,datasetnuevo$lgID=="PBLA")
nba<-nba %>%
  distinct(nba$name)
PBLA <- length(nba$name)
PBLA

Liga<-c("NBA","ABA","ABL1","NBL","NPBL","PBLA")
Cantidad_de_equipos<-c(NBA,ABA,ABL1,NBL,NPBL,PBLA)
equipos_por_ligas<-data.frame(Liga,Cantidad_de_equipos)
View(equipos_por_ligas)
Liga<- paste(Liga, Cantidad_de_equipos)
Liga<- paste(Liga, " equipos", sep = "")

color<-c("#1874CD","green","#757575","yellow","red","#CD6600")
pie(Cantidad_de_equipos,Liga,main = "cantidad de equipos por liga"
    ,col = color, border = color)

#pie(Cantidad_de_equipos, labels=Cantidad_de_equipos,
 #   col = rainbow(6), cex = 1,main="cantidad de equipos por liga", angle = 45,lty = 8)
#legend(x="topleft", inset = c(-0.10,0.20),legend = c("NBA","ABA","ABL1","NBL","NPBL","PBLA")
 #      ,title = "Liga",
  #     fill =  c("red", "yellow", "green","turquoise","blue","Hot Pink"))

#ggplot(basquet, aes(x=Liga, y=Cantidad_de_equipos)) + geom_bar(stat = "identity", width = 0.5,fill=rgb(231/255, 76/255, 60/255),color="white" )+ coord_flip()+theme(panel.background = element_rect(fill = "#EAECEE"))+theme(plot.background = element_rect(fill = "#3498DB"))
write.csv(equipos_por_ligas,"~/8 SEMESTRE/CIENCIA DE DATOS/proyectociencia/equipos_por_ligas.csv")

#===================================================================
#los equipos mas ganadores de la NBA enla temporrada 2010 y 2011 como locales
temporadas<- data.frame(nombre_equipos=c(datasetnuevo$name)
                            ,victorias_en_casa=c(datasetnuevo$homeWon),
                        liga=c(datasetnuevo$lgID)
                        ,a�o=c(datasetnuevo$year))

promtem2010<-(temporadas[temporadas$a�o==2009,]$victorias_en_casa)
View(promtem2010)
pro2010<-mean(promtem2010)
pro2010





#ordenamos de menor a mayor
El_duplicados <- El_duplicados[with(El_duplicados,order(El_duplicados$victorias_en_casa)
                                    ),]

#generamos un subset con el cual buscamos el equipo que tiene mas de 39 victorias
# en la NBA como local
equipos_ganadores <- subset(El_duplicados,El_duplicados$victorias_en_casa >=39)
pro<-mean(equipos_ganadores$victorias_en_casa,na.rm = TRUE)
View(equipos_ganadores)
ggplot(equipos_ganadores, aes(x=victorias_en_casa, y=nombre_equipos)) + geom_bar(stat = "identity", width = 0.5,fill=rgb(127/255, 179/255, 213/255))
#promedio de victorias e casa

write.csv(equipos_ganadores,"~/8 SEMESTRE/CIENCIA DE DATOS/proyectociencia/equipos_ganadores.csv")
View(datasetnuevo)

#==========================================================================================
#los equipos que perdieron mas partidos repcto al promedio de perdidos en el a�os 2010 de la NBA
los<-data.frame(a�o=c(datasetnuevo$year),
                liga=c(datasetnuevo$lgID)
                ,perdidos=c(datasetnuevo$lost)
                ,equipo=c(datasetnuevo$name))

promedio<-(los[los$a�o==2010,]$perdidos)
pro<-mean(promedio)
pro

equiposperdi<-data.frame(los,los$a�o==2010  & los$perdidos>pro &los$liga=="NBA")
View(equiposperdi)
equipos_perdedores<-subset()
ggplot(equipos_perdedores, aes(x=perdidos, y=equipo)) + geom_bar(stat = "identity", width = 0.5,fill=rgb(127/255, 179/255, 213/255))

write.csv(equipos_perdedores,"~/8 SEMESTRE/CIENCIA DE DATOS/proyectociencia/equipos_perdedores.csv")
View(equipos_perdedores)


#====================================================================================
#cuantos el equipo mas defensivo en la historia de la NBA
defe<-data.frame(a�o=c(datasetnuevo$year),equipo=c(datasetnuevo$name),)


# barplot(height=Partidos_Perdidos, names=Equipos,
#         main = "partidos mas perdidos en el 2010 de la NBA",
#         ylim= c(0,100), las=2,space=0.25)
# barplot(ventas, mes,xlab="ventas", ylab="mes",
# border="blue")
#ggplot(data = equipos_perdedores) + geom_histogram(aes(x=perdidos,fill=factor(equipo)),bins=10, position = "identity",alpha = 0.5)

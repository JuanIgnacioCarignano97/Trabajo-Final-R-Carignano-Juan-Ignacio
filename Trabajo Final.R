# Librerias necesarias.
library(xml2)
library(rvest)
library(dplyr)      
library(ggplot2)
library(reshape2)
library(knitr)
library(shiny)
library(plotly)
library(prettydoc)
library(lubridate)
library(plyr)
library(formattable)
library(rmdformats)
library(dygraphs)

# Importacion de tablas.
#Tabla de España.
url.ibex <- "https://www.worldometers.info/world-population/spain-population/"
tmp <- read_html(url.ibex)
tmp <- html_nodes(tmp, "table")
length(tmp)
sapply(tmp, class)
sapply(tmp, function(x) dim(html_table(x, fill = TRUE)))
Spain <- html_table(tmp[[2]], header = T)
Spain

#Tabla Argentina
url.ibex <- "https://www.worldometers.info/world-population/argentina-population/"
tmp <- read_html(url.ibex)
tmp <- html_nodes(tmp, "table")
length(tmp)
sapply(tmp, class)
sapply(tmp, function(x) dim(html_table(x, fill = TRUE)))
Argentina <- html_table(tmp[[2]], header = T)
Argentina

# Manipulacion de la tabla de España.
Spain$Population <- gsub("[,]" , "" , Spain$Population)
Spain$`Yearly %  Change` <- gsub("," , "." , Spain$`Yearly %  Change`)
Spain$`Yearly %  Change` <- gsub("%" , "" , Spain$`Yearly %  Change`)
Spain$`Yearly Change` <- gsub("[,]" , "" , Spain$`Yearly Change`)
Spain$`Migrants (net)` <- gsub("[,]" , "" , Spain$`Migrants (net)`)
Spain$`Urban Pop %` <- gsub("%" , "" , Spain$`Urban Pop %`)
Spain$`Urban Population` <- gsub("[,]" , "" , Spain$`Urban Population`)
Spain$`Country's Share of World Pop` <- gsub("%" , "" , Spain$`Country's Share of World Pop`)
Spain$`World Population` <- gsub("[,]" , "" , Spain$`World Population`)
Spain$Population <- as.numeric(Spain$Population)
Spain$`Yearly %  Change` <- as.numeric(Spain$`Yearly %  Change`)
Spain$`Yearly Change` <- as.numeric(Spain$`Yearly Change`)
Spain$`Migrants (net)` <- as.numeric(Spain$`Migrants (net)`)
Spain$`Urban Pop %` <- as.numeric(Spain$`Urban Pop %`)
Spain$`Urban Population` <- as.numeric(Spain$`Urban Population`)
Spain$`Country's Share of World Pop` <- as.numeric(Spain$`Country's Share of World Pop`)
Spain$`World Population` <- as.numeric(Spain$`World Population`)
Spain <- Spain %>% mutate(País="España")
colnames(Spain) <- c("Año" , "Población" , "Porcentaje de Variación" , "Variación" , "Migración Neta" , "Edad Media" , "Tasa de Fertilidad" , "Densidad de Población por km2" , "Porcentaje de Población Urbana" , "Población Urbana" , "Porcentaje de la Población Mundial" , "Población Mundial" , "Ranking Mundial" , "País")

# Manipulacion de la tabla de Argentina.
Argentina$Population <- gsub("[,]" , "" , Argentina$Population)
Argentina$`Yearly %  Change` <- gsub("," , "." , Argentina$`Yearly %  Change`)
Argentina$`Yearly %  Change` <- gsub("%" , "" , Argentina$`Yearly %  Change`)
Argentina$`Yearly Change` <- gsub("[,]" , "" , Argentina$`Yearly Change`)
Argentina$`Migrants (net)` <- gsub("[,]" , "" , Argentina$`Migrants (net)`)
Argentina$`Urban Pop %` <- gsub("%" , "" , Argentina$`Urban Pop %`)
Argentina$`Urban Population` <- gsub("[,]" , "" , Argentina$`Urban Population`)
Argentina$`Country's Share of World Pop` <- gsub("%" , "" , Argentina$`Country's Share of World Pop`)
Argentina$`World Population` <- gsub("[,]" , "" , Argentina$`World Population`)
Argentina$Population <- as.numeric(Argentina$Population)
Argentina$`Yearly %  Change` <- as.numeric(Argentina$`Yearly %  Change`)
Argentina$`Yearly Change` <- as.numeric(Argentina$`Yearly Change`)
Argentina$`Migrants (net)` <- as.numeric(Argentina$`Migrants (net)`)
Argentina$`Urban Pop %` <- as.numeric(Argentina$`Urban Pop %`)
Argentina$`Urban Population` <- as.numeric(Argentina$`Urban Population`)
Argentina$`Country's Share of World Pop` <- as.numeric(Argentina$`Country's Share of World Pop`)
Argentina$`World Population` <- as.numeric(Argentina$`World Population`)
Argentina <- Argentina %>% mutate(País="Argentina")
colnames(Argentina) <- c("Año" , "Población" , "Porcentaje de Variación" , "Variación" , "Migración Neta" , "Edad Media" , "Tasa de Fertilidad" , "Densidad de Población por km2" , "Porcentaje de Población Urbana" , "Población Urbana" , "Porcentaje de la Población Mundial" , "Población Mundial" , "Ranking Mundial" , "País")

# Union de las tablas.
Tabla <- rbind(Argentina[,c(1,2,6,7,14)], Spain[,c(1,2,6,7,14)])
head(Tabla)
Tabla = Tabla [ , c(1,5,2,3,4)]

# Graficos.
# Garfico Tasa de Fertilidad
tf <- ggplot(data=Tabla, aes(x = Año , y = `Tasa de Fertilidad`, colour = País))
tf <- tf + geom_line(stat = "identity") +
  theme(text = element_text(size = 10)) +
  ggtitle("Evolución de la Tasa de Fertilidad") +
  theme(plot.title = element_text(family = "Comic Sans MS",
                                  size = rel(2),
                                  vjust = 2,
                                  face = "bold",
                                  color = "red",
                                  lineheight = 1.5)) +
  labs(x = "Años" , y = "Tasa de Fertilidad") + 
  theme(axis.title.x = element_text(face = "bold", vjust = -0.5, colour = "blue", size = rel(1.2))) +
  theme(axis.title.y = element_text(face = "bold", vjust = 1, colour = "blue", size = rel(1.2)))
tf
ggplotly(tf)

# Grafico de la Edad Media
em <- ggplot(data=Tabla, aes(x = Año , y = `Edad Media`))
em <- em + geom_line(stat = "identity") +
  theme(text = element_text(size = 10)) +
  ggtitle("Evolución de la Edad Media") +
  theme(plot.title = element_text(family = "Comic Sans MS",
                                  size = rel(2),
                                  vjust = 2,
                                  face = "bold",
                                  color = "red",
                                  lineheight = 1.5)) +
  labs(x = "Años" , y = "Edad Media Promedio") + 
  theme(axis.title.x = element_text(face = "bold", vjust = -0.5, colour = "blue", size = rel(1.2))) +
  theme(axis.title.y = element_text(face = "bold", vjust = 1, colour = "blue", size = rel(1.2))) +
  facet_grid(~País)
em



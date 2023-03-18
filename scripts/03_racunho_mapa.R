## Mapa - Populacoes mestrado Fabricius
## 16/03/2023

## Seguindo tutoriais:
## https://r.geocompx.org/adv-map.html

## Pacotes

pacman::p_load(sf, terra, dplyr, spData)
library(terra)
library(dplyr)
library(spData)
library(spDataLarge)
library(tmap)    # for static and interactive maps
library(leaflet) # for interactive maps
library(ggplot2) # tidyverse data visualization package
library(raster)
library(scales)
library(RColorBrewer)

coord_pontos <- read_delim("dados_brutos_g_amarali/csv/fabricius_mestrado_coordenadas.csv",
                           delim = ";", escape_double = FALSE, trim_ws = TRUE)

alt <- raster::getData("alt", country = "BRA", 
                       mask = TRUE, path = "./mapas/")

bra <- getData("GADM", country = "BRA", level = 1,
              path = "./mapas/")
windows()

plot(alt)

plot(bra)
points(coord_pontos$Longitude, coord_pontos$Latitude, 
       pch = 16, col = mycolors, cex = 0.7)
legend(x = "left", legend = levels(Localidade),
       col = mycolors, pch = 16, cex = 0.4)


Localidade <- as.factor(coord_pontos$Localidade)
is.factor(Localidade)

map_data <- 
  alt +
  geom_point(data=coord_pontos, 
             aes(x=Longitude, y=Latitude), colour= Localidade, 
             fill="Pink",pch=21, size=5, alpha=I(0.7))

### cor!!

nb.cols <- 45
 display.brewer.all(n=NULL, type="qual", select=NULL, exact.n=TRUE, 
                   colorblindFriendly=TRUE)
display.brewer.pal(15, "BrBG")
mycolors <- colorRampPalette(brewer.pal(12, "Paired"))(nb.cols)
?colorRampPalette
?brewer.pal

# Create a ggplot with 18 colors 
# Use scale_fill_manual
ggplot(df) + 
  geom_col(aes(name, Sepal.Length, fill = factor(Sepal.Length))) +
  scale_fill_manual(values = mycolors) +
  theme_minimal() +
  theme(legend.position = "top")

map_data

pontos <- import.csv()
plot(alt)
tm_shape(alt) +
  tm_fill()


## Mapa clados

## Organizacao de dados

tabelatotal<- read_csv("dados_novos/fabricius_folidose_cladosnovos_imput.csv")
tabelatotal$Localidade <- as.factor(tabelatotal$Localidade)

TabTotsNA<- tabelatotal  %>% drop_na()
nrow(TabTotsNA)
TabTotsNA$gymno_clades <- as.factor(TabTotsNA$gymno_clades)

levels(TabTotsNA$gymno_clades)

## Cor

nb.cols2 <- 11
mycolors2 <- colorRampPalette(brewer.pal(12, "Paired"))(nb.cols2)


## Mapa

windows()

plot(alt)

plot(bra)

Longitude <- unique(TabTotsNA$Longitude)
Latitude <- unique(TabTotsNA$Latitude)
oi <- data.frame(Longitude, Latitude)
locs <- data.frame(TabTotsNA[,c(3,5,6)])
locs <- locs %>% levels(locs$Localidade)


points(TabTotsNA$Longitude, TabTotsNA$Latitude, 
       pch = 16, col = levels(TabTotsNA$gymno_clades), cex = 0.7)
legend(x = "left", legend = levels(TabTotsNA$gymno_clades),
       col = levels(TabTotsNA$gymno_clades), pch = 16, cex = 0.6)



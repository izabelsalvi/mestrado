#### Análise de PCA
#### 16/03/2023

install.packages("corrr")
install.packages("ggcorrplot")
install.packages("FactoMineR")
install.packages("factoextra")

library(readr)
library(tidyverse)
library(corrr)
library(ggcorrplot)
library(FactoMineR)
library(factoextra)
library(psych)

fabricius_folidose_cladosnovos_imput <- read_csv("dados/alterados/fabricius_folidose_cladosnovos_imput.csv")
names(fabricius_folidose_cladosnovos_imput)




gymno_folidose_pca <-  fabricius_folidose_cladosnovos_imput[-c(1045,1046,1047),  7:35]
local_nomes <- as.factor(fabricius_folidose_cladosnovos_imput$Localidade[-c(1045,1046,1047)])
length(local_nomes)

gymnofolpca <- data.frame(gymno_folidose_pca, local_nomes)
summary(gymnofolpca)


#pairs.panels(gymnofolpca[,],
#             gap = 0,
#             bg = terrain.colors(5)[gymnofolpca$local_nomes],
#             pch=21)

is.factor(gymnofolpca$local_nomes)

local_nomes <- unique(local_nomes)
local_numero <- c(1:45) 
loc <- data.frame(local_nomes,local_numero)

gymnonomenum <- full_join(loc, gymnofolpca, multiple = "all")
gymnosonum <- gymnonomenum[,-1]

## matriz de correlação
data_normalized <- scale(gymnosonum[,-1])
head(data_normalized)
corr_matrix <- cor(data_normalized)
windows()
ggcorrplot(corr_matrix)


# componentes principais
pcagymno <- prcomp(gymnosonum[,-1], scale = TRUE) # escala?
pcagymno$rotation
fviz_eig(pcagymno)

plot(pcagymno)

#'below code changes the directions of the biplot, if we donot include
#the below two lines the plot will be mirror image to the below one.'

pcagymno$rotation=-pcagymno$rotation
pcagymno$x=-pcagymno$x
# biplot (pcagymno , scale =0, col= c("red", "darkblue") )
?biplot


fviz_pca_var(pcagymno,
              col.var = "contrib", # Color by contributions to the PC
              gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
              repel = TRUE     # Avoid text overlapping
)

windows()


groups <- as.factor(fabricius_folidose_cladosnovos_imput$Localidade[-c(1045,1046,1047)])
clados <- 

fviz_pca_ind(pcagymno,
             col.ind = groups, # color by groups
             addEllipses = TRUE, # Concentration ellipses
             ellipse.type = "confidence",
             legend.title = "Groups",
             repel = TRUE, 
             label = "none"
)



?fviz_pca_ind


## Com os clados

## Limpeza de dados
gymnoclados<- fabricius_folidose_cladosnovos_imput  %>% drop_na()

nrow(gymnoclados$gymno_clades)

gymnoclados$gymno_clades <- as.factor(gymnoclados$gymno_clades)
is.factor(gymnoclados$gymno_clades)
names(gymnoclados)

gymnoanalise_clados <- gymnoclados[,-c(1:6)]



## Matriz de correlação
data_normalized_2 <- scale(gymnoanalise_clados)
head(data_normalized_2)
corr_matrix_2 <- cor(data_normalized_2)
windows()
ggcorrplot(corr_matrix_2)


# componentes principais
pcacla <- prcomp(gymnoanalise_clados, scale = TRUE) # escala?
pcacla $rotation
fviz_eig(pcacla)

plot(pcacla)

pcacla$rotation=-pcacla$rotation
pcacla$x=-pcacla$x
# biplot (pcacla , scale =0, col= c("red", "darkblue") )
?biplot


fviz_pca_var(pcacla,
             col.var = "contrib", # Color by contributions to the PC
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE     # Avoid text overlapping
)

windows()


groups_cla <- as.factor(gymnoclados$gymno_clades)

dev.off()

windows()  
fviz_pca_ind(pcacla,
               col.ind = gymnoclados$gymno_clades, # color by groups
               addEllipses = FALSE, # Concentration ellipses
               ellipse.type = "confidence",
               legend.title = "Groups",
               repel = TRUE, 
               label = "none"
  )


windows()  
fviz_pca_ind(pcacla,
             col.ind = gymnoclados$Localidade, # color by groups
             addEllipses = TRUE, # Concentration ellipses
             ellipse.type = "confidence",
             legend.title = "Groups",
             repel = FAlSE, 
             label = "none"
)
?fviz_pca_ind















############################################
View(gymno_folidose_pca)
?prcomp

all(is.finite(gymnofolpca))
?sapply 
sapply(gymnofolpca, avg)

a <- c()
b <- c()

a <- is.numeric(gymnofolpca[,1])

for (i in 1:29) {
  a <- is.numeric(gymnofolpca[,i])
  b <- c(a, b)
}

d <- c()
for (i in 1:29) {
  c <-which(!is.finite(gymnofolpca[,i]))
  d <- c(c, d)
}

gymnofolpca <- gymnofolpca[-c(1045,1046,1047),]
nrow(gymnofolpca)

# componentes principais
cpgymno <- prcomp(gymnofolpca, scale = TRUE) # escala?
cpgymno$rotation

fviz_eig(cpgymno)


attributes(cpgymno)

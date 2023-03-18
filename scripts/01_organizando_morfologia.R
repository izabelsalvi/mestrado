#### Organizacao dos Dados de Folidose do Fabricius
#### 15/03/2023


library(readr)
library(tidyverse)

## usando a tabela com os dados imputados do proprio fabricius (tem que mudar)

gymno_folidose <- read_delim("dados_brutos_g_amarali/csv/gymno_folidose_imputfab.csv", 
                                    delim = ";", escape_double = FALSE, trim_ws = TRUE)

gymno_coord <- read_delim("dados_brutos_g_amarali/csv/gymno_folidose_coord.csv",
                                    delim = ";", escape_double = FALSE, trim_ws = TRUE)


## criando tabela com clados e localidades
gymno_local_todos <- length(unique(gymno_folidose$Localidade))
gymno_clades <-  c(1, 2, 2, 3, 4, 4, 5, 
                   6, 6, 7, 7, 7,  9, 10,
                   11, 12, 12)
Localidade <- c("SaoDomingos", "ColinasdoSul", "Minacu",
                 "AltoParaiso" , "BarradoGarcas",
                 "NovaXavantina" , "Caseara" ,
                 "SaoSalvadorTocantins" , "Peixe", "Parana",
                 "MonteAlegre", "Cavalcante", "Almas" , "Mateiros",
                 "Lajeado(UHE Luis)", "Carolina" , "Estreito"  )
gymno_claloc <- data.frame(Localidade, gymno_clades)

gymno_loc <- full_join(gymno_coord, gymno_claloc, by = NULL)

gymno_tot <- full_join(gymno_folidose, gymno_loc)
gymno_tot <- gymno_tot[,c(1, 2, 34, 32, 33, 3:31)]
names(gymno_tot)

write.csv(gymno_tot, file = "dados_novos/fabricius_folidose_cladosnovos_imput.csv")


## numero de individuos por clados

numind <- c()

for (i in 1:12) {
  a <- length(which(gymno_tot$gymno_clades == i))
  numind <- c(numind, a) 
} 

cla <- c(1:12)

numindcla <- data.frame(cla, numind)

## numeros de indiviuos sem clados - 218

nrow(gymno_tot) - sum(numind)



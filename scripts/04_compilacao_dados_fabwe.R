## Megazordizacao dos Dados
## 17/03/2023

## Compilacao de todos os dados existentes de acordo 
## com o ID dos bichos

## Pacotes
library(readr)
library(tidyverse)

## Dados de PDF - do Doutorado do Fabricius

Fab_DocCH2_IDTot <- read_delim("dados/brutos/do_pdf/Fab_DocCH2_IDTot.csv",
                                delim = ";")
Fab_DocCH2_IDTot <- Fab_DocCH2_IDTot[-c(33,34,81,82),]
  # espécimens usados no Cap 2 no doutorado do Fabricius,
  # ID do especime = ESPECIE (clados A a H), id genbank

  # detalhes de primers - nao fiz tabela - no doc

Fab_Doc_LocCoord <- read_delim("dados/brutos/do_pdf/Fab_Doc_LocCoord.csv", 
                               delim = ";", escape_double = FALSE, trim_ws = TRUE)
  # coordenadas dos municípios e de cada local de coleta

Fab_DocAP_GenStats <- read_delim("dados/brutos/do_pdf/Fab_DocAP_GenStats.csv", 
                                 delim = ";", escape_double = FALSE, trim_ws = TRUE)
  # estatíticas dos dados genéticos para cada locus (404) utilizado
  # nas análises, num pares de bases, polimorfimsos transversoes, 
  # diversidade de nucleotidios

Fab_DocAP_IDLoci <- read_delim("dados/brutos/do_pdf/Fab_DocAP_IDLoci.csv", 
                               delim = ";", escape_double = FALSE, trim_ws = TRUE)
  # individuos utilizados para as analises AP (Anchored Phylogenomics),
  # ID, e outrasinformacoes sobre loci que nao sei oq significam
head(Fab_DocAP_IDLoci)

Fab_DocCitBAP_IDCoordAlt <- read_delim("dados/brutos/do_pdf/Fab_DocCitBAP_IDCoordAlt.csv", 
                                       delim = ";", escape_double = FALSE, trim_ws = TRUE)
  # individuos usados para sequencia de citocromo b e AP, ID
  # e coord

Fab_DocCitBAP_IDCoordAlt_out <- read_delim("dados/brutos/do_pdf/Fab_DocCitBAP_IDCoordAlt_out.csv", 
                                           delim = ";", escape_double = FALSE, trim_ws = TRUE)
  #  OUTGRUPS -individuos usados para sequencia de citocromo b 
  # e AP, ID e coord



## Dados mestrado Fabricius
fabricius_folidose_cladosnovos_imput <- read_delim("dados/alterados/fabricius_folidose_cladosnovos_imput.csv", 
                                                   delim = ",", escape_double = FALSE, trim_ws = TRUE)
  # individuos chunb e mzusp




## Tentando organizar os dados da chunb

local_ordem <- read_csv("dados/brutos/ordem_local_tombo_colecao.csv",
                        col_names = FALSE)

tombo_col <- read_delim("dados/brutos/tombo_colecao.csv",
                        delim = ";",
                        escape_double = FALSE,
                        col_names = FALSE)
  # importando os dados gerais - local ordem tem os dados da localidade - 
  # usar so para resolver conflitos - numeros de tombo repetidos
  
long_tombo_col <- pivot_longer(tombo_col, -1)
long_tombo_col_sna <- long_tombo_col %>%
  filter(!is.na(value))
long_tombo_col_sna <- long_tombo_col_sna[,c(1,3)]
colnames(long_tombo_col_sna) <- c("Colecao", "Tombo")
  # organizando tabela com tombo e colecao


tombo_local <- cbind(local_ordem,tombo_col)
which(duplicated(long_tombo_col_sna$Tombo))
long_tombo_col_sna[c(800,801),]
  # encontrando dados duplicados
  # 293 - Januaria eh MTJ
  # 293 - Pirenopolis CHUNB
  # 299 - Januaria MTJ
  # 299 - Pirenopolis CHUNB


long_tombo_col_sna <- long_tombo_col_sna %>%
  filter(Colecao != "MTJ" | Tombo != 299)
long_tombo_col_sna <- long_tombo_col_sna %>%
  filter(Colecao != "MTJ" | Tombo != 293)
  # removendo os dados duplicados da colecao do MTJ 



FabMestComp <- left_join(fabricius_folidose_cladosnovos_imput,
                         long_tombo_col_sna)
  # juntando



fabna <- FabMestComp %>%
  filter(is.na(Colecao))

unique(fabna$Localidade)

teste<- FabMestComp %>%
  mutate(Colecao = ifelse(lag(Localidade) == "Almas", "CHUNB", Colecao))
teste <- teste %>%
  mutate(Colecao = ifelse(lag(Localidade) == "Almas", "CHUNB", Colecao))
teste <- teste %>%
  mutate(Colecao = ifelse(lag(Localidade) == "AltoParaiso", "CHUNB", Colecao))
teste$Colecao[140] <- "CHUNB"
teste$Colecao[224] <- "MZUSP"
teste <- teste %>%
  mutate(Colecao = ifelse(lag(Localidade) == "Parana", "CHUNB", Colecao))
teste$Colecao[705] <- "CHUNB"
teste$Colecao[1] <- "CHUNB"
  # organizando e preenchendo os dados que faltaram

FabMestCompleto <- teste[,c(1,2,36,3,4:35)]
write.csv(FabMestCompleto, "dados/alterados/FabMestCompleto.csv")
  # Tabela



## Juntando mestrado com doutorado

ID_total <- FabMestCompleto[,c(2:7)]
FabMestrado <- rep("SIM", times = nrow(ID_total))
ID_total <- cbind(ID_total, FabMestrado)
colnames(ID_total)[1] <- "ID"
  # criando uma tabela de IDs


## Juntando os dados do cap 2 doutorado (arvore molecular com 2 genes)
DocCh2 <- rep("SIM", times = nrow(Fab_DocCH2_IDTot))
FabDocCh2ID <- cbind(Fab_DocCH2_IDTot, DocCh2)

  # deixando a ID apenas com numero
teste2 <- gsub('MTR', "",
     gsub('CHUNB', "",
          gsub("LG", "",
               gsub("GRC", "",
                    gsub("MRT", "",
                         gsub("MD", "",
                              gsub("ESTR", "",
                                   gsub("[[:punct:]]", "",
                                        FabDocCh2ID$ID))))))))

FabDocCh2ID <- subset(FabDocCh2ID, select = -ID)
  # tirar o ID da tablena

ID <- teste2
  # chamar teste 2 de ID
  
FabDocCh2ID <- cbind(ID, FabDocCh2ID)
  # juntar na tabela

ID_total$ID <- as.character(ID_total$ID)

teste3 <- full_join(FabDocCh2ID , ID_total)
teste3 <- teste3[,c(1,8,7,13,9,4,5,11,12,10,3,2,6)]
  # tabela geral -> localidade (mestrado), locality doutorado

teste3$Colecao[c(1:3,26,27)] <- "MTR"
teste3$Colecao[c(68,69,82,83)] <- "MRT"
teste3$Colecao[c(4:7,13:21,30:38,42:46,49:58,
                 62,65:67,70:74,86:88,91:97)] <- "CHUNB"
teste3$Colecao[c(8,47,48,84,85)] <- "LG"
teste3$Colecao[c(9:11, 22:25)] <- "ESTR"
teste3$Colecao[c(28)] <- "LAJ"
teste3$Colecao[c(39:41,56:61,63,64,75:81)] <- "GRC"
teste3$Colecao[c(89,90)] <- "MD"
  # depois,na tabela geral botar as coleções manualmente


ID_total <- teste3
colnames(ID_total)[3] <- "FabDocCh2"
  # tabela dados mestrado doutorado  capitlo 2


## Doutorado Cap 5 - analises AP

FabDocCh4AP <- rep("SIM", times = nrow(Fab_DocAP_IDLoci))
FabAPID <- cbind(Fab_DocAP_IDLoci, FabDocCh4AP)

# deixando a ID apenas com numero
temp1 <- gsub('MTR', "",
               gsub('CHUNB', "",
                    gsub("LG", "",
                         gsub("GRC", "",
                              gsub("MRT", "",
                                   gsub("MD", "",
                                        gsub("ESTR", "",
                                             gsub("[[:punct:]]", "",
                                                  gsub("LAJ", "",
                                                  FabAPID$ID)))))))))

FabAPID <- subset(FabAPID, select = -ID)
ID <- temp1
FabAPID  <- cbind(ID, FabAPID)

temp2 <- full_join(FabAPID[,-2] , ID_total)

teste3$Colecao[c(1:3,26,27)] <- "MTR"
teste3$Colecao[c(68,69,82,83)] <- "MRT"
teste3$Colecao[c(4:7,13:21,30:38,42:46,49:58,
                 62,65:67,70:74,86:88,91:97)] <- "CHUNB"
teste3$Colecao[c(8,47,48,84,85)] <- "LG"
teste3$Colecao[c(9:11, 22:25)] <- "ESTR"
teste3$Colecao[c(28)] <- "LAJ"
teste3$Colecao[c(39:41,56:61,63,64,75:81)] <- "GRC"
teste3$Colecao[c(89,90)] <- "MD"
  # lembrar de colocar os dados do wellington

write.csv(temp2, "apagar/temporario_compilacaoenorme.csv")

  






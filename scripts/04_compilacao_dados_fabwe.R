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


## Dados mestrado Welington

WelSPamarali_hemipenis_medidos <- read_delim("dados/brutos/csv/welington_cladosamarali_hemipenis_jamedidos.csv", 
                                             delim = ";", escape_double = FALSE, trim_ws = TRUE)
HemipenisMedido <- rep("SIM", nrow(WelSPamarali_hemipenis_medidos)) 
WelSPamarali_hemipenis_medidos <- cbind(WelSPamarali_hemipenis_medidos, HemipenisMedido)
WelSPamarali_hemipenis_medidos <- WelSPamarali_hemipenis_medidos[-c(12:18),]
  # Hemipenis ja medidos

WelSPamali_hemipenis_evertido <- read_delim("dados/brutos/csv/welington_ID_clado_hemipenis_evertido.csv", 
                                                       delim = ";", escape_double = FALSE, trim_ws = TRUE)
  # hemipenis ja preparados - falta tirar foto

EspecimesLACV_EmprestimoCHUNB <- read_delim("dados/brutos/csv/Especimes_LACV_EmprestimoCHUNB.csv", 
                                             delim = ";", escape_double = FALSE, trim_ws = TRUE)
  # especimes emprestadas da CHUNB que estao no LACV


## Juntando mestrado com doutorado e Welington mestrado

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

temp2$Colecao[1] <- "MTR"
temp2$Colecao[c(3,5,10,11,17)] <- "GRC"
temp2$Colecao[8] <- "ESTR"
temp2$Colecao[16] <- "MRT"
temp2$Colecao[20] <- "LG"
temp2$Colecao[23] <- "LAJ"

ID_total<- temp2


## Juntando os dados do welington

  # especimes LACV
EspecimesLACV_EmprestimoCHUNB$ID <- as.character(EspecimesLACV_EmprestimoCHUNB$ID)
temp5 <- full_join(EspecimesLACV_EmprestimoCHUNB, ID_total)
temp5$Colecao[1:46] <- "CHUNB"
ID_total <- temp5


  # hemipenis evertido
hemipeve <- subset(WelSPamali_hemipenis_evertido, select = -Colecao)
hemipeve$ID <- as.character(hemipeve$ID)
temp6 <- full_join(hemipeve, ID_total)
temp6$No_LACV[c(2,9)] <-"SIM" 
temp6$Colecao[c(2,9)] <- "LACV"

colnames(temp6)[c(1,17)] <- c("Especie_Welington", "Especie_Fabricius")

ID_total <- temp6


  # hemipenis medido
temphemipmed <- WelSPamarali_hemipenis_medidos[,c(3,11)] 
temphemipmed$ID <- as.character(temphemipmed$ID)
temp8 <- full_join(temphemipmed, ID_total)

temp8$Colecao[c(1,2)] <- "MTR"
temp8$Colecao[c(3,4)] <- "UFMG"
temp8$Colecao[5] <- "LACV"

temp8$Especie_Welington[1] <- "Gymnodactylus darwinii"
temp8$Especie_Welington[2] <- "Gymnodactylus sp1"
temp8$Especie_Welington[3] <- "Gymnodactylus sp2"
temp8$Especie_Welington[4] <- "Gymnodactylus sp3"
temp8$Especie_Welington[5] <- "Gymnodactylus geckoides"

ID_total <- temp8

temp9 <- subset(ID_total, select = c(ID, Colecao, HemipenisMedido, 
                Hemipenis_Evertido, No_LACV, FabMestrado, 
                FabDocCh2, FabDocCh4AP, `Brazilian State`, Localidade,
                Locality,  Latitude, Longitude, Especie_Welington,
                Especie_Fabricius, gymno_clades))

names(ID_total)

write.csv(temp2, "apagar/temporario_compilacaoenorme.csv")

temp7 <- subset(temp6, select = c(Especie, Species))

## so mais uma coisa

welingtonIDCoord <- read_delim("dados/brutos/csv/welington_gerais_id_colecao_coordenadas_fabricius.csv", 
                               delim = ";", escape_double = FALSE, 
                               col_types = cols(`#` = col_character(), 
                               ID = col_skip()), trim_ws = TRUE)

colnames(welingtonIDCoord)[5] <- "ID" 

temp10 <- full_join(welingtonIDCoord[,c(5:7,9:11)], ID_total)
  #localidades welington



  

  # localidades fabricius Fab_DocCitBAP_IDCoordAlt_out Fab_DocCitBAP_IDCoordAlt
FabIDCoordAltAPCit <- rbind(Fab_DocCitBAP_IDCoordAlt, Fab_DocCitBAP_IDCoordAlt_out )

ID2 <- gsub('MTR', "",
                gsub('CHUNB', "",
                     gsub("LG", "",
                          gsub("GRC", "",
                               gsub("MRT", "",
                                    gsub("MD", "",
                                         gsub("ESTR", "",
                                              gsub("[[:punct:]]", "",
                                                   gsub("LAJ", "",
                                                        FabIDCoordAltAPCit$ID)))))))))
  
temp12 <- FabIDCoordAltAPCit 
temp12$ID <- ID2
temp12$Species[1:155] <- NA
colnames(temp12)[3:6] <- c("Loc2", "Estado2", "Lat2", "Long2") 

temp13 <- full_join(temp12, temp10)

## Unindo colunas repetidas  
  
## Localidade
temp13$Localidade <- ifelse(is.na(temp13$Localidade),
                            paste(temp13$Localidade,
                                  temp13$Locality, sep = "_"),
                            temp13$Localidade)
temp13 <- subset(temp13, select = -Locality)

temp13$Localidade <- gsub("_NA", "",
     gsub("NA_", "",
          gsub("NA_NA", NA,
               temp13$Localidade)))

temp13$Localidade <- ifelse(is.na(temp13$Localidade),
                            paste(temp13$Localidade,
                                  temp13$Loc, sep = "_"),
                            temp13$Localidade)

temp13$Localidade <- gsub("_NA", "",
                          gsub("NA_", "",
                               gsub("NA_NA", NA,
                                    temp13$Localidade)))
temp13 <- subset(temp13, select = -Loc)



temp13$Localidade <- ifelse(is.na(temp13$Localidade),
                            paste(temp13$Localidade,
                                  temp13$Loc2, sep = "_"),
                            temp13$Localidade)

temp13$Localidade <- gsub("_NA", "",
                          gsub("NA_", "",
                               gsub("NA_NA", NA,
                                    temp13$Localidade)))
temp13 <- subset(temp13, select = -Loc2)

#pausa p garantias
write.csv(temp13, "apagar/quaselatemp13.csv")

# juntar coordenadas lat
temp13$Latitude <- ifelse(is.na(temp13$Latitude),
                            paste(temp13$Latitude,
                                  temp13$Lat, sep = "_"),
                            temp13$Latitude)
temp13 <- subset(temp13, select = -Lat)
temp13$Latitude <- gsub("_NA", "",
                          gsub("NA_", "",
                               gsub("NA_NA", NA,
                                    temp13$Latitude)))

temp13$Latitude <- ifelse(is.na(temp13$Latitude),
                          paste(temp13$Latitude,
                                temp13$Lat2, sep = "_"),
                          temp13$Latitude)
temp13 <- subset(temp13, select = -Lat2)
temp13$Latitude <- gsub("_NA", "",
                        gsub("NA_", "",
                             gsub("NA_NA", NA,
                                  temp13$Latitude)))


# longitude
temp13$Longitude <- ifelse(is.na(temp13$Longitude),
                          paste(temp13$Longitude,
                                temp13$Long, sep = "_"),
                          temp13$Longitude)
temp13 <- subset(temp13, select = -Long)
temp13$Longitude<- gsub("_NA", "",
                        gsub("NA_", "",
                             gsub("NA_NA", NA,
                                  temp13$Longitude)))

temp13$Longitude <- ifelse(is.na(temp13$Longitude),
                           paste(temp13$Longitude,
                                 temp13$Long2, sep = "_"),
                           temp13$Longitude)
temp13 <- subset(temp13, select = -Long2)
temp13$Longitude<- gsub("_NA", "",
                        gsub("NA_", "",
                             gsub("NA_NA", NA,
                                  temp13$Longitude)))

# juntar altitudes
temp13$`Altitude (m)` <- ifelse(is.na(temp13$`Altitude (m)`),
                           paste(temp13$`Altitude (m)`,
                                 temp13$Altitude, sep = "_"),
                           temp13$`Altitude (m)`)
temp13 <- subset(temp13, select = -Altitude)
temp13$`Altitude (m)`<- gsub("_NA", "",
                        gsub("NA_", "",
                             gsub("NA_NA", NA,
                                  temp13$`Altitude (m)`)))


# juntar estado
temp13$UF <- ifelse(is.na(temp13$UF),
                                paste(temp13$UF,
                                      temp13$Estado2, sep = "_"),
                                temp13$UF)
temp13 <- subset(temp13, select = -Estado2)
temp13$UF<- gsub("_NA", "",
                             gsub("NA_", "",
                                  gsub("NA_NA", NA,
                                       temp13$UF)))

temp13$UF <- ifelse(is.na(temp13$UF),
                    paste(temp13$UF,
                          temp13$`Brazilian State`, sep = "_"),
                    temp13$UF)
temp13 <- subset(temp13, select = -`Brazilian State`)
temp13$UF<- gsub("_NA", "",
                 gsub("NA_", "",
                      gsub("NA_NA", NA,
                           temp13$UF)))

# especie
temp13$Especie_Fabricius <- ifelse(is.na(temp13$Especie_Fabricius),
                    paste(temp13$Especie_Fabricius,
                          temp13$Species, sep = "_"),
                    temp13$Especie_Fabricius)
temp13$Especie_Fabricius<- gsub("_NA", "",
                                gsub("NA_", "",
                                     gsub("NA_NA", NA,
                                          temp13$Especie_Fabricius)))

temp13 <- subset(temp13, select = -Species)

temp14 <- temp13

temp14 <- temp14 %>%
  mutate(Especie_Fabricius = ifelse(lag(is.na(temp14$Especie_Fabricius))), "G. amarali", Especie_Fabricius)

temp14$Especie_Fabricius[is.na(temp14$Especie_Fabricius)] <- "G. amarali"

names(temp14)


write.csv(temp14, "dados/alterados/ID_total_maisbagunca.csv")

temp15<- subset(temp14, select = c(ID, Colecao, Localidade,
                                   UF, Latitude, Longitude, 
                                   `Altitude (m)`, Especie_Fabricius, Especie_Welington,
                                   gymno_clades, HemipenisMedido, 
                                   Hemipenis_Evertido, No_LACV, FabDocCh4AP, 
                                   FabDocCh2, FabMestrado))

write.csv(temp15, "dados/alterados/ID_total.csv")

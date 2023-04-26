## Dados pegar na colecao
## 22-04-2023

## Temporario
#ID_na <- ID_total %>%
#  filter(is.na(FabDocCh4AP) & is.na(FabDocCh2)
#         & is.na(FabMestrado) & is.na(No_LACV) &
#           is.na(Hemipenis_Evertido))

#write.csv(ID_na, "dados/alterados/ID_na.csv")


## Pacotes
library(tidyverse)

## Lendo os dados
ID_total <- read.csv("dados/alterados/ID_total.csv")

## tirando espaco
ID_total$Localidade <- gsub("[[:punct:]]", "",
                            gsub(" ", "",
                                 gsub("ç", "c",
                                      gsub("ó", "o",
                                           gsub("â", "a",
                                                gsub("ã", 'a',
                                                     gsub("ç", "c",
                                                          gsub("Ã³", "o",
                                                               gsub("á", "a",
                                                                    ID_total$Localidade)))))))))
write.csv(ID_total,"dados/alterados/ID_total.csv")



## Nomes dos locais
unique(ID_total$Localidade)

# 16 individuos
Almas <- ID_total %>%
  filter(Localidade == "Almas" & Colecao == c("CHUNB"))
unique(Almas$Colecao)
nrow(Almas)


# Tudo MTR
AlmasEESGT <- ID_total %>%
  filter(Localidade == "AlmasEESGT")
unique(AlmasEESGT$Colecao)
AlmasEESGT$FabDocCh2
AlmasEESGT$FabDocCh4AP
AlmasEESGT$FabMestrado
AlmasEESGT <- AlmasEESGT %>%
  filter(Colecao == "CHUNB" | is.na(Colecao) | Colecao == "GRC" | Colecao == "LACV")
nrow(AlmasEESGT)


# 25 individuos - pego 20
AltoPairaiso <- ID_total %>%
  filter(Localidade == "AltoParaiso")
nrow(AltoPairaiso)
AltoPairaiso <- AltoPairaiso[c(2:22),]

# 1 unico
AltoParnaiba <- ID_total %>%
  filter(Localidade == "AltoParnaiba" & Colecao == c("CHUNB"))
nrow(AltoParnaiba)

# 8 
BarradoGarcas <- ID_total %>%
  filter(Localidade == "BarradoGarcas" | Localidade == "BarradoGarca")
unique(BarradoGarcas$Colecao)
BarradoGarcas <- BarradoGarcas %>%
  filter(Colecao == "CHUNB" | is.na(Colecao))
nrow(BarradoGarcas)

## flop - mzusp
BarraRioSaoDomingos <- ID_total %>%
  filter(Localidade == "BarraRioSaoDomingos")
unique(BarraRioSaoDomingos$Colecao)
BarraRioSaoDomingos <- BarraRioSaoDomingos %>%
  filter(Colecao == "CHUNB" | is.na(Colecao) | Colecao == "GRC")
nrow(BarraRioSaoDomingos)
  
## só 1 - mestrado 
BomJesusTocantins <- ID_total %>%
  filter(Localidade == "BomJesusTocantins")
unique(BomJesusTocantins$Colecao)
BomJesusTocantins <- BomJesusTocantins %>%
  filter(Colecao == "CHUNB" | is.na(Colecao) | Colecao == "GRC")
nrow(BomJesusTocantins)

# mzusp - mest
Buritis <- ID_total %>%
  filter(Localidade == "Buritis")
unique(Buritis$Colecao)
Buritis <- Buritis %>%
  filter(Colecao == "CHUNB" | is.na(Colecao) | Colecao == "GRC")
nrow(Buritis)

## mzusp - mest
ColinasdoSul <- ID_total %>%
  filter(Localidade == "ColinasdoSul")
unique(ColinasdoSul$Colecao)
ColinasdoSul$FabDocCh2
ColinasdoSul$FabDocCh4AP
ColinasdoSul$FabMestrado
ColinasdoSul <- ColinasdoSul %>%
  filter(Colecao == "CHUNB" | is.na(Colecao) | Colecao == "GRC" | Colecao == "LACV")
nrow(ColinasdoSul)


# Maranhao - nao eh amarali - 5 
Carolina <- ID_total %>%
  filter(Localidade == "Carolina")
unique(Carolina$Colecao)
Carolina$FabDocCh2
Carolina$FabDocCh4AP
Carolina$FabMestrado
Carolina <- Carolina %>%
  filter(Colecao == "CHUNB" | is.na(Colecao) | Colecao == "GRC" | Colecao == "LACV")
nrow(Carolina)


# 42 - sep 20
Caseara <- ID_total %>%
  filter(Localidade == "Caseara")
unique(Caseara$Colecao)
Caseara$FabDocCh2
Caseara$FabDocCh4AP
Caseara$FabMestrado
Caseara$FabDoc3CitB
Caseara <- Caseara %>%
  filter(Colecao == "CHUNB" | is.na(Colecao) | Colecao == "GRC" | Colecao == "LACV")
nrow(Caseara)
Caseara <- Caseara[c(6:10,12:28),]
nrow(Caseara)


# 10 - tem de tudo e tb utilizados no doc e mest msm bichos - 3
Cavalcante <- ID_total %>%
  filter(Localidade == "Cavalcante")
unique(Cavalcante$Colecao)
Cavalcante$FabDocCh2
Cavalcante$FabDocCh4AP
Cavalcante$FabMestrado
Cavalcante <- Cavalcante %>%
  filter(Colecao == "CHUNB" | is.na(Colecao) | Colecao == "GRC" | Colecao == "LACV")
nrow(Cavalcante)


# mzusp 1 ind
Cocalinho <- ID_total %>%
  filter(Localidade == "Cocalinho")
unique(Cocalinho$Colecao)
Cocalinho$FabDocCh2
Cocalinho$FabDocCh4AP
Cocalinho$FabMestrado
Cocalinho <- Cocalinho %>%
  filter(Colecao == "CHUNB" | is.na(Colecao) | Colecao == "GRC" | Colecao == "LACV")
nrow(Cocalinho)

# 3 chunb - goias
Cocalzinho <- ID_total %>%
  filter(Localidade == "Cocalzinho")
unique(Cocalzinho$Colecao)
Cocalzinho$FabDocCh2
Cocalzinho$FabDocCh4AP
Cocalzinho$FabMestrado
Cocalzinho <- Cocalzinho %>%
  filter(Colecao == "CHUNB" | is.na(Colecao) | Colecao == "GRC" | Colecao == "LACV")
nrow(Cocalzinho)


# FAb cit b 2 - só 
CocalzinhodeGoias <- ID_total %>%
  filter(Localidade == "CocalzinhodeGoias")
unique(CocalzinhodeGoias$Colecao)
CocalzinhodeGoias$FabDocCh2
CocalzinhodeGoias$FabDocCh4AP
CocalzinhodeGoias$FabMestrado
CocalzinhodeGoias <- CocalzinhodeGoias %>%
  filter(Colecao == "CHUNB" | is.na(Colecao) | Colecao == "GRC" | Colecao == "LACV")
nrow(CocalzinhodeGoias)


# 7 ind chunb
ColinasdoSul <- ID_total %>%
  filter(Localidade == "ColinasdoSul")
unique(ColinasdoSul$Colecao)
ColinasdoSul$FabDocCh2
ColinasdoSul$FabDocCh4AP
ColinasdoSul$FabMestrado
ColinasdoSul <- ColinasdoSul %>%
  filter(Colecao == "CHUNB" | is.na(Colecao) | Colecao == "GRC" | Colecao == "LACV")
nrow(ColinasdoSul)


# so 1
Combinado <- ID_total %>%
  filter(Localidade == "Combinado")
unique(Combinado$Colecao)
Combinado$FabDocCh2
Combinado$FabDocCh4AP
Combinado$FabMestrado
Combinado <- Combinado %>%
  filter(Colecao == "CHUNB" | is.na(Colecao) | Colecao == "GRC" | Colecao == "LACV")
nrow(Combinado)

# 1
Coribe <- ID_total %>%
  filter(Localidade == "Coribe")
unique(Coribe$Colecao)
Coribe$FabDocCh2
Coribe$FabDocCh4AP
Coribe$FabMestrado
Coribe <- Coribe %>%
  filter(Colecao == "CHUNB" | is.na(Colecao) | Colecao == "GRC" | Colecao == "LACV")
nrow(Coribe)

# 1
Dianopolis <- ID_total %>%
  filter(Localidade == "Dianopolis")
unique(Dianopolis$Colecao)
Dianopolis$FabDocCh2
Dianopolis$FabDocCh4AP
Dianopolis$FabMestrado
Dianopolis <- Dianopolis %>%
  filter(Colecao == "CHUNB" | is.na(Colecao) | Colecao == "GRC" | Colecao == "LACV")
nrow(Dianopolis)

# 1 chunb so, muitos de estr
Estreito <- ID_total %>%
  filter(Localidade == "Estreito")
unique(Estreito$Colecao)
Estreito$FabDocCh2
Estreito$FabDocCh4AP
Estreito$FabMestrado
Estreito <- Estreito %>%
  filter(Colecao == "CHUNB" | is.na(Colecao) | Colecao == "GRC" | Colecao == "LACV")
nrow(Estreito)

# 0 - todos CGERV
EstreitoCGERV <- ID_total %>%
  filter(Localidade == "EstreitoCGERV")
unique(EstreitoCGERV$Colecao)
EstreitoCGERV$FabDocCh2
EstreitoCGERV$FabDocCh4AP
EstreitoCGERV$FabMestrado
EstreitoCGERV <- EstreitoCGERV %>%
  filter(Colecao == "CHUNB" | is.na(Colecao) | Colecao == "GRC" | Colecao == "LACV")
nrow(EstreitoCGERV)

# 1 gr externo
Exu <- ID_total %>%
  filter(Localidade == "Exu")
unique(Exu$Colecao)
Exu$FabDocCh2
Exu$FabDocCh4AP
Exu$FabMestrado
Exu <- Exu %>%
  filter(Colecao == "CHUNB" | is.na(Colecao) | Colecao == "GRC" | Colecao == "LACV")
nrow(Exu)

# 1 AP 
Figueiropolis <- ID_total %>%
  filter(Localidade == "Figueiropolis")
unique(Figueiropolis$Colecao)
Figueiropolis$FabDocCh2
Figueiropolis$FabDocCh4AP
Figueiropolis$FabMestrado
Figueiropolis <- Figueiropolis %>%
  filter(Colecao == "CHUNB" | is.na(Colecao) | Colecao == "GRC" | Colecao == "LACV")
nrow(Figueiropolis)

# 0 ESTR
Goiatins <- ID_total %>%
  filter(Localidade == "Goiatins")
unique(Goiatins$Colecao)
Goiatins$FabDocCh2
Goiatins$FabDocCh4AP
Goiatins$FabMestrado
Goiatins$FabDoc3CitB
Goiatins <- Goiatins %>%
  filter(Colecao == "CHUNB" | is.na(Colecao) | Colecao == "GRC" | Colecao == "LACV")
nrow(Goiatins)


# TO - so 1 - resto MTR
Guarai <- ID_total %>%
  filter(Localidade == "Guarai")
unique(Guarai$Colecao)
Guarai$FabDocCh2
Guarai$FabDocCh4AP
Guarai$FabMestrado
Guarai <- Guarai %>%
  filter(Colecao == "CHUNB" | is.na(Colecao) | Colecao == "GRC" | Colecao == "LACV")
nrow(Guarai)

# so mzusp 1
Gurupi <- ID_total %>%
  filter(Localidade == "Gurupi")
unique(Gurupi$Colecao)
Gurupi$FabDocCh2
Gurupi$FabDocCh4AP
Gurupi$FabMestrado
Gurupi <- Gurupi %>%
  filter(Colecao == "CHUNB" | is.na(Colecao) | Colecao == "GRC" | Colecao == "LACV")
nrow(Gurupi)

# mest - mzusp
Ipueiras <- ID_total %>%
  filter(Localidade == "Ipueiras")
unique(Ipueiras$Colecao)
Ipueiras$FabDocCh2
Ipueiras$FabDocCh4AP
Ipueiras$FabMestrado
Ipueiras <- Ipueiras %>%
  filter(Colecao == "CHUNB" | is.na(Colecao) | Colecao == "GRC" | Colecao == "LACV")
nrow(Ipueiras)

# so mest - 2 - resto MTJ 
JanuariaPeruacu <- ID_total %>%
  filter(Localidade == "JanuariaPeruacu")
unique(JanuariaPeruacu$Colecao)
JanuariaPeruacu$FabDocCh2
JanuariaPeruacu$FabDocCh4AP
JanuariaPeruacu$FabMestrado
JanuariaPeruacu <- JanuariaPeruacu %>%
  filter(Colecao == "CHUNB" | is.na(Colecao) | Colecao == "GRC" | Colecao == "LACV")
nrow(JanuariaPeruacu)

# cit b 1 so
JuncodoSerido <- ID_total %>%
  filter(Localidade == "JuncodoSerido")
unique(JuncodoSerido$Colecao)
JuncodoSerido$FabDocCh2
JuncodoSerido$FabDocCh4AP
JuncodoSerido$FabMestrado
JuncodoSerido <- JuncodoSerido %>%
  filter(Colecao == "CHUNB" | is.na(Colecao) | Colecao == "GRC" | Colecao == "LACV")
nrow(JuncodoSerido)

# so laj - doc2 e ap
Lajeado <- ID_total %>%
  filter(Localidade == "Lajeado")
unique(Lajeado$Colecao)
Lajeado$FabDocCh2
Lajeado$FabDocCh4AP
Lajeado$FabMestrado
Lajeado <- Lajeado %>%
  filter(Colecao == "CHUNB" | is.na(Colecao) | Colecao == "GRC" | Colecao == "LACV")
nrow(Lajeado)

# so cit - LAJ
LajeadoUHE <- ID_total %>%
  filter(Localidade == "LajeadoUHE")
unique(LajeadoUHE$Colecao)
LajeadoUHE$FabDocCh2
LajeadoUHE$FabDocCh4AP
LajeadoUHE$FabMestrado
LajeadoUHE <- LajeadoUHE %>%
  filter(Colecao == "CHUNB" | is.na(Colecao) | Colecao == "GRC" | Colecao == "LACV")
nrow(LajeadoUHE)


# 1 so - resto muitos mzusp
LajeadoUHELuis <- ID_total %>%
  filter(Localidade == "LajeadoUHELuis")
unique(LajeadoUHELuis$Colecao)
LajeadoUHELuis$FabDocCh2
LajeadoUHELuis$FabDocCh4AP
LajeadoUHELuis$FabMestrado
LajeadoUHELuis <- LajeadoUHELuis %>%
  filter(Colecao == "CHUNB" | is.na(Colecao) | Colecao == "GRC" | Colecao == "LACV")
nrow(LajeadoUHELuis)

# 4 chunb - n tem folidose
Mamanguape <- ID_total %>%
  filter(Localidade == "Mamanguape")
unique(Mamanguape$Colecao)
Mamanguape$FabDocCh2
Mamanguape$FabDocCh4AP
Mamanguape$FabMestrado
Mamanguape <- Mamanguape %>%
  filter(Colecao == "CHUNB" | is.na(Colecao) | Colecao == "GRC" | Colecao == "LACV")
nrow(Mamanguape)

# 4 chunb - grupo ext
Manga <- ID_total %>%
  filter(Localidade == "Manga")
unique(Manga$Colecao)
Manga$FabDocCh2
Manga$FabDocCh4AP
Manga$FabMestrado
Manga <- Manga %>%
  filter(Colecao == "CHUNB" | is.na(Colecao) | Colecao == "GRC" | Colecao == "LACV")
nrow(Manga)

# tem muitos - >100 mest e outros - peguei 5 a mais
Mateiros <- ID_total %>%
  filter(Localidade == "Mateiros")
unique(Mateiros$Colecao)
Mateiros$FabDocCh2
Mateiros$FabDocCh4AP
Mateiros$FabMestrado
Mateiros <- Mateiros %>%
  filter(Colecao == "CHUNB" | is.na(Colecao) | Colecao == "GRC" | Colecao == "LACV")
nrow(Mateiros)
Mateiros <- Mateiros[c(1:4, 5, 16:31),]


# 3 chunb
MatiasCardoso <- ID_total %>%
  filter(Localidade == "MatiasCardoso")
unique(MatiasCardoso$Colecao)
MatiasCardoso$FabDocCh2
MatiasCardoso$FabDocCh4AP
MatiasCardoso$FabMestrado
MatiasCardoso <- MatiasCardoso %>%
  filter(Colecao == "CHUNB" | is.na(Colecao) | Colecao == "GRC" | Colecao == "LACV")
nrow(MatiasCardoso)

# 1
Milagres <- ID_total %>%
  filter(Localidade == "Milagres")
unique(Milagres$Colecao)
Milagres$FabDocCh2
Milagres$FabDocCh4AP
Milagres$FabMestrado
Milagres <- Milagres %>%
  filter(Colecao == "CHUNB" | is.na(Colecao) | Colecao == "GRC" | Colecao == "LACV")
nrow(Milagres)

# tem muitos - peguei 20
Minacu <- ID_total %>%
  filter(Localidade == "Minacu")
unique(Minacu$Colecao)
Minacu$FabDocCh2
Minacu$FabDocCh4AP
Minacu$FabMestrado
Minacu <- Minacu %>%
  filter(Colecao == "CHUNB" | is.na(Colecao) | Colecao == "GRC" | Colecao == "LACV")
nrow(Minacu)
Minacu <- Minacu[c(2,4,5,9:29),]

# 1 mest
Mineiros <- ID_total %>%
  filter(Localidade == "Mineiros")
unique(Mineiros$Colecao)
Mineiros$FabDocCh2
Mineiros$FabDocCh4AP
Mineiros$FabMestrado
Mineiros <- Mineiros %>%
  filter(Colecao == "CHUNB" | is.na(Colecao) | Colecao == "GRC" | Colecao == "LACV")
nrow(Mineiros)

# Cit b - 10 - folidose abaixo - so montealegre
MonteAlegredeGoias <- ID_total %>%
  filter(Localidade == "MonteAlegredeGoias" | Localidade == "MonteAlegre")
unique(MonteAlegredeGoias$Colecao)
MonteAlegredeGoias$FabDocCh2
MonteAlegredeGoias$FabDocCh4AP
MonteAlegredeGoias$FabMestrado
MonteAlegredeGoias <- MonteAlegredeGoias %>%
  filter(Colecao == "CHUNB" | is.na(Colecao) | Colecao == "GRC" | Colecao == "LACV")
nrow(MonteAlegredeGoias)
MonteAlegredeGoias <- MonteAlegredeGoias[c(10, 11:30),]



# muitos peguei 10 para juntar com o de cima
MonteAlegre <- ID_total %>%
  filter(Localidade == "MonteAlegre")
unique(MonteAlegre$Colecao)
MonteAlegre$FabDocCh2
MonteAlegre$FabDocCh4AP
MonteAlegre$FabMestrado
MonteAlegre <- MonteAlegre %>%
  filter(Colecao == "CHUNB" | is.na(Colecao) | Colecao == "GRC" | Colecao == "LACV")
nrow(MonteAlegre)
MonteAlegre <- MonteAlegre[c(1:10),]

# 1 AP
Natividade <- ID_total %>%
  filter(Localidade == "Natividade")
unique(Natividade$Colecao)
Natividade$FabDocCh2
Natividade$FabDocCh4AP
Natividade$FabMestrado
Natividade <- Natividade %>%
  filter(Colecao == "CHUNB" | is.na(Colecao) | Colecao == "GRC" | Colecao == "LACV")
nrow(Natividade)

# So LG e MZusp - 
Niquelandia <- ID_total %>%
  filter(Localidade == "Niquelandia")
unique(Niquelandia$Colecao)
Niquelandia$FabDocCh2
Niquelandia$FabDocCh4AP
Niquelandia$FabMestrado
Niquelandia <- Niquelandia %>%
  filter(Colecao == "CHUNB" | is.na(Colecao) | Colecao == "GRC" | Colecao == "LACV")
nrow(Niquelandia)

# todos os 25
NovaXavantina <- ID_total %>%
  filter(Localidade == "NovaXavantina")
unique(NovaXavantina$Colecao)
NovaXavantina$FabDocCh2
NovaXavantina$FabDocCh4AP
NovaXavantina$FabMestrado
NovaXavantina <- NovaXavantina %>%
  filter(Colecao == "CHUNB" | is.na(Colecao) | Colecao == "GRC" | Colecao == "LACV")
nrow(NovaXavantina)
NovaXavantina <- NovaXavantina[c(6:25),]


# 1 mest
NovaRoma <- ID_total %>%
  filter(Localidade == "NovaRoma")
unique(NovaRoma$Colecao)
NovaRoma$FabDocCh2
NovaRoma$FabDocCh4AP
NovaRoma$FabMestrado
NovaRoma <- NovaRoma %>%
  filter(Colecao == "CHUNB" | is.na(Colecao) | Colecao == "GRC" | Colecao == "LACV")
nrow(NovaRoma)


# 20 tudo
Palmas <- ID_total %>%
  filter(Localidade == "Palmas")
unique(Palmas$Colecao)
Palmas$FabDocCh2
Palmas$FabDocCh4AP
Palmas$FabMestrado
Palmas <- Palmas %>%
  filter(Colecao == "CHUNB" | is.na(Colecao) | Colecao == "GRC" | Colecao == "LACV")
nrow(Palmas)
Palmas <- Palmas[c(2, 4:23),]

#3 - fab cit b 
PalmasEast <- ID_total %>%
  filter(Localidade == "PalmasEast")
unique(PalmasEast$Colecao)
PalmasEast$FabDocCh2
PalmasEast$FabDocCh4AP
PalmasEast$FabMestrado
PalmasEast <- PalmasEast %>%
  filter(Colecao == "CHUNB" | is.na(Colecao) | Colecao == "GRC" | Colecao == "LACV")
nrow(PalmasEast)

# cit b - 7
PalmasWest <- ID_total %>%
  filter(Localidade == "PalmasWest")
unique(PalmasWest$Colecao)
PalmasWest$FabDocCh2
PalmasWest$FabDocCh4AP
PalmasWest$FabMestrado
PalmasWest <- PalmasWest %>%
  filter(Colecao == "CHUNB" | is.na(Colecao) | Colecao == "GRC" | Colecao == "LACV")
nrow(PalmasWest)

# Parana 20 tudo - muito mais >100
Parana <- ID_total %>%
  filter(Localidade == "Parana")
unique(Parana$Colecao)
Parana$FabDocCh2
Parana$FabDocCh4AP
Parana$FabMestrado
Parana <- Parana %>%
  filter(Colecao == "CHUNB" | is.na(Colecao) | Colecao == "GRC" | Colecao == "LACV")
nrow(Parana)
Parana <- Parana[c(2,4,6:23),]

# 2 mestrado - mata
PedroAfonso <- ID_total %>%
  filter(Localidade == "PedroAfonso")
unique(PedroAfonso$Colecao)
PedroAfonso$FabDocCh2
PedroAfonso$FabDocCh4AP
PedroAfonso$FabMestrado
PedroAfonso <- PedroAfonso %>%
  filter(Colecao == "CHUNB" | is.na(Colecao) | Colecao == "GRC" | Colecao == "LACV")
nrow(PedroAfonso)

# 10 - tem mais no MZusp
Peixe <- ID_total %>%
  filter(Localidade == "Peixe")
unique(Peixe$Colecao)
Peixe$FabDocCh2
Peixe$FabDocCh4AP
Peixe$FabMestrado
Peixe <- Peixe %>%
  filter(Colecao == "CHUNB" | is.na(Colecao) | Colecao == "GRC" | Colecao == "LACV")
nrow(Peixe)

# 20 varios - tem mais
Pirenopolis <- ID_total %>%
  filter(Localidade == "Pirenopolis")
unique(Pirenopolis$Colecao)
Pirenopolis$FabDocCh2
Pirenopolis$FabDocCh4AP
Pirenopolis$FabMestrado
Pirenopolis <- Pirenopolis %>%
  filter(Colecao == "CHUNB" | is.na(Colecao) | Colecao == "GRC" | Colecao == "LACV")
nrow(Pirenopolis)
Pirenopolis <- Pirenopolis[c(4, 6:25),]

# mzusp so mest 
Pium <- ID_total %>%
  filter(Localidade == "Pium")
unique(Pium$Colecao)
Pium$FabDocCh2
Pium$FabDocCh4AP
Pium$FabMestrado
Pium <- Pium %>%
  filter(Colecao == "CHUNB" | is.na(Colecao) | Colecao == "GRC" | Colecao == "LACV")
nrow(Pium)

# 3 so mest
PortoAlegreTocantins <- ID_total %>%
  filter(Localidade == "PortoAlegreTocantins")
unique(PortoAlegreTocantins$Colecao)
PortoAlegreTocantins$FabDocCh2
PortoAlegreTocantins$FabDocCh4AP
PortoAlegreTocantins$FabMestrado
PortoAlegreTocantins <- PortoAlegreTocantins %>%
  filter(Colecao == "CHUNB" | is.na(Colecao) | Colecao == "GRC" | Colecao == "LACV")
nrow(PortoAlegreTocantins)

# 1 mest
PortoNacional <- ID_total %>%
  filter(Localidade == "PortoNacional")
unique(PortoNacional$Colecao)
PortoNacional$FabDocCh2
PortoNacional$FabDocCh4AP
PortoNacional$FabMestrado
PortoNacional <- PortoNacional %>%
  filter(Colecao == "CHUNB" | is.na(Colecao) | Colecao == "GRC" | Colecao == "LACV")
nrow(PortoNacional)

# LG
PortoSeguro <- ID_total %>%
  filter(Localidade == "PortoSeguro")
unique(PortoSeguro$Colecao)
PortoSeguro$FabDocCh2
PortoSeguro$FabDocCh4AP
PortoSeguro$FabMestrado
PortoSeguro <- PortoSeguro %>%
  filter(Colecao == "CHUNB" | is.na(Colecao) | Colecao == "GRC" | Colecao == "LACV")
nrow(PortoSeguro)

# mest - so mzusp
Posse <- ID_total %>%
  filter(Localidade == "Posse")
unique(Posse$Colecao)
Posse$FabDocCh2
Posse$FabDocCh4AP
Posse$FabMestrado
Posse <- Posse %>%
  filter(Colecao == "CHUNB" | is.na(Colecao) | Colecao == "GRC" | Colecao == "LACV")
nrow(Posse)

## darwini cit b e kif - 3, nao tem folidose
PresidenteKennedy <- ID_total %>%
  filter(Localidade == "PresidenteKennedy")
unique(PresidenteKennedy$Colecao)
PresidenteKennedy$FabDocCh2
PresidenteKennedy$FabDocCh4AP
PresidenteKennedy$FabMestrado
PresidenteKennedy <- PresidenteKennedy %>%
  filter(Colecao == "CHUNB" | is.na(Colecao) | Colecao == "GRC" | Colecao == "LACV")
nrow(PresidenteKennedy)

# 1 que talvez tenha no guarino mas n sei - usado no mest
PresidenteKennedydarwini <- ID_total %>%
  filter(Localidade == "PresidenteKennedydarwini")
unique(PresidenteKennedydarwini$Colecao)
PresidenteKennedydarwini$FabDocCh2
PresidenteKennedydarwini$FabDocCh4AP
PresidenteKennedydarwini$FabMestrado
PresidenteKennedydarwini <- PresidenteKennedydarwini %>%
  filter(Colecao == "CHUNB" | is.na(Colecao) | Colecao == "GRC" | Colecao == "LACV")
nrow(PresidenteKennedydarwini)

# mzusp mest
RiodasMortes <- ID_total %>%
  filter(Localidade == "RiodasMortes")
unique(RiodasMortes$Colecao)
RiodasMortes$FabDocCh2
RiodasMortes$FabDocCh4AP
RiodasMortes$FabMestrado
RiodasMortes <- RiodasMortes %>%
  filter(Colecao == "CHUNB" | is.na(Colecao) | Colecao == "GRC" | Colecao == "LACV")
nrow(RiodasMortes)

# 1 mest mzusp
RioVerde <- ID_total %>%
  filter(Localidade == "RioVerde")
unique(RioVerde$Colecao)
RioVerde$FabDocCh2
RioVerde$FabDocCh4AP
RioVerde$FabMestrado
RioVerde <- RioVerde %>%
  filter(Colecao == "CHUNB" | is.na(Colecao) | Colecao == "GRC" | Colecao == "LACV")
nrow(RioVerde)


# 1 cit b
SantanadoCariri <- ID_total %>%
  filter(Localidade == "SantanadoCariri")
unique(SantanadoCariri$Colecao)
SantanadoCariri$FabDocCh2
SantanadoCariri$FabDocCh4AP
SantanadoCariri$FabMestrado
SantanadoCariri <- SantanadoCariri %>%
  filter(Colecao == "CHUNB" | is.na(Colecao) | Colecao == "GRC" | Colecao == "LACV")
nrow(SantanadoCariri)

# tem muitos >100 - 21 pegados
SaoDomingos <- ID_total %>%
  filter(Localidade == "SaoDomingos")
unique(SaoDomingos$Colecao)
SaoDomingos$FabDocCh2
SaoDomingos$FabDocCh4AP
SaoDomingos$FabMestrado
SaoDomingos <- SaoDomingos %>%
  filter(Colecao == "CHUNB" | is.na(Colecao) | Colecao == "GRC" | Colecao == "LACV")
nrow(SaoDomingos)
SaoDomingos <- SaoDomingos[c(7,8,11,15:35,123),]


# so mtr - mas immportante
SaoSalvador <- ID_total %>%
  filter(Localidade == "SaoSalvador")
unique(SaoSalvador$Colecao)
SaoSalvador$FabDocCh2
SaoSalvador$FabDocCh4AP
SaoSalvador$FabMestrado
SaoSalvador <- SaoSalvador %>%
  filter(Colecao == "CHUNB" | is.na(Colecao) | Colecao == "GRC" | Colecao == "LACV")
nrow(SaoSalvador)

# 1 so e do mest ainda
SaoDesideriogeckoides <- ID_total %>%
  filter(Localidade == "SaoDesideriogeckoides")
unique(SaoDesideriogeckoides$Colecao)
SaoDesideriogeckoides$FabDocCh2
SaoDesideriogeckoides$FabDocCh4AP
SaoDesideriogeckoides$FabMestrado
SaoDesideriogeckoides <- SaoDesideriogeckoides %>%
  filter(Colecao == "CHUNB" | is.na(Colecao) | Colecao == "GRC" | Colecao == "LACV")
nrow(SaoDesideriogeckoides)

# so mzusp e so mest - mas varios
SaoSalvadorTocantins <- ID_total %>%
  filter(Localidade == "SaoSalvadorTocantins")
unique(SaoSalvadorTocantins$Colecao)
SaoSalvadorTocantins$FabDocCh2
SaoSalvadorTocantins$FabDocCh4AP
SaoSalvadorTocantins$FabMestrado
SaoSalvadorTocantins <- SaoSalvadorTocantins %>%
  filter(Colecao == "CHUNB" | is.na(Colecao) | Colecao == "GRC" | Colecao == "LACV")
nrow(SaoSalvadorTocantins)

# so mzusp - mest
SerraNegra <- ID_total %>%
  filter(Localidade == "SerraNegra")
unique(SerraNegra$Colecao)
SerraNegra$FabDocCh2
SerraNegra$FabDocCh4AP
SerraNegra$FabMestrado
SerraNegra <- SerraNegra %>%
  filter(Colecao == "CHUNB" | is.na(Colecao) | Colecao == "GRC" | Colecao == "LACV")
nrow(SerraNegra)

# 2 mest
TeresinadeGoias <- ID_total %>%
  filter(Localidade == "TeresinadeGoias")
unique(TeresinadeGoias$Colecao)
TeresinadeGoias$FabDocCh2
TeresinadeGoias$FabDocCh4AP
TeresinadeGoias$FabMestrado
TeresinadeGoias <- TeresinadeGoias %>%
  filter(Colecao == "CHUNB" | is.na(Colecao) | Colecao == "GRC" | Colecao == "LACV")
nrow(TeresinadeGoias)

# 3 doc
Tiangua <- ID_total %>%
  filter(Localidade == "Tiangua")
unique(Tiangua$Colecao)
Tiangua$FabDocCh2
Tiangua$FabDocCh4AP
Tiangua$FabMestrado
Tiangua <- Tiangua %>%
  filter(Colecao == "CHUNB" | is.na(Colecao) | Colecao == "GRC" | Colecao == "LACV")
nrow(Tiangua)

# col MD - so Doc 2
Una <- ID_total %>%
  filter(Localidade == "Una")
unique(Una$Colecao)
Una$FabDocCh2
Una$FabDocCh4AP
Una$FabMestrado
Una <- Una %>%
  filter(Colecao == "CHUNB" | is.na(Colecao) | Colecao == "GRC" | Colecao == "LACV")
nrow(Una)


pedir_col <- rbind(SaoDomingos, Minacu, ColinasdoSul, AltoPairaiso,
      Caseara, NovaXavantina, Peixe, Cavalcante,
      MonteAlegredeGoias, Parana,
      Almas, Mateiros, Carolina, Manga, PresidenteKennedy, Pirenopolis,
      Palmas, MatiasCardoso)
nrow(pedir_col)

write.csv(pedir_col, "dados/alterados/pedir_col.csv")

## Lixo

## Script 3 - megazordizacao de dados

teste <- unique(fabricius_folidose_cladosnovos_imput$Localidade)
write.csv(teste, file = "nomes_local.csv")

Pasta1a <- read_delim("apagar/Pasta1a.csv", 
                      delim = ";",
                      escape_double = FALSE,
                      trim_ws = TRUE,
                      col_names = FALSE)
opa <- Pasta1a[-c(47:49),]
colnames(opa) <- c("Localidade", "Colecao")

fabricius_folidose_cladosnovos_imput <- full_join(opa, fabricius_folidose_cladosnovos_imput)


fab <- fabricius_folidose_cladosnovos_imput %>%
  mutate(Colecao = ifelse(Tombo == c(52006:52008,
                                     11199:11204,
                                     11173:11189,
                                     11502:11515,
                                     12437, 12439,
                                     12440, 14555,
                                     24654, 25089,
                                     25090, 52607:52610,
                                     52612, 86:96, 292:294,
                                     296:314, 372:392, 394:399,
                                     893, 9497, 11080, 34681:34684,
                                     39806, 39851, 39924, 47754),
                          "CHUNB", Colecao))

fab1 <- fab %>%
  mutate(Colecao = ifelse(lag(Colecao) == "ambos", "MZUSP", Colecao))


write.csv(fab, file = "dados/alterados/FabMestTudo.csv")
?export.csv
fabMest_feio <- read_delim("dados/alterados/fabMest-feio.csv", 
                           delim = ";",
                           escape_double = FALSE,
                           trim_ws = TRUE, 
                           col_names = FALSE)
View(fabMest_feio)

colecao <- fabMest_feio$X3
#vector com nomes das colecoes

fabMest_feio <- fabMest_feio[,-c(1,2,3)]
#só os numeros de tombo

pivot_longer(fabMest_feio, cols = -c(1,2))




fab <- fabMest_feio
fab

c <- list()

a <- c()

for (i in 1:length(fab)){
  a <-  unlist(fab[i,])
  c <- append(c, list(a))
}



c <- lapply(c, function(x) x[complete.cases(x)])

lista <- c

# Convertendo os elementos de intervalo em vetores numéricos

lista <- sapply(lista, function(x){
  if(grepl(":", x)){
    as.numeric(unlist(strsplit(x,":")))
  } else {
    as.numeric(x)
  }
})

?grepl

lista_convertida <- lapply(lista, function(x) {
  lapply(x, function(y) {
    eval(parse(text = y))
  })
})

# Descompactando a lista de vetores em um único vetor
vetor_final <- unlist(lista_convertida)

# Exibindo o resultado
vetor_final


# leitura da tabela em um data.frame
tabela <- read.table(text = "13	25	53	75	99	102
42411	42426																																														
400:417	12434:12436	17521																																													
37296																																															
52006:53008																																														
45302:45336																																															
53290:53294																																															
36309	38645	38646																																													
44698	44704	44708	48337	50402																																											
866																																															
33562																																															
50831																																															
14148:	14150	28204:28310	40964	40965	40968	40969	40973	40974	40976	40978:40980	40984:40986	40988	40989	40992	40998	41000	41002	41004:41006	41011	41013	41014	41020	41021	41027	41028	41030	41031	41033	41035	41037:41039	41042	41045	41046	41049:41051	41055	41056	41060:41062	41064	41067:41071	41073	41075:41077	41079:41082	41086:41089	41091	42351	42355	42356
3009:3076	3078:3133	8001:8030	8032:8036	8040:8059	9498	53160																																									
864																																															
53051:53085	69370																																														
55880:55890																																															
11173:11189	11199:11204	11502:11515	12437	12439	12440	14555	24654	25089	25090																																						
33536	33537	33539:33542	33544	33545	33547:33551	33553	33557:33559	33561	33563	33564	33567:33570	33572	33573	33575:33577	33579:33582	33585:37138																															
50829	50830																																														
52607:52610	52612																																														
86:96	292:294	296:314	372:392	394:399	893	9497	11080	34681:34684	39806	39851	39924																																				
38911:38913																																															
47754																																															
33538	33543	33546	33552	33554:33556	33560	33565	33566	33571	33574	33578	35196:35233	37139:37163")


# Lendo os dados a partir do arquivo txt
dados <- read.table("apagar/gpc.txt", header = FALSE, sep = "\t", na.strings = "", stringsAsFactors = FALSE)

# Transformando as colunas que contêm ":" em sequências de números
for(i in 1:ncol(dados)){
  if(grepl(":", dados[,i])){
    valores <- unlist(strsplit(dados[,i], ":"))
    sequencia <- seq(from = as.numeric(valores[1]), to = as.numeric(valores[2]))
    dados[,i] <- NA
    dados[,i][seq_along(sequencia)] <- sequencia
  }
}

# Convertendo todas as colunas para numérico
dados[] <- lapply(dados, as.numeric)

# Verificando o resultado
str(dados)


# transforma cada linha em um vetor de caracteres
lines <- as.character(readLines("apagar/gpc.txt", encoding = "UTF-8"))

dados <- read.table("apagar/gpc.txt", header = FALSE, sep = "\t", na.strings = "", stringsAsFactors = FALSE, fill = TRUE)


# percorre todas as linhas
for (i in seq_along(lines)) {
  # divide a linha pelos espaços em branco e remove os vazios
  cells <- trimws(strsplit(lines[i], " ")[[1]])
  # percorre todas as células da linha
  for (j in seq_along(cells)) {
    # se a célula contém ":", cria a sequência de números
    if (grepl(":", cells[j])) {
      # divide a célula pelos ":" e remove os vazios
      parts <- trimws(strsplit(cells[j], ":")[[1]])
      # cria a sequência de números
      numbers <- seq(as.numeric(parts[1]), as.numeric(parts[2]))
      # converte os números para caracteres
      numbers <- as.character(numbers)
      # substitui a célula original pela sequência de números
      cells[j] <- paste(numbers, collapse = " ")
    }
  }
  # une as células de volta em uma linha e imprime
  cat(paste(cells, collapse = "\t"), "\n")
}


# Transformando as colunas que contêm ":" em sequências de números
for(i in 1:ncol(dados)){
  if(grepl(":", dados[,i])){
    valores <- unlist(strsplit(dados[,i], ":"))
    sequencia <- seq(from = as.numeric(valores[1]), to = as.numeric(valores[2]))
    dados[,i] <- NA
    dados[,i][seq_along(sequencia)] <- sequencia
  }
}

# Convertendo todas as colunas para numérico
dados[] <- lapply(dados, as.numeric)

# Verificando o resultado
str(dados)







tabela <- read.table("apagar/gpc.txt", header = FALSE, sep = "\t", na.strings = "", stringsAsFactors = FALSE, fill = TRUE)



# Convertendo os dados para um vetor
dados <- unlist(tabela)

# Função que separa os números de acordo com a regra definida
separar_numeros <- function(dado) {
  # Verifica se o dado contém o caractere ":"
  if (":" %in% dado) {
    # Separa os números
    numeros <- as.numeric(strsplit(dado, ":")[[1]])
    # Cria uma sequência de números
    seq(numeros[1], numeros[2])
  } else {
    # Retorna o número como um vetor
    as.numeric(dado)
  }
}

# Aplica a função para separar os números em cada elemento do vetor
dados_separados <- lapply(dados, separar_numeros)

# Converte o resultado para uma matriz com o mesmo número de colunas da tabela original
dados_matriz <- matrix(unlist(dados_separados), ncol = ncol(tabela))

# Exibe a matriz resultante
dados_matriz

View(dados_matriz)


# ler arquivo
dados <- read.table("apagar/gpc.txt", sep = "\t", fill = TRUE)

# criar uma função para converter números antes de ':' e depois de ':' em sequência de números
converte_seq <- function(nums) {
  # verifica se há ':' na string
  if (":" %in% nums) {
    # separa os números antes e depois do ':'
    nums_separados <- strsplit(nums, ":", fixed = TRUE)[[1]]
    # converte os números em inteiros
    nums_int <- as.integer(nums_separados)
    # verifica se os números foram convertidos corretamente
    if (any(is.na(nums_int))) {
      # caso um dos números não possa ser convertido, retorna NA
      return(rep(NA, length(nums)))
    } else {
      # caso contrário, cria a sequência de números
      return(seq(nums_int[1], nums_int[2]))
    }
  } else {
    # se não houver ':', retorna o número original
    return(as.integer(nums))
  }
}

":" %in% dados

?grep
# search for matches to argument pattern 

if (grepl(":", dados[,1])){
  nums_separados <- strsplit(dados[,1], ":", fixed = TRUE)[[1]]
}






# aplica a função a cada coluna da tabela
dados_convertidos <- apply(dados, 2, converte_seq)

# remove as colunas que contêm apenas valores NA
dados_convertidos <- dados_convertidos[, !apply(is.na(dados_convertidos), 2, all)]

# cria um data frame com as colunas convertidas
dados_finais <- data.frame(dados_convertidos)

# mostra o resultado
dados_finais


#### rascunho extrair dados NENHUM FUNCIONOU

## SITE TABULA - SALVOS FIREFOX - RAINDROP

install.packages("tcltk2") 

PDE_install_Xpdftools4.02()    # Download and install the Xpdf command line tools
PDE_check_Xpdf_install()        # Check if all required XPDF tools are installed correctly
install.packages("PDE", dependencies = TRUE)

library(tcltk2)
library(PDE)

PDE_analyzer_i()

install.packages("pdftools")
library(pdftools)
library(tidyverse)

PDF <- pdf_text("tentativa_extrair_tab_pdf/Thesis_Revised-192-194.pdf") %>%
  readr::read_lines() #open the PDF inside your project folder

View(PDF)

FabIDCh2 <- PDF[-c(1:26)]

Fa01 <- FabIDCh2[1:39]



all_stat_lines <- Fa01[-c(38,39)] %>%
  str_squish() %>%
  strsplit(split = " ")# remove empty spaces

var_lines <- c("Cytb Haplotype", "Species", "ID", "Locality", "Brazilian State", "Genbank accession number (cytb/ KIF24)") # create your variable names

var_lines

df <- plyr::ldply(all_stat_lines) #create a data frame
head(df)


cat(PDF[2])


pdf_toc("tentativa_extrair_tab_pdf/Thesis_Revised-192-194.pdf") %>% str(max.level = 3)


###########################################################################
## https://www.charlesbordet.com/en/extract-pdf/#use-pdftoolspdf_text
# NAO

text2 <- strsplit(PDF, "\n")

install.packages("tm")
library("tm")

read <- readPDF(control = list(text = "-layout"))

document <- Corpus(URISource("tentativa_extrair_tab_pdf/Thesis_Revised-192-194.pdf"), readerControl = list(reader = read))
doc <- content(document[[1]])
head(doc)

page_breaks <- grep("\\f", doc)

doc[page_breaks[1]]


# Remove header of the first page
president_row <- grep("^President:", doc)[1]
doc <- doc[(president_row + 1):length(doc)]

?grep

# Remove footer on first page
footer_row_1 <- grep("This record contains the text of speeches ", doc)[1]
footer_row_2 <- grep("\\f", doc)[1] - 1
doc <- doc[- c(footer_row_1:footer_row_2)]

# Remove headers on other pages
header_rows <- grep("^\\f", doc) # Remember: \f are for page breaks
doc[header_rows] <- "page" # I put a marker here that will be useful later
doc <- doc[- (header_rows - 1)]


#################################################

## TENTATIVA 3
## PDF TOOLS DE NOVO
## https://www.youtube.com/watch?v=bJH-S2iaxNo

library("pdftools")
library("tidyverse")
library("ggplot2")

PDF
?map()

raw_text <- map("tentativa_extrair_tab_pdf/Thesis_Revised-192-194.pdf", pdf_text)
clean_table1 <- function(raw) {
  raw <- map(raw, ~ str_split(.x, "\\n") %>% unlist())
  raw <- reduce(raw, c)
  
  #specify the start and end of the table
  table_start <- stringr::str_which(tolower(raw), "MTR14578*")
  table_end <- stringr::str_which(tolower(raw), "58")
  table_end <- table_end[min(which(table_end > table_start))]
  
  #build the table and remove special characters
  table <- raw[(table_start):(table_end)]
  table <- str_replace_all(table, "\\s{2,}", "|")
  text_con <- textConnection(table)
  data_table <- read.csv(text_con, sep = "|")
}

results <- map_df(raw_text, clean_table1) 

clean_table1(raw_text)




# download pdf and load file 
url <- c("tentativa_extrair_tab_pdf/Thesis_Revised-192-194.pdf")
raw_text <- map(url, pdf_text)

#function to scrape data and clean
clean_table1 <- function(raw) {
  
  # Split the single pages
  raw <- map(raw, ~ str_split(.x, "\\n") %>% unlist())
  # Concatenate the split pages
  raw <- reduce(raw, c)
  
  # specify the start and end of the table data
  table_start <- stringr::str_which(tolower(raw), "MTR14578*")
  table_end <- stringr::str_which(tolower(raw), "MRT6435")
  table_end <- table_end[min(which(table_end > table_start))]
  
  #Build the table  and remove special characters
  table <- raw[(table_start):(table_end)]
  table <- str_replace_all(table, "\\s{2,}", "|")
  text_con <- textConnection(table)
  data_table <- read.csv(text_con, sep = "|")
  
  
}
results <- map_df(raw_text, clean_table1) 
head(results)
TestDF<-data.frame(results)
TestDF <- TestDF[-c(1)]

#Create a list of column names 
colnames(data_table) <- c("x","Alcohol Use Pattern", "Males","Females","Total")
data_table
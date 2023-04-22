# Bibliotecas
library(tidyverse)
library(Hmisc)
library(data.table)

# Limpa workspace 
rm(list=ls())

# Criando tabelas
Checkouts_By_Title_Data_Lens_2017 <- read.csv("Checkouts_By_Title_Data_Lens_2017.csv")
Checkouts_By_Title_Data_Lens_2016 <- read.csv("Checkouts_By_Title_Data_Lens_2016.csv")
Integrated_Library_System__ILS__Data_Dictionary <- read.csv("Integrated_Library_System__ILS__Data_Dictionary.csv")
Library_Collection_Inventory <- read.csv("Library_Collection_Inventory.csv")

# Entendendo variáveis:
describe(Integrated_Library_System__ILS__Data_Dictionary) # Dicionário, informações sobre os itens
describe(Checkouts_By_Title_Data_Lens_2017) # Checkouts
describe(Library_Collection_Inventory) # Inventário: 2.687.149 registros

# Tratamento dos dados removendo registros duplicados do inventário - critério: última data de report de localização:

# Identificando registros duplicados:
Inventory_Duplicates <- Library_Collection_Inventory %>% 
  add_count(BibNum) %>%
  filter(n > 1)
  
# Identificando número de registros distintos, considerando a última ReportDate:
Inventory_Distinct <- Library_Collection_Inventory %>% 
  arrange(desc(ReportDate)) %>% 
  distinct(BibNum, .keep_all = TRUE)

# Restam 584.391 registros únicos (BibNum)

# Incluir última contagem dos itens de acordo com a ReportDate mais recente:
# Foram removidas colunas de localização/coleção que não agregam informação para a tabela distinta

Collection_Inventory_ItemCount <- Library_Collection_Inventory %>%  
  group_by(BibNum, ReportDate) %>% 
  summarise(Total_ItemCount=sum(ItemCount)) %>% 
  arrange(desc(ReportDate)) %>% 
  distinct(BibNum, .keep_all = TRUE)

Inventory_Distinct_Final <- Inventory_Distinct %>% 
  left_join(Collection_Inventory_ItemCount, by = c("BibNum", "ReportDate")) %>% 
  select(-c(ItemCollection, ItemLocation, FloatingItem, ItemCount))

#' Explicação dos tratamentos realizados:
#' Os dados da coleção (Library_Collection_Iventory) são duplicados para atualizar a localização dos itens na biblioteca.
#' 
#' Foram realizados testes para compreender a dinâmica de ReportDate dos itens e identifiquei que existem itens com último 
#' ReportDate em 09/2017 e que tiveram checkouts registrados em data posterior no mesmo ano, o que sugere que nem todos 
#' os itens do inventário foram reportados na última contagem. Por isso, ao limpar a base de dados, foram considerados como 
#' itens presentes no inventário todos os registros de BibNum, mesmo os que não tiveram ReportDate em 10/2017 
#' (estes foram ao todo 199 registros). Caso se compreenda que devem ser considerados apenas os itens do último Report, basta 
#' filtrar pelo campo ReportDate a data mais recente, que apresentará apenas os itens da contagem mais atual.
#' 
#' Desta forma, para a contagem do total de itens, foram considerados duplicados apenas os itens que possuem todos os campos iguais
#' exceto ReportDate (nesse caso, foram considerados apenas os mais atuais), pois entende-se que podem se tratar de itens de 
#' coleções diferentes, e/ou que estavam em localizações diferentes na mesma data, e que, portanto, devem ser contabilizados
#' como itens diferentes do acervo.

#-------------------------------------------
# How many books were checkedout in 2017?

Checkout_Books_2017 <- Checkouts_By_Title_Data_Lens_2017 %>% 
  left_join(Integrated_Library_System__ILS__Data_Dictionary, by = c("ItemType"="Code")) %>% 
  group_by(Format.Subgroup) %>% 
  filter(Format.Subgroup=="Book")

Total_Books_2017 <- Checkout_Books_2017 %>% 
  summarise(N_Books_2017 = n_distinct(ItemBarcode))

# Resposta: Total de livros com checkouts registrados distinct(ItemBarcode)) = 866.535

#-------------------------------------------
# How many books are there in the library? 

# Identificando apenas códigos de itens tipo "Book":
Code_Books <- Integrated_Library_System__ILS__Data_Dictionary %>% 
  filter(Code.Type=="ItemType"& Format.Subgroup=="Book") %>% 
  select(Code) %>% 
  mutate(Books = 1)

# Calculando total de livros:
Total_Books <- Inventory_Distinct_Final %>% 
  left_join(Code_Books, by = c("ItemType" = "Code")) %>% 
  summarise(N_Books = sum(Total_ItemCount*Books, na.rm = TRUE))

# Resposta: Total de livros = 1.332.993

#--------------------------------------------
# How many titles?

# Número de títulos de livros:
Total_Book_Titles <- Inventory_Distinct_Final %>%
  left_join(Code_Books, by = c("ItemType" = "Code")) %>% 
  filter(Books==1) %>% 
  summarise(N_Titles=n_distinct(BibNum))

# Resposta: Total de Títulos (livros) =	497.692

# Número de títulos (todos itens):
Total_Titles <- Inventory_Distinct_Final %>%
  summarise(N_Titles=n_distinct(BibNum))

# Resposta: Total de Títulos (todos itens) = 584.391

#-------------------------------------------
# What are the top 10 loaned books in 2017?
Top10_Books <- Checkout_Books_2017 %>% 
  group_by(BibNumber) %>% 
  summarise(N_Checkouts=n()) %>% 
  slice_max(N_Checkouts, n=10) %>% 
  left_join(Inventory_Distinct_Final, by = c('BibNumber'='BibNum')) %>% 
  select(BibNumber, N_Checkouts, Title) %>% 
  distinct()

#'Resposta: Top10_Books
#'1 Hillbilly elegy : a memoir of a family and culture in crisis / J.D. Vance.                        
#'2 The Underground Railroad : a novel / Colson Whitehead.                                            
#'3 Commonwealth : a novel / Ann Patchett.                                                            
#'4 Today will be different : a novel / Maria Semple.                                                 
#'5 Moonglow : a novel / Michael Chabon.                                                              
#'6 Born a crime : stories from a South African childhood / Trevor Noah.                              
#'7 Strangers in their own land : anger and mourning on the American right / Arlie Russell Hochschild.
#'8 A man called Ove : a novel / Fredrik Backman ; [translation, Henning Koch].                     
#'9 Lincoln in the bardo : a novel / George Saunders.                                                 
#'10 The wrong side of goodbye : a novel / Michael Connelly. 

#--------------------------------------------
# How many book titles were taken on a loan in 2017?

N_Book_Titles_Chkout_2017 <- Checkout_Books_2017 %>%
  summarise(N_Titles=n_distinct(BibNumber))

# Resposta: 251.156 títulos de livros.

#--------------------------------------------
# What was the most popular genre in 2017?

TopGenre_2017 <- Checkouts_By_Title_Data_Lens_2017 %>% 
  left_join(Integrated_Library_System__ILS__Data_Dictionary, by = c("Collection"="Code")) %>% 
  group_by(Category.Group) %>% 
  summarise(N=n()) %>% 
  arrange(desc(N))

# Resposta: Fiction.

#--------------------------------------------
# Did the number of book loans diminished or increased from 2016 to 2017? By how much?

Checkouts_By_Title_Data_Lens_2016 <- read.csv("Checkouts_By_Title_Data_Lens_2016.csv")

Books_Checkout_2016 <- Checkouts_By_Title_Data_Lens_2016 %>% 
  left_join(Integrated_Library_System__ILS__Data_Dictionary, by = c("ItemType"="Code")) %>% 
  group_by(Format.Subgroup) %>% 
  filter(Format.Subgroup=="Book") %>% 
  summarise(Total=n())

# Total de checkouts de livros registrados em 2016 = 3.809.638 	

Books_Checkout_2017 <- Checkout_Books_2017 %>% 
  summarise(Total=n())

# Total de checkouts de livros registrados em 2017 = 3.085.248

Books_Checkout_2016_2017 <- Books_Checkout_2017$Total/Books_Checkout_2016$Total * 100 - 100

# Resposta: Diminuiu em -19.01467 %



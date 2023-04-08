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
# Os dados da coleção (Library_Collection_Iventory) são duplicados para atualizar a localização dos itens na biblioteca.

Library_Collection_Inventory <- Library_Collection_Inventory %>% 
  arrange(desc(ReportDate)) %>% 
  distinct(BibNum, .keep_all = TRUE)

# Restam 584.391 registros únicos (BibNum)

#-------------------------------------------
# How many books were checkout in 2017?

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
Total_Books <- Library_Collection_Inventory %>% 
  left_join(Code_Books, by = c("ItemType" = "Code")) %>% 
  summarise(N_Books = sum(ItemCount*Books, na.rm = TRUE)) %>% 
  summarise(Total_Books = sum(N_Books))

# Resposta: Total de livros = 620.819

#--------------------------------------------
# How many titles?

# Número de títulos de livros:
Total_Book_Titles <- Library_Collection_Inventory %>%
  left_join(Code_Books, by = c("ItemType" = "Code")) %>% 
  filter(Books==1) %>% 
  summarise(N_Titles=n_distinct(BibNum))

# Resposta: Total de Títulos (livros) =	497.692

# Número de títulos (todos itens):
Total_Titles <- Library_Collection_Inventory %>%
  summarise(N_Titles=n_distinct(BibNum))

# Resposta: Total de Títulos (todos itens) = 584.391

#-------------------------------------------
# What are the top 10 loaned books in 2017?
Top10_Books <- Checkout_Books_2017 %>% 
  group_by(BibNumber) %>% 
  summarise(N_Checkouts=n()) %>% 
  slice_max(N_Checkouts, n=10) %>% 
  left_join(Library_Collection_Inventory, by = c('BibNumber'='BibNum')) %>% 
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



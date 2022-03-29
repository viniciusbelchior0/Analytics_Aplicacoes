library(readxl)
library(tidyverse)
olimpiadas <- read_excel("C:/Users/NOTEBOOK CASA/Desktop/olimpiadas_clean.xlsx", 
                               col_types = c("text", "numeric", "numeric", 
                                             "numeric", "numeric", "text", "text"))
olimpiadas <- olimpiadas %>% mutate(ID_discipline = paste(Discipline, Year, sep = "_"))

eventos <- read_excel("C:/Users/NOTEBOOK CASA/Desktop/olimpiadas_clean.xlsx", 
                               sheet = "events")
eventos <- eventos %>% mutate(ID_discipline = paste(Discipline, Year, sep = "_"))



library(ineq)

#1. Comparando o gini pelo total de medalhas e pelo score por pais
pais <- olimpiadas %>% left_join(select(eventos, ID_discipline, Score),
                                    by = "ID_discipline") %>%
  mutate(Total_corrigido = Total*Score) %>%
  group_by(Country) %>%
  summarise(Modalidades = n_distinct(Discipline),
            Gini_T = ineq(Total, type = "Gini"),
            Gini_S = ineq(Total_corrigido, type = "Gini"),
            Ouro = sum(`1st`), Prata = sum(`2nd`),
            Bronze = sum(`3rd`), Total = sum(Total)) %>%
  arrange(desc(Total))

#2. Comparando o gini pelo total de medalhas e pelo score por pais e ano
pais_ano <- olimpiadas %>% left_join(select(eventos, ID_discipline, Score),
                                    by = "ID_discipline") %>%
  mutate(Total_corrigido = Total*Score) %>%
  group_by(Country, Year) %>%
  summarise(Modalidades = n_distinct(Discipline),
            Gini_T = ineq(Total, type = "Gini"),
            Gini_S = ineq(Total_corrigido, type = "Gini"),
            Ouro = sum(`1st`), Prata = sum(`2nd`),
            Bronze = sum(`3rd`), Total = sum(Total)) %>%
  arrange(desc(Total))




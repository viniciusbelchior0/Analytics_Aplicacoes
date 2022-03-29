library(readxl)
driver_standings <- read_excel("C:/Users/NOTEBOOK CASA/Desktop/Analytics/Bases/Esportes/F1/df_f1_data.xlsx", 
                         sheet = "drivers_standings")

race_results <- read_excel("C:/Users/NOTEBOOK CASA/Desktop/Analytics/Bases/Esportes/F1/df_f1_data.xlsx", 
                         sheet = "race_results")

library(tidyverse)
library(ineq)

# Desequilibro por campeonato

# Vitorias por Equipe (concentracao de vitorias)
vit_equipe <- race_results %>% group_by(YEAR, CAR) %>% summarise(Wins = n())

# Campeoes: construtores e pilotos
construtores <- driver_standings %>% filter(POS != "DQ") %>%
  group_by(YEAR, CAR) %>% summarise(Pts = sum(PTS)) %>%
  mutate(PropPts = Pts/sum(Pts)) %>%
  slice(which.max(Pts))

pilotos <- driver_standings %>% filter(POS == "1")


# CONCENTRACAO 
anos_ineq_vit <- vit_equipe %>% group_by(YEAR) %>% summarise(Gini = ineq(Wins, type = "Gini")) %>%
  arrange(Gini)

anos_ineq_pts <- driver_standings %>% group_by(YEAR,CAR) %>%
  summarise(Pts = sum(PTS)) %>%
  group_by(YEAR) %>% summarise(Gini = ineq(Pts, type = "Gini")) %>%
  arrange(Gini)

## unindo informacoes
anos_ineq_pts <- left_join(anos_ineq_pts, pilotos, by = "YEAR") %>%
  select(-c(3,6))
anos_ineq_pts <- left_join(anos_ineq_pts, construtores, by = "YEAR")
colnames(anos_ineq_pts) <- c("YEAR","Gini_Pts", "DRIVERS_CHAMPION","NATIONALITY","DRIVERS_CAR",
                             "DRIVERS_PTS","CONSTRUCTORS_CHAMPION","CONSTRUCTORS_PTS", "PROP_PTS")


anos_ineq_vit <- left_join(anos_ineq_vit, pilotos, by = "YEAR") %>%
  select(-c(3,6))
anos_ineq_vit <- left_join(anos_ineq_vit, construtores, by = "YEAR")
colnames(anos_ineq_vit) <- c("YEAR","Gini_Wins", "DRIVERS_CHAMPION","NATIONALITY","DRIVERS_CAR",
                             "DRIVERS_PTS","CONSTRUCTORS_CHAMPION","CONSTRUCTORS_PTS","PROP_PTS")

#tabela final
df_f1_ineq <- left_join(anos_ineq_pts, anos_ineq_vit[,1:2], by = "YEAR") %>%
  select(1,2,10,3:9)

# VISUALIZACOES
#1 - relacao entre dominancia de vitorias e de pontos
ggplot(df_f1_ineq) +
 aes(x = Gini_Pts, y = Gini_Wins, colour = CONSTRUCTORS_CHAMPION, size = PROP_PTS, label=YEAR) +
 geom_point(shape = "circle") +
 scale_color_hue(direction = 1) +
 theme_gray() +
 geom_text(aes(label = YEAR), hjust =0, vjust = 0) +
 geom_hline(yintercept = median(df_f1_ineq$Gini_Wins), linetype = "dashed", color = "black") +
 geom_vline(xintercept = median(df_f1_ineq$Gini_Pts), linetype = "dashed", color =  "black") +
  annotate("text", x = 0.35, y = 0.05, label = "low win ineq, low pts ineq") +
  annotate("text", x= 0.7, y = 0.05, label = "low win ineq, high pts ineq") +
  annotate("text", x = 0.35, y = 0.57, label = "high win ineq, low pts ineq") +
  annotate("text", x = 0.7, y = 0.57, label = "high win ineq, high pts ineq")

#2 - evolucao temporal

ggplot(df_f1_ineq) +
 aes(x = YEAR, y = Gini_Pts) +
 geom_line(size = 1L, colour = "#B22222") +
 theme_classic() +
  geom_hline(yintercept = median(df_f1_ineq$Gini_Pts), color = "black") +
  geom_hline(yintercept = mean(df_f1_ineq$Gini_Pts), linetype = "dashed", color = "red")


# juntando as duas series
df_ineq_tidy <- df_f1_ineq %>% pivot_longer(Gini_Pts:Gini_Wins, names_to = "Index", values_to = "Value")

ggplot(df_ineq_tidy) +
 aes(x = YEAR, y = Value, colour = Index) +
 geom_line(size = 0.85) +
 scale_color_hue(direction = 1) +
 theme_gray() +
  geom_hline(yintercept = 0.54, linetype = "dashed", color = "red") +
  geom_hline(yintercept = 0.33, color = "blue")

## significado: alta concentracao de pontos significa campeonato desequilibrado,
# alta concentracao de vitorias significa que apenas poucas equipes ganhas as corridas


# Por nacioanlidade
nat <- ggplot(df_f1_ineq) +
  aes(x = Gini_Pts, y = Gini_Wins, colour = NATIONALITY, size = PROP_PTS, label=YEAR) +
  geom_point(shape = "circle") +
  scale_color_hue(direction = 1) +
  theme_gray() +
  geom_text(aes(label = YEAR), hjust =0, vjust = 0) +
  geom_hline(yintercept = median(df_f1_ineq$Gini_Wins), linetype = "dashed", color = "black") +
  geom_vline(xintercept = median(df_f1_ineq$Gini_Pts), linetype = "dashed", color =  "black") +
  annotate("text", x = 0.35, y = 0.05, label = "low win ineq, low pts ineq") +
  annotate("text", x= 0.7, y = 0.05, label = "low win ineq, high pts ineq") +
  annotate("text", x = 0.35, y = 0.57, label = "high win ineq, low pts ineq") +
  annotate("text", x = 0.7, y = 0.57, label = "high win ineq, high pts ineq")

library(plotly)
ggplotly(nat)


# Qual piloto esteve em campeonatos mais difices

library(dplyr)
library(ggplot2)

a <- df_ineq_tidy %>%
 filter(DRIVERS_CHAMPION %in% c("Emerson Fittipaldi", "Alain Prost", "Lewis Hamilton", 
"Nelson Piquet", "Michael Schumacher", "Max Verstappen", "Sebastian Vettel", "Ayrton Senna")) %>%
 filter(NATIONALITY %in% 
 c("GBR", "BRA", "GER", "FRA")) %>%
 filter(Index %in% "Gini_Pts") %>%
 ggplot() +
 aes(x = DRIVERS_CHAMPION, y = Value, fill = Index) +
 geom_boxplot(shape = "circle") +
 scale_fill_hue(direction = 1) +
 theme_gray()

b <- df_ineq_tidy %>%
  filter(DRIVERS_CHAMPION %in% c("Emerson Fittipaldi", "Alain Prost", "Lewis Hamilton", 
                                 "Nelson Piquet", "Michael Schumacher", "Max Verstappen", "Sebastian Vettel", "Ayrton Senna")) %>%
  filter(NATIONALITY %in% 
           c("GBR", "BRA", "GER", "FRA")) %>%
  filter(Index %in% "Gini_Wins") %>%
  ggplot() +
  aes(x = DRIVERS_CHAMPION, y = Value, fill = Index) +
  geom_boxplot(shape = "circle") +
  scale_fill_hue(direction = 1) +
  theme_gray()

library(gridExtra)

grid.arrange(a,b)


# Diferenca entre primeiro e segunda lugares
## em razao da amplitude
# formula = 1 - 2 / amplitude

lag_results <- driver_standings %>% group_by(YEAR) %>% mutate(Diff = abs(PTS - lag(PTS, default = PTS[1]))) %>%
  filter(POS %in% c("2")) %>% select(7,8)

runnerup_range <- driver_standings %>% group_by(YEAR) %>% summarise(Max = max(PTS), Min = min(PTS),
                                                  Amplitude = max(PTS)-min(PTS)) %>%
  left_join(lag_results, by = "YEAR") %>%
  mutate(RunnerupRange = Diff/Max,
         Range = Diff/Amplitude)

cor(runnerup_range$Diff, runnerup_range$RunnerupRange)


# pelo z-score, qual piloto foi o melhor de todos em determinada temporada
zscore_drivers <- driver_standings %>% group_by(YEAR) %>% mutate(Zscore = scale(PTS)) %>%
  arrange(desc(Zscore)) %>% filter(YEAR >= 1980 & POS == "1") %>% ungroup() %>%
  select(7,2,3,5,6,8) #considerando apos 1980 e apenas os campeoes


# Materia extra:
# https://ge.globo.com/motor/formula-1/noticia/relembre-todos-os-campeonatos-da-historia-da-f1-em-70-anos-parte-2.ghtml


# COMENTARIOS/CONCLUSAO: muito importante qual metrica/metodologia utilizar


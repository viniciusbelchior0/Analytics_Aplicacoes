library(tidyverse)
library(readxl)

WorldCupMatches <- read_excel("WorldCupMatches.xlsx", sheet = "Matches")
TopStandings <- read_excel("WorldCupMatches.xlsx", sheet = "Top_Standings")

data <- WorldCupMatches %>% select(-c(8,9,10))
TopStandings <- TopStandings %>% mutate(ID = paste(Team, "_", Tournament))

#1. Gols na Fase de Grupos ####
goals <- data %>% filter(Stage == "group stage") %>%
                  group_by(Team, Tournament, Stage) %>%
                  summarise(Matches = n(),Goals = sum(Team_Score), GoalsDiff = sum(Team_Score_Margin)) %>%
                  mutate(Year = as.numeric(str_sub(Tournament, -4,-1)), ID = paste(Team, "_", Tournament)) %>%
                  arrange(Year,desc(Goals), desc(GoalsDiff))

goals <- left_join(goals, TopStandings[,c(3,5)], by = "ID") %>%
         drop_na()

goals %>%
  filter(Year >= 1962L & Year <= 2018L) %>%
  ggplot() +
  aes(x = Position, y = Goals, size = GoalsDiff) +
  geom_point(shape = "circle", colour = "#112446") +
  theme_gray() +
  facet_wrap(vars(Year)) +
  geom_hline(yintercept = 6, linetype = "dashed", size = 1.05) +
  geom_hline(yintercept = 3, linetype = "dashed", color = "red", size = 1)

ggplot(goals) +
  aes(x = as.factor(Position), y = Goals, fill = as.factor(Position)) +
  geom_boxplot() +
  scale_fill_hue(direction = 1) +
  theme_minimal() +
  geom_hline(yintercept = 6, linetype = "dashed", size = 1.25, color = "red") +
  geom_hline(yintercept = 3, linetype = "dashed", size = 1, color = "blue")

ggplot(goals) +
  aes(x = as.factor(Position), y = GoalsDiff, fill = as.factor(Position)) +
  geom_boxplot() +
  scale_fill_hue(direction = 1) +
  theme_minimal() +
  geom_hline(yintercept = 4, linetype = "dashed", size = 1.25, color = "red") +
  geom_hline(yintercept = 2, linetype = "dashed", size = 1, color = "blue")


# Conclusao: 
# Os campeos geralmente nao sao os que mais fazem gols. Terceiros e quartos, no geral, apresentam
# melhor resultado nesse aspecto. Já houve casos em que o campeão fez apenas três gols na fase de grupos:
# Italia 1982 e Franca 2018. No entanto, eles apresentam saldo de gols com pouca variabilidade: entre 3 e 5.

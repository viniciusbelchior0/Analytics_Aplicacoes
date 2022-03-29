library(readxl)
library(tidyverse)
library(RcppRoll)
dados_cb <- read_excel("C:/Users/NOTEBOOK CASA/Desktop/CampeonatoBrasileiro/Dados_CampeonatoBrasileiro.xlsx")
dados_cb$Rodada <- as.numeric(dados_cb$Rodada)

serie_a <- dados_cb %>%
  select(-c(3,4,13,14,16)) %>%
  mutate(id_rodada = paste(Equipe, Ano, Rodada, sep = "_"),
                              pts = ifelse(Resultado == "V",3,ifelse(Resultado == "E",1,0)),
                              saldoGols = GP - GC,
                              `%golsPen` = PenFeitos/GP,
                              #efPen = PenFeitos/PenBatidos,
                              #defPen = DefPenAdv/PenAdv,
                              cardScore = CrtsA + 5*CrtV,
                              difFaltas = FaltasCometidas - FaltasSofridas,
                              `%golsContra` = GolsContra/GC)
#substiuir os Nas posteriormente; as métricas de eficiencia para pentalti, podem nao ser
# boas, pois nao sao capazes de discriminar se nao houve penalti ou o goleiro foi mal


# Substituindo NAs e Inf por 0
serie_a <- serie_a %>% mutate(`%golsPen` = replace_na(`%golsPen`,0),
                              `%golsContra` = replace_na(`%golsContra`,0))
serie_a <- serie_a %>% mutate_all(function(x) ifelse(is.infinite(x), 0, x)) 



# Agregando os dados ##
# lembrar de colocar aproveitamento em casa, como visitante, etc.
valor = 3

serie_a_rl <- serie_a %>%
  arrange(Equipe, Ano, Rodada) %>%
  group_by(Equipe, Ano) %>%
  mutate(rl_gp = roll_sum(GP, valor, align = "right", fill = NA),
         rl_gc = roll_sum(GC, valor, align = "right", fill = NA),
         rl_posse = roll_mean(Posse,valor,align = "right", fill = NA),
         rl_totalchutes = roll_mean(TotalChutes,valor, align = "right", fill = NA),
         rl_chgol = roll_mean(`ChGol%`,valor,align = "right", fill = NA),
         rl_gch = roll_mean(`G/Ch`,valor,align = "right", fill = NA),
         rl_cagc = roll_mean(CaGC, valor, align = "right", fill = NA),
         rl_def = roll_mean(`%Defesas`,valor,align = "right", fill = NA),
         rl_cleansheet = roll_sum(CleanSheet,valor,align = "right", fill = NA),
         rl_difaltas = roll_mean(difFaltas,valor,align = "right", fill = NA),
         rl_impedimentos = roll_mean(Impedimentos,valor,align = "right", fill = NA),
         rl_cruzamentos = roll_mean(Cruzamentos,valor,align = "right", fill = NA),
         rl_cortes = roll_mean(Cortes,valor,align = "right", fill = NA),
         rl_roubadas = roll_mean(RoubadasDeBola,valor,align = "right", fill = NA),
         rl_pts = roll_sum(pts,valor,align = "right", fill = NA),
         rl_saldogols = roll_sum(saldoGols,valor,align = "right", fill = NA),
         rl_cardscore = roll_mean(cardScore,valor,align = "right", fill = NA),
         rl_golspen = roll_mean(`%golsPen`,valor,align = "right", fill = NA),
         rl_golscontra = roll_mean(`%golsContra`,valor,align = "right",fill = NA)) %>%
  select(1:5,9,11,6,44:62) %>% ungroup()

colnames(serie_a_rl) <- c("equipe","ano","rodada","dia","local","oponente",
                                 "formacao","resultado","gp","gc","posse",
                                 "totalchutes","%chgol","g_ch","cagc","%defesas",
                                 "cleansheets","dif_faltas",
                                 "impedimentos","cruzamentos","cortes","roubadas","pts",
                                 "saldo_gols","cardscore","%gols_pen","%gols_contra")

serie_a_rl <- serie_a_rl %>% mutate(id_rodada = paste(equipe, ano,rodada, sep = "_"),
                                    id_opp = paste(oponente, ano, rodada, sep = "_"))

# dividir as tabelas 
home <- serie_a_rl %>% filter(local == "Em casa")
away <- serie_a_rl %>% filter(local == "Visitante")
colnames(away) <- c("equipe","ano","rodada","dia","local","oponente",
                          "opp_formacao","resultado","opp_gp","opp_gc","opp_posse",
                          "opp_totalchutes","opp_%chgol","opp_g_ch","opp_cagc","opp_%defesas",
                          "opp_cleansheets","opp_dif_faltas",
                          "opp_impedimentos","opp_cruzamentos","opp_cortes","opp_roubadas",
                    "opp_pts","opp_saldo_gols","opp_cardscore","opp_%gols_pen",
                    "%opp_gols_contra","id_rodada","id_opp")

away <- away %>% select(28,7,9:27)


# tipo 1 - geral
df_serieA <- left_join(home, away, by = c("id_opp" = "id_rodada")) %>% drop_na()

# tipo 2 - subtracao
df_serieA_2 <- df_serieA %>% mutate(gp = gp - opp_gp,
                                    gc = gc - opp_gc,
                                    posse= posse - opp_posse,
                                    totalchutes = totalchutes - opp_totalchutes,
                                    `%chgol`= `%chgol` - `opp_%chgol`,
                                    g_ch= g_ch - opp_g_ch,
                                    cagc = cagc - opp_cagc,
                                    `%defesas` = `%defesas` - `opp_%defesas`,
                                    cleansheets = cleansheets - opp_cleansheets ,
                                    dif_faltas = dif_faltas - opp_dif_faltas ,
                                    impedimentos = impedimentos - opp_impedimentos,
                                    cruzamentos = cruzamentos - opp_cruzamentos,
                                    cortes = cortes - opp_cortes ,
                                    roubadas = roubadas - opp_roubadas,
                                    pts = pts - opp_pts ,
                                    saldo_gols = saldo_gols - opp_saldo_gols,
                                    cardscore = cardscore - opp_cardscore ,
                                    `%gols_pen` = `%gols_pen` - `opp_%gols_pen`,
                                    `%gols_contra` = `%gols_contra` - `%opp_gols_contra`) %>%
  select(1:27,30)

# OBS:
# -> fazer as tabelas para roll de 1,3,5,10
# -> fazer a tabela para geral (apenas usar a funcao de agregacao original)

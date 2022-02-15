install.packages('sf')
install.packages('ggspatial')

# Instalando os demais pacotes
install.packages(c("gridExtra","GoodmanKruskal","FactoMineR","readxl","lubridate","fastDummies", "dendextend", "caret",
"e1071","rpart","randomForest"))

# Carregando os pacotes e configurado o ambiente
library(tidyverse)
library(gridExtra)
library(GoodmanKruskal)
library(FactoMineR)
library(readxl)
library(lubridate)
library(fastDummies)
library(dendextend)
library(caret)
library(e1071)
library(rpart)
library(randomForest)
library(sf)
library(ggspatial)
options(scipen = 999, digits = 2)

acidentes01 <- read.csv2("/content/acidentes01.csv", encoding = "UTF-8")
acidentes02 <- read.csv2("/content/acidentes02.csv", encoding = "UTF-8")
acidentes03 <- read.csv2("/content/acidentes03.csv", encoding = "UTF-8")
acidentes04 <- read.csv2("/content/acidentes04.csv", encoding = "UTF-8")

acidentes01[,c(1,26:30)] <- lapply(acidentes01[,c(1,26:30)], function(x){as.character(x)})
acidentes02[,c(1,6,26:30)] <- lapply(acidentes02[,c(1,6,26:30)], function(x){as.character(x)})
acidentes03[,c(1,6,26:30)] <- lapply(acidentes03[,c(1,6,26:30)], function(x){as.character(x)})
acidentes04[,c(1,6,26:27)] <- lapply(acidentes04[,c(1,6,26:27)], function(x){as.character(x)})

acidentes01$km <- as.numeric(acidentes01$km)

acidentes <- bind_rows(acidentes01, acidentes02, acidentes03, acidentes04)

head(acidentes) #nao remover NAs pois ainda ha muitas informacoes uteis / base nao tem dados para certa data

str(acidentes)

acidentes$km <- round(acidentes$km, digits = 0)

acidentes$data_inversa <- as.Date(acidentes$data_inversa, "%d/%m/%Y")

acidentes <- acidentes %>% mutate(ano = year(data_inversa), mes = month(data_inversa), hora = as.POSIXct(acidentes$horario, format = "%H:%M:%S"))

acidentes$horario <- substring(acidentes$horario,1,2)

acidentes <- acidentes %>% filter(classificacao_acidente != "")

head(acidentes)

tipo_acidentes <- read_excel("/content/acidentes_rotulos_corrigidos.xlsx", col_types = c("text", "text"), sheet = 1)
causa_acidentes <- read_excel("/content/acidentes_rotulos_corrigidos.xlsx", col_types = c("text", "text"), sheet = 2)

head(tipo_acidentes)
head(causa_acidentes)

acidentes <- left_join(acidentes, causa_acidentes, by = "causa_acidente")
acidentes <- left_join(acidentes, tipo_acidentes, by = "tipo_acidente")

head(acidentes)

str(acidentes)

acidentes$dia_semana <- str_replace_all(acidentes$dia_semana, c("Sexta" = "sexta-feira", "Sábado" = "sábado",
"Domingo" = "domingo", "Segunda" = "segunda-feira", "Quinta" = "quinta-feira", "Quarta" = "quarta-feira",
"Terça" = "terça-feira"))

acidentes$dia_semana <- str_replace_all(acidentes$dia_semana, c("segunda-feira" = "1.segunda-feira", "terça-feira" = "2.terça-feira",
"quarta-feira" = "3.quarta-feira", "quinta-feira" = "4.quinta-feira", "sexta-feira" = "5.sexta-feira", "sábado" = "6.sábado",
"domingo" = "7.domingo"))

"""Agregação e análise de dados"""

acidentes %>% group_by(data_inversa) %>% summarise(acidentes = n()) %>% ggplot(aes(x = data_inversa, y = acidentes)) + geom_line(size = 0.5, colour = "#EF562D") +
labs(title = "Acidentes por Data", x = "Data", y = "Acidentes") + theme_gray()

acidentes %>%
 ggplot() +
  aes(x = data_inversa, fill = classificacao_acidente) +
  geom_bar() +
  scale_fill_hue(direction = 1) +
  theme_gray() +
  labs(title = "Classificação dos Acidentes por Data", x = "Data", y = "Acidentes") + theme(legend.position = "bottom")

table(acidentes$tipo_acidente_corrigido, acidentes$ano) %>% as.data.frame() %>%
ggplot(aes(Var2, Var1)) + geom_tile(aes(fill = Freq),colour = "white", na.rm = TRUE) +
  scale_fill_gradient(low = "#E0EEF7", high = "#548BA1") +  
  guides(fill=guide_legend(title="Total de Acidentes")) +
  theme_bw() + theme_minimal() + 
  labs(title = "Tipo de Acidente por Ano (Total)",
       x = "Ano", y = "Tipo de Acidente") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())

acidentes %>% group_by(dia_semana) %>% summarise(N = n()) %>% arrange(desc(N)) %>%
  mutate(Prop = N/sum(N))

filters <- list("dia_semana == '1.segunda-feira' ",
                "dia_semana == '2.terça-feira' ",
                "dia_semana == '3.quarta-feira' ",
                "dia_semana == '4.quinta-feira' ",
                "dia_semana == '5.sexta-feira' ",
                "dia_semana == '6.sábado' ",
                "dia_semana == '7.domingo' ")


myfun <- function(fltr, df){
  
  df <- filter(df, eval(parse(text = fltr)))
  ggplot(df, aes(x = as.numeric(horario))) +
    geom_histogram(bins = 24L, fill = "#EF562D", color = "black") +
    ggtitle(fltr) +
    theme_gray()
  
}


ggs <- lapply(filters, myfun, df = acidentes)

gridExtra::grid.arrange(grobs = ggs)

acidentes %>% group_by(classificacao_acidente) %>% summarise(acidentes = n(), pessoas = sum(pessoas),
veiculos = sum(veiculos)) %>% mutate(Prop = acidentes/sum(acidentes)) %>%
 arrange(desc(acidentes))

acidentes %>% group_by(tipo_acidente) %>% summarise(Acidentes = n(), pessoas = sum(pessoas), veiculos = sum(veiculos),ilesos = sum(ilesos) ,feridos = sum(feridos), graves = sum(feridos_graves),
mortos = sum(mortos), ignorados = sum(ignorados), tSemVitimas = sum(classificacao_acidente == "Sem Vítimas"),tFeridas = sum(classificacao_acidente == "Com Vítimas Feridas"),
tFatais = sum(classificacao_acidente == "Com Vítimas Fatais"),tIgnorado = sum(classificacao_acidente == "Ignorado")) %>% mutate(across(10:13, ~ . / Acidentes)) %>% mutate(AcGraves = tFeridas + tFatais) %>% arrange(desc(Acidentes)) %>%
ggplot() + aes(x = Acidentes,y = AcGraves,colour = tipo_acidente,size = pessoas) + geom_point(shape = "circle") +
scale_color_hue(direction = 1) + theme_gray() + labs(title = "Relação entre Número de Acidentes e Gravidade", x = "Número de Acidentes",
y = "Proporção de Acidentes Graves", subtitle = "A proporção de Acidentes graves é a soma dos acidentes que resultaram em pessoas feridas ou mortas em relação ao total de acidentes")

"""primeiro: analisar as causas de acidentes (variaveis categoricas) (feito)

segundo: reunir essas informacoes e colocar as variaveis quantitativas (classificacao acidente ~ veiculos, pessoas; tipo = boxplot)

terceiro: sumarisar as BRs e criar um score (e fazer mapa com acidentes recentes) <> fazer mapa com causa de acidente por regiao

# Relação entre variáveis categóricas: Teste qui-quadrado e Goodman-Kruskal
"""

chisq.test(acidentes$classificacao_acidente, acidentes$tipo_acidente_corrigido)
chisq.test(acidentes$classificacao_acidente, acidentes$causa_acidente_corrigido)
chisq.test(acidentes$classificacao_acidente, acidentes$condicao_metereologica)
chisq.test(acidentes$classificacao_acidente, acidentes$fase_dia)
chisq.test(acidentes$classificacao_acidente, acidentes$sentido_via)
chisq.test(acidentes$classificacao_acidente, acidentes$dia_semana)
chisq.test(acidentes$classificacao_acidente, acidentes$uso_solo)
chisq.test(acidentes$classificacao_acidente, acidentes$tipo_pista)
chisq.test(acidentes$classificacao_acidente, acidentes$tracado_via)
# todos eles parecem estar errados -> problema de alta amostragem e muitos rótulos ???

acidentes %>% select(c(11:17,34:35)) %>% GKtauDataframe() %>% plot(corrColors = "blue")

"""Relação entre variáveis categóricas: Totais e Proporções"""

#1.1 - Total de classificação do acidente por tipo do acidente
table(acidentes$tipo_acidente_corrigido,acidentes$classificacao_acidente) %>% as.data.frame() %>%
ggplot(aes(Var2, Var1)) + geom_tile(aes(fill = Freq),colour = "white", na.rm = TRUE) +
  scale_fill_gradient(low = "#E0EEF7", high = "#548BA1") +  
  guides(fill=guide_legend(title="Total de Acidentes")) +
  theme_bw() + theme_minimal() + 
  labs(title = "Relação entre Desfecho do Acidente com o tipo de Acidente(Total)",
       x = "Classificação do Acidente", y = "Tipo de Acidente") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
  axis.text.x = element_text(angle = 90, vjust = 0.5, hjust =1))

#1.2 - Proporcao da classificacao do acidente por tipo de acidente
table(acidentes$tipo_acidente_corrigido,acidentes$classificacao_acidente) %>% as.data.frame() %>% pivot_wider(names_from = Var2, values_from = Freq)  %>%
mutate(Total = rowSums(across(where(is.numeric)))) %>% ungroup()  %>% mutate(across(2:6, ~ . / Total)) %>% select(-Total) %>% pivot_longer(2:5, names_to = "Hora", values_to = "PropAcidentes") %>%
ggplot(aes(Hora, Var1)) + geom_tile(aes(fill = PropAcidentes),colour = "white", na.rm = TRUE) +
  scale_fill_gradient(low = "white",high = "red") +  
  guides(fill=guide_legend(title="Proporção da Classificação")) +
  theme_bw() + theme_minimal() + 
  labs(title = "Relação entre desfecho do Acidente com o Tipo de Acidente(Proporção)",
       x = "Classificação do Acidente", y = "Tipo do Acidente") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
  axis.text.x = element_text(angle = 90, vjust = 0.5, hjust =1))

#2.1 - Classificacao o acidente por dia da semana (total)
table(acidentes$classificacao_acidente,acidentes$dia_semana) %>% as.data.frame() %>%
ggplot(aes(Var2, Var1)) + geom_tile(aes(fill = Freq),colour = "white", na.rm = TRUE) +
  scale_fill_gradient(low = "#E0EEF7", high = "#548BA1") +  
  guides(fill=guide_legend(title="Total de Acidentes")) +
  theme_bw() + theme_minimal() + 
  labs(title = "Desfecho do acidente por Dia da Semana (Total de Acidentes)",
       x = "Dia da semana", y = "Desfecho do acidente") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
  axis.text.x = element_text(angle = 90, vjust = 0.5, hjust =1))

#2.2 - Proporção da classificação do acidente por dia da semana
table(acidentes$classificacao_acidente,acidentes$dia_semana) %>% as.data.frame() %>%
pivot_wider(names_from = Var2, values_from = Freq)  %>%
mutate(Total = rowSums(across(where(is.numeric)))) %>% ungroup() %>% mutate(across(2:9, ~ . / Total)) %>% select(-Total) %>% pivot_longer(2:8, names_to = "Hora", values_to = "PropAcidentes") %>%
ggplot(aes(Hora, Var1)) + geom_tile(aes(fill = PropAcidentes),colour = "white", na.rm = TRUE) +
  scale_fill_gradient(low = "white",high = "red") +  
  guides(fill=guide_legend(title="Proporção do Tipo Acidente")) +
  theme_bw() + theme_minimal() + 
  labs(title = "Desfecho de acidente por dia da semana (Proporção por Dia)",
       x = "Dia da Semana", y = "Classificacao Acidente") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
  axis.text.x = element_text(angle = 90, vjust = 0.5, hjust =1))

#3.1 - Relacao entre tipo de acidente e dia da semana (total) 
table(acidentes$tipo_acidente_corrigido,acidentes$dia_semana) %>% as.data.frame() %>%
ggplot(aes(Var2, Var1)) + geom_tile(aes(fill = Freq),colour = "white", na.rm = TRUE) +
  scale_fill_gradient(low = "#E0EEF7", high = "#548BA1") +  
  guides(fill=guide_legend(title="Total de Acidentes")) +
  theme_bw() + theme_minimal() + 
  labs(title = "Relação entre Tipo de Acidente e Dia da Semana (Total de Acidentes)",
       x = "Dia da semana", y = "Tipo de Acidente") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
  axis.text.x = element_text(angle = 90, vjust = 0.5, hjust =1))

#3.2 - Relação entre tipo do acidente e dia da semana(proporção)
table(acidentes$tipo_acidente_corrigido,acidentes$dia_semana) %>% as.data.frame() %>%
pivot_wider(names_from = Var2, values_from = Freq)  %>%
mutate(Total = rowSums(across(where(is.numeric)))) %>% ungroup() %>% mutate(across(2:9, ~ . / Total)) %>% select(-Total) %>% pivot_longer(2:8, names_to = "Hora", values_to = "PropAcidentes") %>%
ggplot(aes(Hora, Var1)) + geom_tile(aes(fill = PropAcidentes),colour = "white", na.rm = TRUE) +
  scale_fill_gradient(low = "white",high = "red") +  
  guides(fill=guide_legend(title="Proporção do Tipo Acidente")) +
  theme_bw() + theme_minimal() + 
  labs(title = "Relação entre Tipo de Acidente e Dia da Semana (Proporção de Acidentes por Dia)",
       x = "Dia da semana", y = "Tipo Acidente") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
  axis.text.x = element_text(angle = 90, vjust = 0.5, hjust =1))

"""horario ~ tipo_acidente e classificacao_acidente no mesmo grafico (gridArrow)

"""

#4.1 - relacao entre desfecho do acidente e horario(total)
table(acidentes$classificacao_acidente,acidentes$horario) %>% as.data.frame() %>%
ggplot(aes(Var2, Var1)) + geom_tile(aes(fill = Freq),colour = "white", na.rm = TRUE) +
scale_fill_gradient(low = "#E0EEF7", high = "#548BA1") +  
  guides(fill=guide_legend(title="Total de Acidentes")) +
  theme_bw() + theme_minimal() + 
  labs(title = "Relação entre desfecho do Acidente e seu Horário (Total de Acidentes)",
       x = "Hora", y = "Classificação do Acidente") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
  axis.text.x = element_text(angle = 90, vjust = 0.5, hjust =1))

#4.2 - relacao entre desfecho do acidente e horario (proporcao)
table(acidentes$classificacao_acidente,acidentes$horario) %>% as.data.frame() %>% pivot_wider(names_from = Var2, values_from = Freq) %>%
mutate(Total = rowSums(across(where(is.numeric)))) %>% ungroup() %>% mutate(across(2:26, ~ . / Total)) %>% select(-Total) %>% pivot_longer(2:24, names_to = "Hora", values_to = "PropAcidentes") %>%
ggplot(aes(Hora, Var1)) + geom_tile(aes(fill = PropAcidentes),colour = "white", na.rm = TRUE) +
  scale_fill_gradient(low = "white",high = "red") +  
  guides(fill=guide_legend(title="Proporção da Classificação")) +
  theme_bw() + theme_minimal() + 
  labs(title = "Relação entre desfecho do Acidente e seu Horário (Proporção por Horário)",
       x = "Hora", y = "Classificação do Acidente") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
  axis.text.x = element_text(angle = 90, vjust = 0.5, hjust =1))

#5.1 - Relação entre tipo de acidente e horario
table(acidentes$tipo_acidente_corrigido,acidentes$horario) %>% as.data.frame() %>%
ggplot(aes(Var2, Var1)) + geom_tile(aes(fill = Freq),colour = "white", na.rm = TRUE) +
  scale_fill_gradient(low = "#E0EEF7", high = "#548BA1") +  
  guides(fill=guide_legend(title="Total de Acidentes")) +
  theme_bw() + theme_minimal() + 
  labs(title = "Relação entre tipo de acidente e seu Horário",
       x = "Hora", y = "Tipo do Acidente") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
  axis.text.x = element_text(angle = 90, vjust = 0.5, hjust =1))

#5.2 - Relação entre tipo de acidente e horario
table(acidentes$tipo_acidente_corrigido,acidentes$horario) %>% as.data.frame() %>% pivot_wider(names_from = Var2, values_from = Freq) %>%
mutate(Total = rowSums(across(where(is.numeric)))) %>% ungroup() %>% mutate(across(2:26, ~ . / Total)) %>% select(-Total) %>% pivot_longer(2:25, names_to = "Hora", values_to = "PropAcidentes") %>%
ggplot(aes(Hora, Var1)) + geom_tile(aes(fill = PropAcidentes),colour = "white", na.rm = TRUE) +
  scale_fill_gradient(low = "white",high = "red") +  
  guides(fill=guide_legend(title="Proporção da Classificação")) +
  theme_bw() + theme_minimal() + 
  labs(title = "",
       x = "Hora", y = "Tipo do Acidente") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
  axis.text.x = element_text(angle = 90, vjust = 0.5, hjust =1))

""" Multiple Correspondence Analysis
 

"""

mca <- MCA(acidentes[,c(11:17)])

summary(mca)

plot.MCA(mca)

"""BRs e seus desfechos"""

BRS <- acidentes %>% group_by(br) %>% summarise(Estados = n_distinct(uf),Municipios = n_distinct(municipio),Acidentes = n(), Pessoas = sum(pessoas), Veiculos = sum(veiculos),
Ilesos = sum(ilesos), Feridos = sum(feridos), Graves = sum(feridos_graves), Mortos = sum(mortos), ignorados = sum(ignorados),
tSemVitimas = sum(classificacao_acidente == "Sem Vítimas"),tFeridas = sum(classificacao_acidente == "Com Vítimas Feridas"),
tFatais = sum(classificacao_acidente == "Com Vítimas Fatais"),tIgnorado = sum(classificacao_acidente == "Ignorado")) %>% mutate(PropFeridosGrave = Graves/Feridos) %>%
select(-Graves) %>% arrange(desc(Acidentes))

head(BRS)

#BRs por acidentes fatais
BRS %>% select(1,4,11:14) %>% mutate(Total = rowSums(across(tSemVitimas:tIgnorado))) %>% ungroup() %>% mutate(across(3:7, ~ . / Total)) %>% select(-Total) %>%
filter(Acidentes > 217) %>% top_n(15, tFatais) %>% arrange(desc(tFatais)) %>% ungroup() %>% mutate(br2 = fct_reorder(br, tFatais)) %>%
ggplot(aes(x = br2, y = tFatais)) + geom_col(fill = "#B22222") + coord_flip() + labs(x = "BR", y = "Proporção de Acidentes Fatais", title = "Proporção de Acidentes Fatais por BR",
subtitle = "Foram consideradas apenas as BRs com mais de 217 acidentes no total")

#BRs por vitimas feridas
BRS %>% select(1,4,11:14) %>% mutate(Total = rowSums(across(tSemVitimas:tIgnorado))) %>% ungroup() %>% mutate(across(3:7, ~ . / Total)) %>% select(-Total) %>%
filter(Acidentes > 217) %>% top_n(15, tFeridas) %>% arrange(desc(tFeridas)) %>% ungroup() %>% mutate(br2 = fct_reorder(br, tFeridas)) %>%
ggplot(aes(x = br2, y = tFeridas)) + geom_col(fill = "#FF8C00") + coord_flip() + labs(x = "BR", y = "Proporção de Acidentes que tiveram vítimas Feridas", title = "Proporção de Acidentes com vítimas Feridas por BR",
subtitle = "Foram consideradas apenas as BRs com mais de 217 acidentes no total")

#BRs por vitimas feridas
BRS %>% select(1,4,11:14) %>% mutate(Total = rowSums(across(tSemVitimas:tIgnorado))) %>% ungroup() %>% mutate(across(3:7, ~ . / Total)) %>% select(-Total) %>%
filter(Acidentes > 217) %>% top_n(15, tSemVitimas) %>% arrange(desc(tSemVitimas)) %>% ungroup() %>% mutate(br2 = fct_reorder(br, tSemVitimas)) %>%
ggplot(aes(x = br2, y = tSemVitimas)) + geom_col(fill = "#4682B4") + coord_flip() + labs(x = "BR", y = "Proporção de Acidentes sem Vítimas", title = "Proporção de Acidentes sem vítimas por BR",
subtitle = "Foram consideradas apenas as BRs com mais de 217 acidentes no total")

BRS_clean <- BRS %>% mutate(across(11:14, ~ . / Acidentes)) %>% ungroup() %>% select(1,2,3,4,11:15) %>% drop_na(br) %>% filter(Acidentes > 212)

dist_BRS <- BRS_clean %>% select(-1) %>% scale() %>% dist(method = "euclidean")

clustering_hierarquical <- function(distancia, method){
  hc_brs <- hclust(distancia, method = method)
  coph_brs <- cophenetic(hc_brs)
  cor(dist_BRS,coph_brs)
}

metodos <- list("ward.D","ward.D2","single","complete","average","mcquitty","median",
                "centroid")

lapply(metodos, clustering_hierarquical, distancia = dist_BRS)

hc_BRS <- hclust(dist_BRS, method = "average")
hc_BRS %>% as.dendrogram() %>% color_branches(k = 6) %>% plot(leaflab = "none")

cluster_assignments <- cutree(hc_BRS, k = 6)#fazendo o corte

BRS <- BRS_clean %>% mutate(cluster_hc = cluster_assignments)

BRS %>% group_by(cluster_hc) %>%
summarise(N = n(), Estados = round(mean(Estados),0), Municipios = round(mean(Municipios)), Acidentes = mean(Acidentes),
tSemVitimas = mean(tSemVitimas), tFeridas = mean(tFeridas), tFatais = mean(tFatais),tIgnorado = mean(tIgnorado),
PropFeridosGrave = mean(PropFeridosGrave))

"""# Estados"""

UFS <- acidentes %>% group_by(uf) %>% summarise(Municipios = n_distinct(municipio),Acidentes = n(), Pessoas = sum(pessoas), Veiculos = sum(veiculos),
Ilesos = sum(ilesos), Feridos = sum(feridos), Graves = sum(feridos_graves), Mortos = sum(mortos), ignorados = sum(ignorados),
tSemVitimas = sum(classificacao_acidente == "Sem Vítimas"),tFeridas = sum(classificacao_acidente == "Com Vítimas Feridas"),
tFatais = sum(classificacao_acidente == "Com Vítimas Fatais"),tIgnorado = sum(classificacao_acidente == "Ignorado")) %>% mutate(PropFeridosGrave = Graves/Feridos) %>%
select(-Graves) %>% arrange(desc(Acidentes))

regioes <- read_excel("/content/regioes.xlsx", col_types = c("text", "text"), sheet = 1)

UFS <- left_join(UFS, regioes, by = "uf")

head(UFS)

ggplot(UFS) + aes(x = Acidentes, y = Mortos, size = Municipios, color = Regiao) + geom_point(shape = "circle") + theme_gray() +
labs(title = "Relação entre Acidentes e Mortos por UF", x =  "Acidentes" , y = "Mortos")

table(acidentes$uf,acidentes$tipo_acidente_corrigido) %>% as.data.frame() %>% pivot_wider(names_from = Var2, values_from = Freq) %>%
mutate(Total = rowSums(across(where(is.numeric)))) %>% ungroup() %>% slice(-1) %>% mutate(across(2:20, ~ . / Total)) %>% select(-Total) %>% pivot_longer(2:19, names_to = "UF", values_to = "PropAcidentes") %>%
ggplot(aes(UF, Var1)) + geom_tile(aes(fill = PropAcidentes),colour = "white", na.rm = TRUE) +
  scale_fill_gradient(low = "white",high = "red") +  
  guides(fill=guide_legend(title="Proporção da Classificação")) +
  theme_bw() + theme_minimal() + 
  labs(title = "Relação entre UF e Tipo Acidente (Proporção)",
       x = "Tipo do Acidente", y = "UF") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
  axis.text.x = element_text(angle = 90, vjust = 0.5, hjust =1))

# ler o shape file
br_mapa <- st_read('/content/BR_UF_2019.shp')

#montar a base para os dados
base_ufs <- UFS %>% select(1,3,10:13) %>% mutate(across(2:6, ~ . / Acidentes))

#fazer join com o shapefile
base_mapa <- left_join(br_mapa, base_ufs, by = c("SIGLA_UF" = "uf"))

ggplot() + geom_sf(data = base_mapa , aes(fill = tFatais)) + scale_fill_gradient(low ="#D48787", high = "#B22222" ) + labs(title = "Proporção de Acidentes Fatais por UF")

ggplot() + geom_sf(data = base_mapa , aes(fill = tFeridas)) + scale_fill_gradient(low ="#EFBE81" , high = "#EF562D") + labs(title = "Proporção de Acidentes com Pessoas Feridas por UF")

ggplot() + geom_sf(data = base_mapa , aes(fill = tSemVitimas)) + scale_fill_gradient(low ="#BBE0FF" , high ="#054A84" ) + labs(title = "Proporção de Acidentes sem Vítimas por UF")

# Score para as identidades (BR-mun) > antees de aplicar os rotulos, calcular os tipos de acidentes ocorridos e suas respectivas proporcoes,
# o score de multiplicacao sera aplicado a proporcao e havera um ajuste baseado no volume de acidentes (acidentes ~ populacao)

BRS_MUN <- acidentes %>% group_by(br, municipio) %>% summarise(Acidentes = n(), Pessoas = sum(pessoas), Veiculos = sum(veiculos),
Ilesos = sum(ilesos), Feridos = sum(feridos), Graves = sum(feridos_graves), Mortos = sum(mortos), ignorados = sum(ignorados),
tSemVitimas = sum(classificacao_acidente == "Sem Vítimas"),tFeridas = sum(classificacao_acidente == "Com Vítimas Feridas"),
tFatais = sum(classificacao_acidente == "Com Vítimas Fatais"),tIgnorado = sum(classificacao_acidente == "Ignorado")) %>% mutate(PropFeridosGrave = Graves/Feridos) %>%
mutate(across(11:14, ~ . / Acidentes)) %>% ungroup() %>% select(-Graves) %>% mutate(Score = tSemVitimas + tIgnorado + tFeridas * 5 + tFatais * 10) %>% mutate(ScoreAdj = Score/Acidentes, BrMun = paste(br, municipio, sep = "_" )) %>%
arrange(desc(Score))

head(BRS_MUN)

quantile(BRS_MUN$Acidentes, seq(0,1,0.1))

BRS_MUN %>% filter(Acidentes > 35) %>% top_n(25, ScoreAdj) %>% arrange(desc(ScoreAdj)) %>% ungroup() %>% mutate(BrMun2 = fct_reorder(BrMun, ScoreAdj)) %>%
ggplot(aes(x = BrMun2, y = ScoreAdj)) + geom_col(fill = "#1DC792") + coord_flip() + labs(x = "BR-Município", y = "Score", title = "OS 25 Municípios com Maior Score",
subtitle = "Foram consideradas apenas os Municípios com mais de 35 acidentes no total")

"""# Modelagem - Previsao do Risco do Acidente"""

acidentes <- acidentes %>% mutate(Desfecho_acidente = ifelse(mortos + feridos > 0,"1","0"),
BrMun = paste(br, municipio, sep = "_")) #prestar atencao na proporcao, ja que possivelmente
# possa ter sido alterada devido aos filtros e remocao de NAs

Scores <- BRS_MUN %>% select(BrMun, ScoreAdj)

acidentes <- left_join(acidentes, Scores, by = "BrMun")

head(acidentes)

names(acidentes)

acidentes_modelling <- acidentes %>% select(1,3,4,12,13,14,15,16,17,36,38) %>% mutate(Final_Semana = ifelse(dia_semana == "6.sábado",1,ifelse(dia_semana == "7.domingo",1,0)))

head(acidentes_modelling)

acidentes_modelling <- acidentes_modelling %>% select(-c(2,3))

# Espaço para holdout  >>> #Score possivelmente influencia bastante (ele é uma composicao linear do objetivo a ser atingido)

table(acidentes_modelling$Desfecho_acidente)

# corrigindo o tipo das variaveis
acidentes_modelling[,c(2:8,10)] <- lapply(acidentes_modelling[,c(2:8,10)], function(x){as.factor(x)})

#reduzindo a amostragem por ela ser muito alta e custar muito processamento
samples <- acidentes_modelling$Desfecho_acidente %>% createDataPartition(p = 0.02, list = FALSE)
model_df <- acidentes_modelling[samples,]

#fazendo o verdadeiro holdout dos dados a serem testados
train_samples <- model_df$Desfecho_acidente %>% createDataPartition(p = 0.75,list = FALSE)
train_data <- model_df[train_samples,] %>% select(2:7,9:10,8)
test_data <- model_df[-train_samples,] %>% select(2:7,9:10,8)

head(train_data)

# Modelagem 1 - Regressao Logistica

reg1 <- glm(Desfecho_acidente ~ ., data = train_data, family = binomial)

summary(reg1)

pred1 <- predict(reg1,test_data[,-9])

pred1 <- ifelse(pred1 > 0.5,1,0)

table(test_data[,9],pred1) %>% confusionMatrix()

# Modelagem 2 - SVM

reg2 <- svm(Desfecho_acidente ~., data = train_data, kernel = "linear", type = "C-classification")

pred2 <- predict(reg2, newdata = test_data[,-9])

table(test_data[,9], pred2) %>% confusionMatrix()

# Modelagem 3 - Decision Tree

reg3 <- rpart(Desfecho_acidente ~., data = train_data)

pred3 <- predict(reg3, newdata = test_data[,-9])

pred3 <- pred3 %>% as.data.frame()
colnames(pred3) <- c("zero","one")
pred3 <- pred3 %>% mutate(result = ifelse(zero > one, 0,1))

table(test_data$Desfecho_acidente,pred3$result) %>% confusionMatrix()

# Modelagem 4 - Random Forest

reg4 <- randomForest(Desfecho_acidente ~., data = train_data, ntree = 100)

pred4 <- predict(reg4, newdata = test_data[,-9])

table(test_data[,9], pred4) %>% confusionMatrix()

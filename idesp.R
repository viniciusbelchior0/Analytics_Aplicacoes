library(readxl)
library(tidyverse)
library(kableExtra)
IDESP <- read_excel("C:/Users/NOTEBOOK CASA/Desktop/Analytics/General/Análises/Sociais/Educação/IDESP.xlsx", 
                    col_types = c("text", "text", "text", 
                                  "text", "text", "text", "text", "numeric", 
                                  "numeric", "numeric", "numeric", 
                                  "numeric", "numeric", "numeric", 
                                  "numeric", "numeric", "numeric", 
                                  "numeric", "numeric", "numeric"))

#1. pivot longer ####
IDESP_long <- IDESP %>% pivot_longer(`2007`:`2019`, names_to = "Ano", values_to = "Nota")

IDESP_long %>% group_by(ESCOLA) %>% filter(MUNICIPIO == "JABOTICABAL" & `NIVEL ENSINO` == "ENSINO MEDIO") %>%
  summarise(NotaMedia = mean(Nota, na.rm = TRUE), Desvio = sd(Nota, na.rm = TRUE)*100) %>%
  kbl() %>% kable_paper()


#boxplot ensino medio jaboticabal
IDESP_long %>% filter(MUNICIPIO == "JABOTICABAL") %>%
ggplot() +
  aes(x = ESCOLA, y = Nota, fill = ESCOLA) +
  geom_boxplot(shape = "circle") +
  scale_fill_hue(direction = 1) +
  theme_gray() +
  theme(legend.position = "bottom", axis.text.x = element_blank()) +
  facet_wrap(vars(`NIVEL ENSINO`))

#2. pivot wider ####
IDESP_wider <- IDESP_long %>% pivot_wider(names_from = `NIVEL ENSINO`, values_from = Nota)

IDESP_wider %>% group_by(MUNICIPIO) %>% summarise(Iniciais = mean(`ANOS INICIAIS`, na.rm = TRUE),
                                                  Finais = mean(`ANOS FINAIS`, na.rm = TRUE),
                                                  Médio = mean(`ENSINO MEDIO`, na.rm = TRUE)) %>%
  arrange(desc(Médio)) %>% kbl() %>%
  kable_paper()


#correlacao entre notas dos niveis
IDESP_wider %>% select(8:10) %>% cor(use = "complete.obs") %>% corrplot::corrplot(method = "color",
                                                            addCoef.col = "black")


#3. MAPA DA EDUCACAO ####
library(geobr)
library(sf)
library(ggspatial)

sp_mapa <- st_read('C:/Users/NOTEBOOK CASA/Desktop/Analytics/General/Análises/Sociais/Educação/sp_municipios/SP_Municipios_2019.shp')
str(sp_mapa)


# plotando um mapa usando ggplot
# criando uma base resumos das cidades
base <- IDESP_long %>% group_by(Cod_Municipio,MUNICIPIO) %>% filter(`NIVEL ENSINO` == "ENSINO MEDIO") %>%
  summarise(Nota = mean(Nota, na.rm = TRUE))
base <- base[-c(1:9),]
colnames(base) <- c("CD_MUN","NM_MUN","Nota")
base$NM_MUN <- NULL

base_dados <- merge(sp_mapa, base, by.x = "CD_MUN", by.y = "CD_MUN")

#mapa das notas por municipio
ggplot() +
  geom_sf(data = base_dados,color = "black", fill = NA) +
  geom_sf(data = base_dados, aes(fill = Nota)) +
  scale_fill_viridis_b()


sp_mapa_meso <- st_read('C:/Users/NOTEBOOK CASA/Desktop/Analytics/General/Análises/Sociais/Educação/sp_mesoregioes/SP_Mesorregioes_2020.shp')

#colocando o limite das mesorregioes no mapa
ggplot() +
  geom_sf(data = base_dados, aes(fill = Nota)) +
  geom_sf(data = sp_mapa_meso, color = "white", fill = NA) +
  scale_fill_viridis_b()

#cidade com melhor nota
IDESP_long %>% group_by(MUNICIPIO) %>% summarise(Nota = mean(Nota, na.rm = TRUE)) %>%
  arrange(desc(Nota))

# Correlacao espacial - Morgans' I
library(spdep)

w <- poly2nb(base_dados, queen = TRUE)# nao deu certo pesquisar
ww <- nb2listw(w, style = "B", zero.policy = TRUE)
ww$weights

moran.test(base_dados$Nota, ww, zero.policy = TRUE)
# correlacao estatisticamente significante e consideravel I = 0.33
#fonte dos dados: https://basedosdados.org/dataset/br-ibge-pam
library(tidyverse)
library(kableExtra)
library(sf)
library(ggspatial)

#1. Lavouras Permanentes
prod <- read.csv("C:\\Users\\NOTEBOOK CASA\\Desktop\\Analytics\\General\\Análises\\Economia\\Agricultura\\municipio_lavouras_permanentes.csv", encoding = "UTF-8")

colnames(prod)
str(prod)
head(prod)

sp <- prod %>% filter(sigla_uf == "SP")
unique(sp$id_municipio)
jaboticabal <- sp %>% filter(id_municipio == "3524303")
jaboticabal %>% group_by(produto) %>% filter(ano >= 2000) %>%
  summarise(Plantada = mean(prop_area_plantada, na.rm = TRUE), 
                                                Colhida = mean(prop_area_colhida, na.rm = TRUE),
                                                ValorProd = mean(prop_valor_producao, na.rm = TRUE),
                                                Rendimento = mean(rendimento_medio, na.rm = TRUE)) %>%
  arrange(desc(Plantada)) %>% drop_na() %>%
  kbl() %>% kable_paper()

#2. Lavouras Temporarias
prod_temp <- read.csv("C:\\Users\\NOTEBOOK CASA\\Desktop\\Analytics\\General\\Análises\\Economia\\Agricultura\\municipio_lavouras_temporarias.csv", encoding = "UTF-8")

colnames(prod_temp)
str(prod_temp)
head(prod_temp)

sp_temp <- prod_temp %>% filter(sigla_uf == "SP")

jaboticabal_temp <- sp_temp %>% filter(id_municipio == "3524303")
jaboticabal_temp %>% group_by(produto) %>% filter(ano >= 2000) %>%
  summarise(Plantada = mean(prop_area_plantada, na.rm = TRUE), 
            Colhida = mean(prop_area_colhida, na.rm = TRUE),
            ValorProd = mean(prop_valor_producao, na.rm = TRUE),
            Rendimento = mean(rendimento_medio, na.rm = TRUE)) %>%
  arrange(desc(Plantada)) %>% drop_na() %>%
  kbl() %>% kable_paper()

#3. Lavoura temporaria mais comum para cada cidade de SP ####
cidades <- prod_temp %>% filter(sigla_uf == "SP" & ano >= 2000) %>% group_by(id_municipio, produto) %>%
  summarise(Plantada = mean(prop_area_plantada, na.rm = TRUE),
            Valor = mean(prop_valor_producao, na.rm = TRUE),
            Rendimento = mean(rendimento_medio, na.rm = TRUE)) %>%
  drop_na() %>% slice(which.max(Plantada)) %>% ungroup()

cidades$id_municipio <- as.character(cidades$id_municipio)

#Elaborando um mapa
sp_mapa <- st_read('C:/Users/NOTEBOOK CASA/Desktop/Analytics/General/Análises/Sociais/Educação/sp_municipios/SP_Municipios_2019.shp')
sp_mapa_meso <- st_read('C:/Users/NOTEBOOK CASA/Desktop/Analytics/General/Análises/Sociais/Educação/sp_mesoregioes/SP_Mesorregioes_2020.shp')

base_mapa <- left_join(sp_mapa, cidades, by = c("CD_MUN" = "id_municipio")) %>% drop_na()

ggplot() +
  geom_sf(data = base_mapa, aes(fill = produto)) +
  geom_sf(data = sp_mapa_meso, color = "white", fill = NA)

#5. Lavouras (temp e perman) mais comuns para cada estado ####
brasil_temp <- prod_temp %>% filter(ano >= 2019) %>% group_by(sigla_uf, produto) %>%
  summarise(Plantada = mean(prop_area_plantada, na.rm = TRUE),
            Valor = mean(prop_valor_producao, na.rm = TRUE),
            Rendimento = mean(rendimento_medio, na.rm = TRUE)) %>%
  drop_na() %>% slice(which.max(Plantada)) %>% ungroup()

brasil_perman <- prod %>% filter(ano >= 2019) %>% group_by(sigla_uf, produto) %>%
  summarise(Plantada = mean(prop_area_plantada, na.rm = TRUE),
            Valor = mean(prop_valor_producao, na.rm = TRUE),
            Rendimento = mean(rendimento_medio, na.rm = TRUE)) %>%
  drop_na() %>% slice(which.max(Plantada)) %>% ungroup()

brasil_geral <- left_join(brasil_temp, brasil_perman, by = "sigla_uf")

uf_mapa <- st_read('C:/Users/NOTEBOOK CASA/Desktop/Analytics/General/Análises/Sociais/Educação/br_unidades_da_federacao/BR_UF_2019.shp')

#unindo a base ao shape
uf_base <- left_join(uf_mapa, brasil_geral, by = c("SIGLA_UF" = "sigla_uf"))

ggplot() +
  geom_sf(data = uf_base, aes(fill = produto.x)) +
  labs(title = "Lavouras Temporárias por UF - 2019")

ggplot() +
  geom_sf(data = uf_base, aes(fill = produto.y)) +
  labs(title = "Lavouras Permanentes por UF - 2019")




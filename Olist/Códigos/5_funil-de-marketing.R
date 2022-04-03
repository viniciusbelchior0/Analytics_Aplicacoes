# Marketing qualified leads
names(qualified_leads)
names(closed_deals)

marketing_funnel <- inner_join(closed_deals, qualified_leads, by = "mql_id")
842/8000 # conversao de apenas 10%

library(summarytools)
view(dfSummary(marketing_funnel))

marketing_funnel <- marketing_funnel %>% mutate(won_date = str_sub(won_date,1,10))
marketing_funnel$won_date <- as.Date(marketing_funnel$won_date, format = "%Y-%m-%d")
marketing_funnel$first_contact_date <- as.Date(marketing_funnel$first_contact_date, format = "%Y-%m-%d")
marketing_funnel <- marketing_funnel %>% mutate(time_acquisition = won_date - first_contact_date)
marketing_funnel$time_acquisition <- as.numeric(marketing_funnel$time_acquisition)

# principais mídias e landing pages ###
marketing_funnel %>% group_by(origin) %>%
  summarise(n = n(),avgTime = mean(time_acquisition),
            cvTime = sd(time_acquisition)/mean(time_acquisition)) %>% 
  mutate(prop = n/sum(n)) %>%
  top_n(10) %>% arrange(desc(n))
# 55% vem de buscas; outros 25% sao desconchecidos; os demais canais não trouxeram bons resultados

marketing_funnel %>% group_by(landing_page_id) %>%
  summarise(n = n()) %>% 
  mutate(prop = n/sum(n)) %>%
  arrange(desc(n))
#66% vem de 10 das 134 landing pages. Duas apresentam excelentes resultados


# tempo medio para aquisicao ###
quantile(marketing_funnel$time_acquisition, seq(0,1,0.1))
mean(marketing_funnel$time_acquisition)
median(marketing_funnel$time_acquisition)
sd(marketing_funnel$time_acquisition)/mean(marketing_funnel$time_acquisition)
# 70% das pessoas ate 40 dias; a partir disso, datas mais esparsas
# olhando pela tabela conjunta acima, search possuem um time relativamente mais elevado,
# porem as outras nao possuem proporcoes para justificar foco nelas

# informacoes referentes a empresa ###
marketing_funnel %>% group_by(business_type) %>% summarise(n = n()) %>%
  mutate(prop = n/sum(n)) %>% arrange(desc(n))
# 70% sao revendedores; outros 30% sao produtores

marketing_funnel %>% group_by(business_segment) %>% summarise(n = n()) %>%
  mutate(prop = n/sum(n)) %>% arrange(desc(n))
# segmentos bem variados; não deve-se focar em algum segmento

marketing_funnel %>% group_by(lead_type) %>% summarise(n = n()) %>%
  mutate(prop = n/sum(n)) %>% arrange(desc(n))
# foco nas online medium e big; depois nas outras

marketing_funnel %>% group_by(lead_behaviour_profile) %>%
  summarise(n = n(), avgTime = mean(time_acquisition),
            cvTime = sd(time_acquisition)/mean(time_acquisition)) %>%
  mutate(prop = n/sum(n)) %>% arrange(desc(n))
# cat, eagle(os dois sao semelhantes) e wolf (sao mais lerdos que os outros)


# as outras duas variaveis nao sao importantes para o relatorio, mas trazem
# informacoes uteis: sales representative id e sales representative developmente id


# Síntese: focar nos canais de pesquisa (organica e paga). Extender as landing pages
# de sucesso, pois algumas poucas trazem resultados significativos (**embora nao se tenha metrica
# de views, e da eficiencia da pagina para se fazer testes A/B**). Tempo medio e de 40 dias
# mais ou menos; grande maioria de revendedores, mas produtores sao expressivos.
# nao focar em segmentos especificos, mas sim em perfis especificos e tipos de loja:
# especialmente as medias e grandes online e as outras offline
# conversao de 10%
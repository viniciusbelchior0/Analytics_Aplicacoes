suppressPackageStartupMessages(library(tidyverse))
suppressPackageStartupMessages(library(sf))
library(kableExtra)


denver <- readr::read_csv('C:/Users/NOTEBOOK CASA/Desktop/denver_coffe/data/denver.csv', show_col_types = FALSE)
neighborhoods <- st_read("C:/Users/NOTEBOOK CASA/Desktop/denver_coffe/data/neighborhoods.shp", quiet=TRUE)
census <- readr::read_csv('C:/Users/NOTEBOOK CASA/Desktop/denver_coffe/data/census.csv', show_col_types = FALSE)


census_analytics <- census %>% mutate(across(4:7, ~./POPULATION_2010),
                                      HpF = NUM_HOUSEHOLDS/FAMILIES, 
                                      p100K = `NUM_HHLD_100K+`/NUM_HOUSEHOLDS)

# estimando o n de households 100k+
census_analytics_lm <- census_analytics %>% select(-c(1,2,10,11)) %>% drop_na()

lm_100k <- lm(p100K ~ ., data = census_analytics_lm)
summary(lm_100k)

# aplicando para os valores NA
census_analytics_na <- census_analytics %>% select(-c(1,10,11)) %>% 
  filter(is.na(p100K))

census_analytics_nan <- census_analytics_na %>% select(-1)

estimat <- predict(lm_100k, newdata = census_analytics_nan)


census_analytics_na$estimat <- estimat
census_analytics_na <- census_analytics_na %>% mutate(estimat = ifelse(estimat < 0, 0, estimat))

# Tabela com os dados ajustados
census_analytics <- left_join(census_analytics, census_analytics_na[,c(1,10)], by = "NBHD_NAME")
census_analytics <- census_analytics %>% mutate(`NUM_HHLD_100K+` = ifelse(is.na(`NUM_HHLD_100K+`), round(NUM_HOUSEHOLDS * estimat,0), `NUM_HHLD_100K+`)) %>% select(-c(12,13)) %>% mutate(p100k = `NUM_HHLD_100K+`/NUM_HOUSEHOLDS)

census_analytics
summary(census_analytics)

# Escolhendo as melhores localizações
best_places <- census_analytics %>% select(NBHD_ID,NBHD_NAME,AGE_18_TO_34,POPULATION_2010,p100k) %>%
  mutate(Pop = ntile(POPULATION_2010,5),Score = AGE_18_TO_34*10 + p100k*5 + (Pop/5)*2.5) %>%
  arrange(desc(Score))

best_places

# Colocando as melhores localizações no mapa
map_data <- left_join(neighborhoods, best_places[,c(-2)], by = "NBHD_ID")

# Transformando as coordenadas em shape file
denver_sf <- denver %>% st_as_sf(coords = c("Longitude","Latitude"))

# Map with the best places, with the starbucks locations
p <- map_data %>% ggplot() + geom_sf(aes(fill = Score)) + scale_fill_gradient(low = "#ffffff", high = "#f9c225") + geom_sf_text(aes(label = ifelse(Score >= 8.545526, NBHD_NAME, "")))
p <- p + geom_sf(data = denver_sf) + labs(title = "Best Places")
p

# Population by neighboord
ggplot() + geom_sf(data = map_data, aes(fill = AGE_18_TO_34)) + scale_fill_viridis_b()

# Bar chart of the best places- create a stylised table will be better
best_places %>% slice_max(Score, n = 15) %>% ggplot(aes(x = reorder(NBHD_NAME, -Score), y = Score, fill = "#f9c225")) + geom_col() + theme(axis.text.x = element_text(angle = 75, hjust= 1))


# Kable
kable(best_places[1:10,],
      col.names = c("ID", "Neighbourd Name", "Demographic Prop", "Population", "Rich","Level","Score"),
      caption = "Table 1: The best places to build a new Cofee",
      digits = 2,
      align = "lccc") %>% 
  kable_paper(font_size = 14) %>%
  row_spec(c(1,3), background = "#ECECEA") %>%
  column_spec(3, color = spec_color(best_places$AGE_18_TO_34)) %>%
  column_spec(5, color = spec_color(best_places$p100k)) %>%
  column_spec(7, color = "black",bold = TRUE)






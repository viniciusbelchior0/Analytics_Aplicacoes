library(tidyverse)
library(summarytools)
library(ggpubr)
library(ggmap)
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)
library(spData)

data <- read.csv("C:\\Users\\NOTEBOOK CASA\\Downloads\\boat_data.csv", encoding = "UTF-8")

#1. Data Cleaning ####
types <- str_split_fixed(data$Type, ",", n = 2)

df <- data %>% mutate(Cur = word(Price,1),
                        Value = as.numeric(word(Price,2)),
                        Location = word(Location,1),
                        Condition = types[,1],
                        FuelType = types[,2],
                        Years = 2022 - Year.Built) %>%
  mutate(typeBoat = case_when(Boat.Type %in% c ("Motor Yacht", "Sport Boat", "Flybridge", "Trawler",      
                                                 "Pilothouse", "Cabin Boat", "Hardtop","Center console boat",
                                                 "Bowrider", "Deck Boat") ~ Boat.Type,
                              TRUE ~ "Others"),
         Condition = case_when(Condition %in% c("new boat from stock","new boat on order") ~ "New",
                               Condition %in% c("Used boat") ~ "Used",
                               Condition == "Display Model" ~ "Display",
                               TRUE ~ Condition),
         Years = case_when(Years > 25 ~ 25,
                           TRUE ~ Years)) %>%
  mutate(FuelType = case_when(FuelType == "" & Condition %in% c("Unleaded", "Diesel","Electric") ~ Condition,
                              FuelType == "" ~ "Unknown",
                              TRUE ~ FuelType),
    Condition = case_when(Condition %in% c("Unleaded", "Diesel","Electric","") ~ "Unknown",
                               TRUE ~ Condition)) %>%
  select(-c(1,4)) %>% arrange(desc(Number.of.views.last.7.days))

#2. Analysis ####

summarytools::view(dfSummary(df))

# Variáveis para análise:
# Price: manter, mas fracionar na visualizacao 
# Boat.Type: muitas. Necessário filtrar maiores
# Manufacture: muitas. Necessária filtrar maiores
# Type: alguns, mas possivel de tentar relacionar
# Year.Built: prestar atenção nos 0
# Length e Witdh: ver se os mais populares são NAs
# Material: fazer relacao
# Location: filtrar as mais populares


#2.1 Most expensive ~ most views ##
ggplot(df) +
 aes(x = Value, y = Number.of.views.last.7.days) +
 geom_point(shape = "circle", size = 1.5, 
 colour = "#175676") +
 scale_x_continuous(trans = "log") +
 scale_y_continuous(trans = "log") +
 labs(x = "Price (Log)", 
 y = "Views (Log)", title = "Number of Views compared to Boat Prices", subtitle = "Adjusted by the currency utilized") +
 theme_light() +
 facet_wrap(vars(Cur)) +
 geom_smooth(method = "lm", se = TRUE, formula = y ~ x, color = "red", size = 1.3) +
 stat_cor()

df %>% group_by(Cur) %>% summarise(Correl = cor(Number.of.views.last.7.days, Value)) # verificando se é assim mesmo



#2.2 quantitativa variables ##
df %>% select_if(is.numeric) %>% cor(use = "complete.obs") %>% corrplot::corrplot(method = "color",
                                                                type = "lower",
                                                                addCoef.col = "black")

# 2.3 Categorical variables ##
library(FactoMineR) # categoricas
df %>% select(6,11,12,14) %>% MCA()

tab_avg <- function(x){
  df %>% group_by({x}) %>% 
    summarise(N = n(),
              Avg = mean(Number.of.views.last.7.days), 
              Med = median(Number.of.views.last.7.days), 
              CV = sd(Number.of.views.last.7.days)/mean(Number.of.views.last.7.days)) %>% 
    arrange(desc(Med), desc(N)) %>% filter(N >= 30)
}

lapply(df[,c(1,6,11,12,14)], tab_avg)
brands <- tab_avg(df$Manufacturer)
brands <- brands %>% mutate(Arank = ntile(Avg, 5), Crank = ntile(CV, 5)) %>%
          arrange(desc(Arank),Crank)
brands$Crank <- as.factor(brands$Crank)
brands <- brands %>% mutate(Level = case_when(Crank == "5" ~ "Volatile",
                                              Crank == "1" ~ "Stable",
                                              TRUE ~ "Regular"))
brands$Brand <- word(brands$`{ ... }`)

gg1 <- brands %>% arrange(desc(Avg)) %>% slice_max(Avg,n = 10) %>%
ggplot() +
  aes(x = reorder(Brand, Avg), fill = Level, weight = Avg) +
  geom_bar() +
  coord_flip() +
  scale_fill_manual(
    values = c(`Volatile` = "#D0767E",`Stable` = "#175676",`Regular` = "#B9C0CA")) +
  theme(panel.background = element_rect(fill = "white", colour = "white",size = 2, linetype = "solid"),
        panel.grid.major = element_line(size = 0.5, linetype = 'solid',colour = "white"), 
        panel.grid.minor = element_line(size = 0.25, linetype = 'solid',colour = "white"),
        legend.position = "right") + labs(title = "Brands", x = "", y = "")


models <- tab_avg(df$typeBoat)
models <- models %>% mutate(Arank = ntile(Avg, 5), Crank = ntile(CV, 5)) %>%
  slice(-1) %>%
  arrange(desc(Arank),Crank)
models$Crank <- as.factor(models$Crank)
models <- models %>% mutate(Level = case_when(Crank == "5" ~ "Volatile",
                                              Crank == "1" ~ "Stable",
                                              TRUE ~ "Regular"))
models$Model <- models$`{ ... }`

gg2 <- models %>% arrange(desc(Avg)) %>% slice_max(Avg,n = 10) %>%
  ggplot() +
  aes(x = reorder(Model, Avg), fill = Level, weight = Avg) +
  geom_bar() +
  coord_flip() +
  scale_fill_manual(
    values = c(`Volatile` = "#D0767E",`Stable` = "#175676",`Regular` = "#B9C0CA")) +
  theme(panel.background = element_rect(fill = "white", colour = "white",size = 2, linetype = "solid"),
        panel.grid.major = element_line(size = 0.5, linetype = 'solid',colour = "white"), 
        panel.grid.minor = element_line(size = 0.25, linetype = 'solid',colour = "white"),
        legend.position = "none") + labs(title = "Boat types", x = "", y = "")

gridExtra::grid.arrange(gg1,gg2, ncol = 2, top = grid::textGrob("Average Views per Post", gp = grid::gpar(fotsize = 12, font = 2)))


# Age/YearBuilt, lenght, widht, material, fueltype
# Lenght e width do not influence

df <- df %>% mutate(Rank = ntile(Number.of.views.last.7.days,5))
df <- df %>% mutate(IsHit = ifelse(Rank == 5, "Hit","No Hit"))

ggplot(df) + # there no relation between the size of the boat and the number of views: both posts have a linear relation
  aes(x = Length, y = Width) +
  geom_point(shape = "circle", size = 1.5, colour = "#112446") +
  scale_x_continuous(trans = "log") +
  scale_y_continuous(trans = "log") +
  theme_minimal() +
  facet_wrap(vars(IsHit))

df$Years <- as.factor(df$Years)
years <- tab_avg(df$Years)
years$Yearr <- years$`{ ... }`
years <- years %>% mutate(Arank = ntile(Avg, 5), Crank = ntile(CV, 5)) %>%
  arrange(desc(Arank),Crank)

years$Crank <- as.factor(years$Crank)
years <- years %>% mutate(Level = case_when(Crank == "5" ~ "Volatile",
                                              Crank == "1" ~ "Stable",
                                              TRUE ~ "Regular"))

gp1 <- years %>% arrange(desc(Avg)) %>% slice_max(Avg,n = 10) %>%
  ggplot() +
  aes(x = reorder(Yearr, Avg), fill = Level, weight = Avg) +
  geom_bar() +
  coord_flip() +
  scale_fill_manual(
    values = c(`Volatile` = "#D0767E",`Stable` = "#175676",`Regular` = "#B9C0CA")) +
  theme(panel.background = element_rect(fill = "white", colour = "white",size = 2, linetype = "solid"),
        panel.grid.major = element_line(size = 0.5, linetype = 'solid',colour = "white"), 
        panel.grid.minor = element_line(size = 0.25, linetype = 'solid',colour = "white"),
        legend.position = "right") + labs(title = "Boat Age", x = "Age of Boat (in years)", y = "")


fuels <- tab_avg(df$FuelType)
fuels$FuelType <- fuels$`{ ... }`
fuels <- fuels %>% mutate(Arank = ntile(Avg, 3), Crank = ntile(CV, 3)) %>%
  arrange(desc(Arank),Crank)

fuels$Crank <- as.factor(fuels$Crank)
fuels <- fuels %>% mutate(Level = case_when(Crank == "3" ~ "Volatile",
                                            Crank == "1" ~ "Stable",
                                            TRUE ~ "Regular"))

gp2 <- fuels %>% arrange(desc(Avg)) %>% slice_max(Avg,n = 10) %>%
  ggplot() +
  aes(x = reorder(FuelType, Avg), fill = Level, weight = Avg) +
  geom_bar() +
  scale_fill_manual(
    values = c(`Volatile` = "#D0767E",`Stable` = "#175676",`Regular` = "#B9C0CA")) +
  theme(panel.background = element_rect(fill = "white", colour = "white",size = 2, linetype = "solid"),
        panel.grid.major = element_line(size = 0.5, linetype = 'solid',colour = "white"), 
        panel.grid.minor = element_line(size = 0.25, linetype = 'solid',colour = "white"),
        legend.position = "none", axis.ticks.x = element_blank(), axis.text.x = element_blank()) +
  labs(title = "Fuel Type", x = "", y = "")


#2.4 Views by country ##
tab <- df %>% group_by(Location) %>% summarise(Posts = n(),
                                               TotalViews = sum(Number.of.views.last.7.days),
                                               AvgViews = mean(Number.of.views.last.7.days),
                                               MedViews = median(Number.of.views.last.7.days),
                                               CvViews = sd(Number.of.views.last.7.days)/mean(Number.of.views.last.7.days)) %>%
  arrange(desc(MedViews), desc(Posts)) %>%
  filter(Posts >= 75)
tab <- tab %>% mutate(CRank = ntile(CvViews, 5))

world <- ne_countries(scale = "medium", returnclass = "sf")
world_points <- cbind(spData::world, st_coordinates(st_centroid(spData::world$geom)))
names(world_points)[names(world_points) == "name_long"] <- "name"
world_points <- world_points %>% filter(name %in% c("Switzerland","Austria", "Germany","Denmark","Netherlands","Spain",    
                                                     "Italy", "Croatia", "Portugal" ,"Greece", "France"))

world <- left_join(world, tab, by = c("sovereignt" = "Location"))
world_focus <- world %>% filter(sovereignt %in% c("Switzerland","Austria", "Germany","Denmark","Netherlands","Spain",    
                                                  "Italy", "Croatia", "Portugal" ,"Greece", "France"))

ggplot(data = world) +
  geom_sf(fill = "green") +
  geom_sf(aes(fill = AvgViews)) +
  scale_fill_gradient(low = "#DAE3F3", high = "#2F5597") +
  geom_text(data= world_points,aes(x=X, y=Y, label=name), #to print the names of countries
            color = "black", check_overlap = FALSE, family = "sans", fontface = "bold",
            size =9 / .pt) +
  coord_sf(xlim = c(-20.0, 45.0), ylim = c(30.0, 73.0), expand = FALSE) +
  theme(panel.background = element_rect(fill = "#FCFCFC"), legend.position = "none",
        axis.ticks = element_blank(),axis.text = element_blank(),
        axis.title = element_blank(),
        plot.title = element_text(size = 22, colour = "#203864", face = "bold"),
        plot.subtitle = element_text(size = 10, colour = "#545050", face = "bold"),
        legend.title = element_text(colour = "black", face = "bold", size = 10)) +
  labs(title = "", subtitle = "") +
  guides(fill = guide_legend(title = "Avg.Views/Post"))


# LINK for reference in creating maps
# https://r-spatial.org/r/2018/10/25/ggplot2-sf.html
library(tidyverse)
library(readxl)

transactions_retail <- read_excel("transactions_retail.xlsx", 
                                  col_types = c("numeric", "text", "text", 
                                                "numeric", "date", "numeric", "numeric", 
                                                "text", "numeric", "text", "text"))


# Obtendo a quantidade vendidade e os preços (base de dados)
produtos <- transactions_retail %>% filter(StockCode %in% c("22423", "47566", "84879", "20725",
                                                            "22720", "23203", "22197", "23298",
                                                            "22086", "20727")) %>%
  select(StockCode, Quantity, UnitPrice) %>% distinct()

# Calculando as funções de demanda

pr_1 <- produtos %>% filter(StockCode == "22423")
pr_2 <- produtos %>% filter(StockCode == "47566")
pr_3 <- produtos %>% filter(StockCode == "84879")
pr_4 <- produtos %>% filter(StockCode == "20725")
pr_5 <- produtos %>% filter(StockCode == "22720")
pr_6 <- produtos %>% filter(StockCode == "23203")
pr_7 <- produtos %>% filter(StockCode == "22197")
pr_8 <- produtos %>% filter(StockCode == "23298")
pr_9 <- produtos %>% filter(StockCode == "22086")
pr_10 <- produtos %>% filter(StockCode == "20727")

el_1 <- lm(log(Quantity) ~ log(UnitPrice), data = pr_1)
el_2 <- lm(log(Quantity) ~ log(UnitPrice), data = pr_2)
el_3 <- lm(log(Quantity) ~ log(UnitPrice), data = pr_3)
el_4 <- lm(log(Quantity) ~ log(UnitPrice), data = pr_4)
el_5 <- lm(log(Quantity) ~ log(UnitPrice), data = pr_5)
el_6 <- lm(log(Quantity) ~ log(UnitPrice), data = pr_6)
el_7 <- lm(log(Quantity) ~ log(UnitPrice), data = pr_7)
el_8 <- lm(log(Quantity) ~ log(UnitPrice), data = pr_8)
el_9 <- lm(log(Quantity) ~ log(UnitPrice), data = pr_9)
el_10 <- lm(log(Quantity) ~ log(UnitPrice), data = pr_10)


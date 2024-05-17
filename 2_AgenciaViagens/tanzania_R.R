library(tidyverse)
library(car)
library(asbio)
library(MASS)
options(scipen = 999, digits = 2)

tanzania <- read.csv("tanzania_train.csv")

# Analisando de maneira rápida o comportamento das variáveis e seus tipos
library(summarytools)
summarytools::view(dfSummary(tanzania))

#1. Data Cleaning ####
str(tanzania)
summary(tanzania)

tanzania <- tanzania %>% mutate(total_female = replace_na(total_female,0),
                                total_male = replace_na(total_male, 0)) #corrigindo os NAs nas variaveis quantitativas e substituindo por 0

# resolvendo os dados faltantes nas variavies categoricas --> criar nova categoria 'Unknown'-, pois nao e interessante tentar fazer imputing
tanzania <- mutate_all(tanzania,list(~na_if(.,""))) #transformando empty strings em NAs

tanzania <- tanzania %>% mutate(travel_with = replace_na(travel_with,"Unknown"),
                                most_impressing = replace_na(most_impressing, "Unknown"))

# Ajustando o nome dos paises + feature engineering: relacionando com o continente e o subcontinente

countries <- read.csv2("countries.csv") #carregando a nova base

tanzania <- left_join(tanzania, countries, by = c("country" = "ï..country"))

# criando novas variáveis
tanzania <- tanzania %>% mutate(total_people = total_male + total_female,
                                total_night = night_mainland + night_zanzibar,
                                total_cost_adj = total_cost/1000,
                                propFemale = total_female/total_people,
                                propMainland = night_mainland/total_night,
                                cost_capita = total_cost/total_people)

tanzania <- tanzania %>% filter(total_people > 0)

# encontrando outliers
summary(tanzania)
quantile(tanzania$total_people, seq(0,1,0.01)) #out >10
quantile(tanzania$total_night, seq(0,1,0.01)) #out >60 ou >36
quantile(tanzania$total_cost, seq(0,1,0.01)) #out >58042335 (talvez usar 1,5*iqr)
# encontrar formulas de obter outliers (mahlnobis), clusterizacao hierarquica

tanzania$mahalnobis <- mahalanobis(tanzania[,c(23,27,28)], colMeans(tanzania[,c(23,27,28)]), cov(tanzania[,c(23,27,28)]))
tanzania$pvalue <- pchisq(tanzania$mahalnobis, df=2, lower.tail = FALSE) #valores menores que 0.001 sao considrados outliers
tanzania_outliers <- tanzania %>% filter(pvalue < 0.001)

summary(tanzania)

tanzania_clean <- tanzania %>% filter(pvalue >= 0.001) %>% dplyr::select(-c(33,34))


# 2. Modelando com Regressão Linear ####

#Vamos modelar as vendas com base em um modelo de regressão linear

##2.1 - Ajustando a base de dados


names(tanzania)

tanzania_train <- tanzania %>% dplyr::select(3,4,27,30,7,8,9,10,28,31,20,21,22,25,23)
tanzania_train_clean <- tanzania_clean %>% dplyr::select(3,4,27,30,7,8,9,10,28,31,20,21,22,25,23)

head(tanzania_train)

## 2.2 - Regressão Linear"""

reg_basica <- lm(total_cost ~., data = tanzania_train)
reg_basica_clean <- lm(total_cost ~., data = tanzania_train_clean)
summary(reg_basica)

#multicolinearidade
ld.vars <- attributes(alias(reg_basica)$Complete)$dimnames[[1]] #resolvendo a multicolinearidade
ld.vars

par(mfrow = c(2,2))
plot(reg_basica)
layout(1)
car::vif(reg_basica)

hatvalues(reg_basica) #valores de alavancagem
im <- influence.measures(reg_basica) #medidas de influencia
summary(im)

MASS::boxcox(reg_basica) #transformacao box cox
abline(v = 0.15, col = "red") #testar raiz cubica e logartimica

# Outros modelos
reglogat <- lm(log(total_cost) ~., data = tanzania_train)
reglogat_clean <- lm(log(total_cost)~., data = tanzania_train_clean)



# medidas de ajuste - comparacao entre modelos
modl <- list(m0 = reg_basica, m1 = reglogat, m2 = reg_basica_clean, m3 = reglogat_clean)

asbio::lm.select(modl) %>%
  mutate_if(is.numeric, round, digits = 2)

measures <- function(x) {
  L <- list(npar = length(coef(x)),
            dfres = df.residual(x),
            nobs = length(fitted(x)),
            RMSE = summary(x)$sigma,
            R2 = summary(x)$r.squared,
            R2adj = summary(x)$adj.r.squared,
            PRESS = press(x),
            logLik = logLik(x),
            AIC = AIC(x),
            BIC = BIC(x))
  unlist(L)
}

t(sapply(modl, measures)) %>%
  as.data.frame() %>%
  mutate_if(is.numeric, round, digits = 3)

# Sumarisando o melhor modelo
summary(reglogat_clean)

a <- step(reglogat_clean, direction = "both")

coeficientes_regressao <- reglogat_clean$coefficients

write.csv2(coeficientes_regressao, "coef_reg.csv")


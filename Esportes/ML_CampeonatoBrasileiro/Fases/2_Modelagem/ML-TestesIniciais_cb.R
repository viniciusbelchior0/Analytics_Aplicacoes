library(tidyverse)
library(caret)
library(rpart)
library(e1071)
library(randomForest)

# Tipo 1 - Geral ####
#geral_1 <- read.csv("C:\\Users\\NOTEBOOK CASA\\Desktop\\CampeonatoBrasileiro\\Dados\\df_SerieA_1.csv")
geral_3 <- read.csv("C:\\Users\\NOTEBOOK CASA\\Desktop\\CampeonatoBrasileiro\\Dados\\df_SerieA_3.csv")
geral_5 <- read.csv("C:\\Users\\NOTEBOOK CASA\\Desktop\\CampeonatoBrasileiro\\Dados\\df_SerieA_5.csv")
#geral_7 <- read.csv("C:\\Users\\NOTEBOOK CASA\\Desktop\\CampeonatoBrasileiro\\Dados\\df_SerieA_7.csv")
geral_10 <- read.csv("C:\\Users\\NOTEBOOK CASA\\Desktop\\CampeonatoBrasileiro\\Dados\\df_SerieA_10.csv")

geral_3 <- geral_3 %>% select(10:28,32:50,9)
geral_3$resultado <- as.factor(geral_3$resultado)
samples_3 <- geral_3$resultado %>% createDataPartition(p = 0.75, list = FALSE)
train_data_3 <- geral_3[samples_3,]
test_data_3 <- geral_3[-samples_3,]

geral_5 <- geral_5 %>% select(10:28,32:50,9)
geral_5$resultado <- as.factor(geral_5$resultado)
samples_5 <- geral_5$resultado %>% createDataPartition(p = 0.75, list = FALSE)
train_data_5 <- geral_5[samples_5,]
test_data_5 <- geral_5[-samples_5,]

geral_10 <- geral_10 %>% select(10:28,32:50,9)
geral_10$resultado <- as.factor(geral_10$resultado)
samples_10 <- geral_10$resultado %>% createDataPartition(p = 0.75, list = FALSE)
train_data_10 <- geral_10[samples_10,]
test_data_10 <- geral_10[-samples_10,]

# SVM
clas3 <- svm(resultado ~., data = train_data_3, kernel = "linear", type = "C-classification")
pred3 <- predict(clas3, newdata = test_data_3[,-40])
confusionMatrix(test_data_3$resultado,pred3)

clas5 <- svm(resultado ~., data = train_data_5, kernel = "linear", type = "C-classification")
pred5 <- predict(clas5, newdata = test_data_5[,-40])
confusionMatrix(test_data_5$resultado,pred5)

clas10 <- svm(resultado ~., data = train_data_10, kernel = "linear", type = "C-classification")
pred10 <- predict(clas10, newdata = test_data_10[,-40])
confusionMatrix(test_data_10$resultado,pred10)

# RandomForest
rf3 <- randomForest(resultado ~., data = train_data_3, ntree = 100)
predrf3 <- predict(rf3, newdata = test_data_3[,-40])
confusionMatrix(test_data_3$resultado, predrf3)

rf5 <- randomForest(resultado ~., data = train_data_5, ntree = 100)
predrf5 <- predict(rf5, newdata = test_data_5[,-40])
confusionMatrix(test_data_5$resultado, predrf5)

rf10 <- randomForest(resultado ~., data = train_data_10, ntree = 100)
predrf10 <- predict(rf10, newdata = test_data_10[,-40])
confusionMatrix(test_data_10$resultado, predrf10)


# Tipo 2 - Subtracao ####
subt_1 <- read.csv("C:\\Users\\NOTEBOOK CASA\\Desktop\\CampeonatoBrasileiro\\Dados\\df_SerieA_Sub_1.csv")
subt_3 <- read.csv("C:\\Users\\NOTEBOOK CASA\\Desktop\\CampeonatoBrasileiro\\Dados\\df_SerieA_Sub_3.csv")
subt_5 <- read.csv("C:\\Users\\NOTEBOOK CASA\\Desktop\\CampeonatoBrasileiro\\Dados\\df_SerieA_Sub_5.csv")
subt_7 <- read.csv("C:\\Users\\NOTEBOOK CASA\\Desktop\\CampeonatoBrasileiro\\Dados\\df_SerieA_Sub_7.csv")
subt_10 <- read.csv("C:\\Users\\NOTEBOOK CASA\\Desktop\\CampeonatoBrasileiro\\Dados\\df_SerieA_Sub_10.csv")

subt_1 <- subt_1 %>% select(10:28,9)
subt_1$resultado <- as.factor(subt_1$resultado)
samples_1 <- subt_1$resultado %>% createDataPartition(p = 0.75, list = FALSE)
train_data_1 <- subt_1[samples_1,]
test_data_1 <- subt_1[-samples_1,]

subt_3 <- subt_3 %>% select(10:28,9)
subt_3$resultado <- as.factor(subt_3$resultado)
samples_3 <- subt_3$resultado %>% createDataPartition(p = 0.75, list = FALSE)
train_data_3 <- subt_3[samples_3,]
test_data_3 <- subt_3[-samples_3,]

subt_5 <- subt_5 %>% select(10:28,9)
subt_5$resultado <- as.factor(subt_5$resultado)
samples_5 <- subt_5$resultado %>% createDataPartition(p = 0.75, list = FALSE)
train_data_5 <- subt_5[samples_5,]
test_data_5 <- subt_5[-samples_5,]

subt_10 <- subt_10 %>% select(10:28,9)
subt_10$resultado <- as.factor(subt_10$resultado)
samples_10 <- subt_10$resultado %>% createDataPartition(p = 0.75, list = FALSE)
train_data_10 <- subt_10[samples_10,]
test_data_10 <- subt_10[-samples_10,]

# SVM
clas3 <- svm(resultado ~., data = train_data_3, kernel = "linear", type = "C-classification")
pred3 <- predict(clas3, newdata = test_data_3[,-20])
confusionMatrix(test_data_3$resultado,pred3)

clas5 <- svm(resultado ~., data = train_data_5, kernel = "linear", type = "C-classification")
pred5 <- predict(clas5, newdata = test_data_5[,-20])
confusionMatrix(test_data_5$resultado,pred5)

clas10 <- svm(resultado ~., data = train_data_10, kernel = "linear", type = "C-classification")
pred10 <- predict(clas10, newdata = test_data_10[,-20])
confusionMatrix(test_data_10$resultado,pred10)

# RandomForest
rf3 <- randomForest(resultado ~., data = train_data_3, ntree = 100)
predrf3 <- predict(rf3, newdata = test_data_3[,-20])
confusionMatrix(test_data_3$resultado, predrf3)

rf5 <- randomForest(resultado ~., data = train_data_5, ntree = 100)
predrf5 <- predict(rf5, newdata = test_data_5[,-20])
confusionMatrix(test_data_5$resultado, predrf5)

rf10 <- randomForest(resultado ~., data = train_data_10, ntree = 100)
predrf10 <- predict(rf10, newdata = test_data_10[,-20])
confusionMatrix(test_data_10$resultado, predrf10)


# Tipo 3 - Gerais com dois rotulos####
#geral_1 <- read.csv("C:\\Users\\NOTEBOOK CASA\\Desktop\\CampeonatoBrasileiro\\Dados\\df_SerieA_1.csv")
geral_3 <- read.csv("C:\\Users\\NOTEBOOK CASA\\Desktop\\CampeonatoBrasileiro\\Dados\\df_SerieA_3.csv")
geral_5 <- read.csv("C:\\Users\\NOTEBOOK CASA\\Desktop\\CampeonatoBrasileiro\\Dados\\df_SerieA_5.csv")
#geral_7 <- read.csv("C:\\Users\\NOTEBOOK CASA\\Desktop\\CampeonatoBrasileiro\\Dados\\df_SerieA_7.csv")
geral_10 <- read.csv("C:\\Users\\NOTEBOOK CASA\\Desktop\\CampeonatoBrasileiro\\Dados\\df_SerieA_10.csv")

geral_3 <- geral_3 %>% select(10:28,32:50,9)
geral_3$resultado <- ifelse(geral_3$resultado == "V",1,0)
geral_3$resultado <- as.factor(geral_3$resultado)
samples_3 <- geral_3$resultado %>% createDataPartition(p = 0.75, list = FALSE)
train_data_3 <- geral_3[samples_3,]
test_data_3 <- geral_3[-samples_3,]

geral_5 <- geral_5 %>% select(10:28,32:50,9)
geral_5$resultado <- ifelse(geral_5$resultado == "V",1,0)
geral_5$resultado <- as.factor(geral_5$resultado)
samples_5 <- geral_5$resultado %>% createDataPartition(p = 0.75, list = FALSE)
train_data_5 <- geral_5[samples_5,]
test_data_5 <- geral_5[-samples_5,]

geral_10 <- geral_10 %>% select(10:28,32:50,9)
geral_10$resultado <- ifelse(geral_10$resultado == "V",1,0)
geral_10$resultado <- as.factor(geral_10$resultado)
samples_10 <- geral_10$resultado %>% createDataPartition(p = 0.75, list = FALSE)
train_data_10 <- geral_10[samples_10,]
test_data_10 <- geral_10[-samples_10,]

# SVM
clas3 <- svm(resultado ~., data = train_data_3, kernel = "linear", type = "C-classification")
pred3 <- predict(clas3, newdata = test_data_3[,-39])
confusionMatrix(test_data_3$resultado,pred3)

clas5 <- svm(resultado ~., data = train_data_5, kernel = "linear", type = "C-classification")
pred5 <- predict(clas5, newdata = test_data_5[,-39])
confusionMatrix(test_data_5$resultado,pred5)

clas10 <- svm(resultado ~., data = train_data_10, kernel = "linear", type = "C-classification")
pred10 <- predict(clas10, newdata = test_data_10[,-39])
confusionMatrix(test_data_10$resultado,pred10)

# RandomForest
rf3 <- randomForest(resultado ~., data = train_data_3, ntree = 100)
predrf3 <- predict(rf3, newdata = test_data_3[,-39])
confusionMatrix(test_data_3$resultado, predrf3)

rf5 <- randomForest(resultado ~., data = train_data_5, ntree = 100)
predrf5 <- predict(rf5, newdata = test_data_5[,-39])
confusionMatrix(test_data_5$resultado, predrf5)

rf10 <- randomForest(resultado ~., data = train_data_10, ntree = 100)
predrf10 <- predict(rf10, newdata = test_data_10[,-39])
confusionMatrix(test_data_10$resultado, predrf10)

# CONCLUSOES ####

# o tipo de 1 data contem data leakage
# os modelos gerais obtiveram melhor desempenho
# Rodadas = 3>5>10>7
# o melhor modelo é Random Forest geral para 3 rodadas; depois SVM geral para 3 rodadas

# em relacao a 2 rotulos, media de 80% para svm e random forest 3 rodadas geral
# random forest pode ainda ir a 84% 

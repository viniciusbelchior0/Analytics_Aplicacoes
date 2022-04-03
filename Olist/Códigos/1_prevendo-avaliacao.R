# Prevendo a nota da pessoa
library(caret)
library(randomForest)
library(e1071)

olist_model <- olist_df %>% select(-c(1,2,4,5,6,7,8,10:13,14,15,16))
str(olist_model)
olist_model$review_score <- as.factor(olist_model$review_score)
olist_model <- olist_model %>% filter(order_status == "delivered") %>% drop_na()
olist_model$order_status <- NULL
olist_model <- olist_model %>% select(2:11,1)
summary(olist_model)


# Tipo 1 - sem downsampling
samples <- createDataPartition(olist_model$review_score, p=0.75, list= FALSE)
train_data <- olist_model[samples,]
test_data <- olist_model[-samples,]


cla1 <- randomForest(review_score ~., data = train_data, ntree = 100)
pred1 <- predict(cla1, newdata= test_data[,-11])
confusionMatrix(pred1,test_data[,11])
# accuracy: 0.61

# Tipo 2 -downsampling
olist_model <- downSample(olist_model[,-11],olist_model$review_score)
samples <- createDataPartition(olist_model$Class, p=0.75, list= FALSE)
train_data <- olist_model[samples,]
test_data <- olist_model[-samples,]


cla2 <- randomForest(Class ~., data = train_data, ntree = 100)
pred2 <- predict(cla2, newdata= test_data[,-11])
confusionMatrix(pred2,test_data[,11])


# Tipo 3 - Remodelar as reviews e downsampling
olist_model <- olist_model %>% mutate(review_score = ifelse(review_score %in% c(4,5),'Positiva',
                                                            ifelse(review_score %in% c(1,2),'Negativa','Neutra')))
olist_model$review_score <- as.factor(olist_model$review_score)
olist_model <- downSample(olist_model[,-11],olist_model$review_score)
samples <- createDataPartition(olist_model$Class, p=0.75, list= FALSE)
train_data <- olist_model[samples,]
test_data <- olist_model[-samples,]

cla3 <- randomForest(Class ~., data = train_data, ntree = 100)
pred3 <- predict(cla3, newdata= test_data[,-11])
confusionMatrix(pred3,test_data[,11])

# Tipo 4 - remodelar binaria e downsampling
olist_model <- olist_model %>% mutate(review_score = ifelse(review_score %in% c(4,5),'1','0'))
olist_model$review_score <- as.factor(olist_model$review_score)
olist_model <- downSample(olist_model[,-11],olist_model$review_score)
samples <- createDataPartition(olist_model$Class, p=0.75, list= FALSE)
train_data <- olist_model[samples,]
test_data <- olist_model[-samples,]

cla3 <- randomForest(Class ~., data = train_data, ntree = 100)
pred3 <- predict(cla3, newdata= test_data[,-11])
confusionMatrix(pred3,test_data[,11])


# dados coletados nao parecem ser suficientes para uma predicao, mesmo corrigindo os 
# problemas de oversampling e muitos rótulos
# talvez texto seja melhor para dar um norte
#2 - Analisando os comentarios das avaliacoes
# COmo o objetivo é apenas ler comentarios, apenas essas colunas serao utilizadas,
# juntamente com a nota da avaliacao
library(tidyverse)
library(tidytext)
library(igraph)
library(ggraph)
library(topicmodels)
library(stopwords)
library(lexiconPT)

comentarios <- olist_df %>% select(review_id, review_score, review_comment_message,
                                   order_status)

# Obtendo as stopwords e os lexicos em Portugues
pt_stopwords <- as.data.frame(stopwords(language="pt", source= "stopwords-iso")) # também existe o snowball, porem com menos palavras
colnames(pt_stopwords) <- c("word")
oplexicon_3 <- oplexicon_v3.0

#1. Comentarios ###
comentarios_token <- comentarios %>% unnest_tokens(word, review_comment_message) %>%
  anti_join(pt_stopwords, by = "word") %>%
  left_join(oplexicon_3, by = c("word" = "term"))

comentarios_bigrams <- comentarios %>% 
  unnest_tokens(bigram, review_comment_message, token="ngrams",n =2)

comentarios_separated <- comentarios_bigrams %>%
  separate(bigram, c("word1","word2"), sep = " ") %>%
  filter(!word1 %in% pt_stopwords$word) %>%
  filter(!word2 %in% pt_stopwords$word)

comentarios_count <- comentarios_separated %>% dplyr::count(word1, word2, sort= TRUE) %>%
  na.omit()

# Grafos com base nos bigrams
comentarios_graph <- comentarios_count %>%
  filter(n >= 100) %>%
  graph_from_data_frame()

set.seed(1153)
a <- grid::arrow(type = "closed", length = unit(.15,"inches"))

ggraph(comentarios_graph, layout = "fr") +
  geom_edge_link(aes(edge_alpha = n), show.legend = FALSE,
                 arrow = a, end_cap = circle(.07,'inches')) +
  geom_node_point(color = "lightblue", size = 5) +
  geom_node_text(aes(label = name), vjust = 1, hjust = 1) +
  theme_void()

# Topic Modelling
#necessario reduzir amostragem - equilibrar classes de avaliacoes
comentarios_token$review_score <- as.factor(comentarios_token$review_score)

comentarios_dtm <- comentarios_token %>%
  dplyr::count(word, review_score) %>%
  cast_dtm(review_score, word, n) %>%
  as.matrix()

comentarios_dtm[1:4, 751:755]

lda_out <- LDA(comentarios_dtm,
               k = 4,
               method = "Gibbs",
               control = list(seed = 42)) %>%
  tidy(matrix = "beta")

lda_out %>% arrange(desc(beta))

lda_out %>%
  group_by(topic) %>%
  top_n(15, beta) %>%
  ungroup() %>%
  mutate(term2 = fct_reorder(term, beta)) %>%
  ggplot(aes(term2,beta,fill = as.factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~topic, scales = "free") + coord_flip()

# Analisando a polaridade das palavras
tab1 <- comentarios_token %>% drop_na() %>%
  group_by(review_id) %>% summarise(Palavras = n(),
                                    Polarity = sum(polarity),
                                    vb = sum(type == "vb"),
                                    adj = sum(type == "adj"),
                                    Score = mean(review_score)) %>%
  arrange(desc(Palavras),desc(Polarity))

tab1$Score <- as.factor(tab1$Score)

ggplot(tab1) +
 aes(x = Score, y = Polarity) +
 geom_boxplot(shape = "circle", fill = "#4682B4") +
 theme_gray()


#palavras mais comuns por grupo
tab2 <- comentarios_token %>% drop_na() %>%
  group_by(review_score) %>% dplyr::count(word, sort=TRUE) %>%
  top_n(15, n) %>%
  ungroup() %>%
  mutate(word2 = fct_reorder(word,n))

tab2$review_score <- as.factor(tab2$review_score)

ggplot(tab2, aes(x = word2, y = n, fill = review_score)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ review_score, scales = "free") +
  coord_flip() +
  labs(title =  "Re", x = "DF")


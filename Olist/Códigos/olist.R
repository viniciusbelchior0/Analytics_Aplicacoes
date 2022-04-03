library(tidyverse)
library(sqldf)

# abrindo os arquivos
customers <- read.csv("C:\\Users\\NOTEBOOK CASA\\Desktop\\Olist\\2. Brazilian E-commerce Olist\\archive\\olist_customers_dataset.csv", encoding = "UTF-8")
geolocation <- read.csv("C:\\Users\\NOTEBOOK CASA\\Desktop\\Olist\\2. Brazilian E-commerce Olist\\archive\\olist_geolocation_dataset.csv", encoding = "UTF-8")
order_itens <- read.csv("C:\\Users\\NOTEBOOK CASA\\Desktop\\Olist\\2. Brazilian E-commerce Olist\\archive\\olist_order_items_dataset.csv", encoding = "UTF-8")
order_payments <- read.csv("C:\\Users\\NOTEBOOK CASA\\Desktop\\Olist\\2. Brazilian E-commerce Olist\\archive\\olist_order_payments_dataset.csv", encoding = "UTF-8")
order_reviews <- read.csv("C:\\Users\\NOTEBOOK CASA\\Desktop\\Olist\\2. Brazilian E-commerce Olist\\archive\\olist_order_reviews_dataset.csv", encoding = "UTF-8")
orders <- read.csv("C:\\Users\\NOTEBOOK CASA\\Desktop\\Olist\\2. Brazilian E-commerce Olist\\archive\\olist_orders_dataset.csv", encoding = "UTF-8")
products <- read.csv("C:\\Users\\NOTEBOOK CASA\\Desktop\\Olist\\2. Brazilian E-commerce Olist\\archive\\olist_products_dataset.csv", encoding = "UTF-8")
sellers <- read.csv("C:\\Users\\NOTEBOOK CASA\\Desktop\\Olist\\2. Brazilian E-commerce Olist\\archive\\olist_sellers_dataset.csv", encoding = "UTF-8")

# funil de marketing (analisado de maneira isolada; join nao traz informacoes complementares aos problemas acima)
closed_deals <- read.csv("C:\\Users\\NOTEBOOK CASA\\Desktop\\Olist\\2. Brazilian E-commerce Olist\\archive\\olist_closed_deals_dataset.csv", encoding = "UTF-8")
qualified_leads <- read.csv("C:\\Users\\NOTEBOOK CASA\\Desktop\\Olist\\2. Brazilian E-commerce Olist\\archive\\olist_marketing_qualified_leads_dataset.csv", encoding = "UTF-8")


# ajustando tabelas que necessitam de tratamento
order_payments <- order_payments %>% group_by(order_id) %>%
  summarise(payment_sequential = n(), payment_types = n_distinct(payment_type),
            payment_installments= sum(payment_installments), payment_value = sum(payment_value))

aaa <- inner_join(orders, order_reviews, by = "order_id")
bbb <- inner_join(aaa, order_itens, by = "order_id")
sellers_score <- left_join(bbb, sellers, by = "seller_id") %>%
  select(order_id, review_id, review_score, seller_id) %>%
  group_by(seller_id) %>% summarise(n = n(), AvgReview = mean(review_score)) %>%
  arrange(desc(n))

sellers <- left_join(sellers, sellers_score, by ="seller_id")

# fazendo as queries
reviews_adj <- sqldf("SELECT review_id, orev.order_id,review_score,review_comment_title,
               review_comment_message, review_creation_date, review_answer_timestamp,
               ord.customer_id, order_status, order_delivered_carrier_date, order_delivered_customer_date,
               order_estimated_delivery_date, customer_unique_id,customer_city, customer_state,
               payment_sequential, payment_types, payment_installments, payment_value
               FROM order_reviews AS orev
               LEFT JOIN orders AS ord ON ord.order_id = orev.order_id
               LEFT JOIN customers AS ctm ON ctm.customer_id = ord.customer_id
               LEFT JOIN order_payments AS opm ON opm.order_id = ord.order_id")

itens_adj <- sqldf("SELECT itn.order_id,
                COUNT(*) AS n_products,
                SUM(itn.price) AS price,
                SUM(itn.freight_value) AS freight_value,
                COUNT(DISTINCT prod.product_category_name) AS n_categories,
                AVG(sel.AvgReview) AS avg_seller_score
                FROM order_itens AS itn
                LEFT JOIN products AS prod ON prod.product_id = itn.product_id
                LEFT JOIN sellers AS sel ON sel.seller_id = itn.seller_id
                GROUP BY itn.order_id")

olist_df <- sqldf("SELECT review_id, rev.order_id, review_score, review_comment_title,
                   review_comment_message, review_creation_date, review_answer_timestamp,
                   customer_id, order_status, order_delivered_carrier_date,
                   order_delivered_customer_date, order_estimated_delivery_date,
                   customer_unique_id, customer_city, customer_state,
                   payment_sequential, payment_types, payment_installments, payment_value,
                   n_products, price, freight_value, n_categories, avg_seller_score
                   FROM reviews_adj AS rev
                   LEFT JOIN itens_adj AS itn ON itn.order_id = rev.order_id")

olist_df <- olist_df %>% mutate(order_delivered_carrier_date = str_sub(order_delivered_carrier_date,1,10),
                                order_delivered_customer_date = str_sub(order_delivered_customer_date,1,10),
                                order_estimated_delivery_date = str_sub(order_estimated_delivery_date, 1,10))

olist_df[,10:12] <- lapply(olist_df[,10:12], function(x){as.Date(x, format= "%Y-%m-%d")})

olist_df <- olist_df %>% mutate(days_delivered = order_delivered_customer_date - order_delivered_carrier_date,
                                days_estimated_diff = order_estimated_delivery_date - order_delivered_customer_date)

olist_df[,25:26] <- lapply(olist_df[,25:26], function(x){as.numeric(x)})

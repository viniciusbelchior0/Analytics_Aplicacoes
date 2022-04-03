#4 - clusterizacao ####
olist_df %>% group_by(customer_unique_id) %>%
  summarise(Orders = n_distinct(order_id), )

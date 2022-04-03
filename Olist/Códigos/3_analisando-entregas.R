#3 - Melhorar entrega ####
entregues <- olist_df %>% filter(order_status == "delivered") %>%
  drop_na(order_status)

entregues %>% filter(days_estimated_diff < 0) %>% count()
6562/97015 # 6% foram entregues atrasados


tab_estados <- entregues %>% group_by(customer_state) %>%
  summarise(Orders = n_distinct(order_id), AvgReview = mean(review_score),
            Customers = n_distinct(customer_unique_id), Cities = n_distinct(customer_city),
            TotalProducts = sum(n_products), TotalValue = sum(payment_value, na.rm = TRUE),
            AvgFreightValue = mean(freight_value), AvgDaysD = mean(days_delivered, na.rm = TRUE),
            CVDaysD = sd(days_delivered, na.rm = TRUE)/mean(days_delivered, na.rm = TRUE),
            AvgDiff = mean(days_estimated_diff, na.rm = TRUE),
            CVDiff = sd(days_estimated_diff, na.rm = TRUE)/mean(days_estimated_diff, na.rm = TRUE)) %>%
  arrange(desc(Orders)) # fazer aquele grafico da caixinha com intervalos

options(repr.plot.width = 10, repr.plot.height = 10)

cut_quantiles <- function(x, q) {
  x <- sort(x)
  i <- order(x)
  x[i[i >= quantile(i, min(q)) & i <= quantile(i, max(q))]]
}

entregues %>%
  filter(!is.na(days_delivered)) %>%
  group_by(customer_state) %>%
  summarise(mean_days = mean(days_delivered), 
            min = first(cut_quantiles(days_delivered, c(0.05, 0.95))), 
            max = last(cut_quantiles(days_delivered, c(0.05, 0.95))), 
            n = n()) %>%
  ungroup() %>%
  mutate(customer_state = fct_reorder(customer_state, mean_days)) %>%
  ggplot(aes(x = mean_days, y = customer_state)) + 
  geom_point(aes(size = n), alpha = 0.5) + 
  geom_errorbarh(aes(xmin = min, xmax = max), alpha = 0.3) + 
  labs(x = "Média em dias para entrega\n(e os limites de 95% dos preços)", y = "Estado", 
       title = "Tempo média para entrega dos pedidos por UF") + 
  scale_size_continuous(name = "Número de pedidos") + 
  theme_light()

# para monitoramento: grafico de controle da qualidade (5 e 95% como limites)
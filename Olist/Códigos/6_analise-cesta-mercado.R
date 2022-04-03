# Marketing Basket Analysis
library(arules)
library(arulesViz)

products <- products %>% select(1,2)
order_itens <- order_itens %>% select(1,3) %>%
  left_join(products, by = "product_id")

AggPosData <- split(order_itens$product_id, order_itens$order_id)
Txns <- as(AggPosData, 'transactions')

Rules <- apriori(Txns)

itemFrequencyPlot(Txns, topN = 10)

plot(Rules, method = "scatter", engine = "htmlwidget", max = 150)#scatterplot
plot(Rules, method = "graph", engine = "htmlwidget", max = 50)#grafo
plot(Rules, method = "grouped", measure = "lift", shading = "confidence")#agrupado


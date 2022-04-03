WITH tb_payments AS (
        SELECT order_id,MAX(payment_sequential) AS payment_sequentials,
        payment_type, SUM(payment_installments) as payment_installments,
        SUM(payment_value) AS payment_value
        FROM order_payments
        GROUP BY order_id),

tb_orders AS (
        SELECT t1.*,
        t2.customer_id, t2.order_status,t2.order_approved_at,
        t2.order_delivered_carrier_date,
        t2.order_delivered_customer_date, t2.order_estimated_delivery_date,
        t3.payment_sequentials, t3.payment_type, t3.payment_installments,
        t3.payment_value,
        t4.customer_unique_id, t4.customer_city, t4.customer_state
        FROM order_reviews t1
        INNER JOIN orders as t2 ON t1.order_id = t2.order_id
        INNER JOIN tb_payments as t3 ON t2.order_id = t3.order_id
        INNER JOIN customers as t4 ON t2.customer_id = t4.customer_id),

tb_score_sellers AS (
        SELECT seller_id, AVG(review_score) as seller_score
        FROM (SELECT t1.order_id AS order_id,
          t1.seller_id AS seller_id,
          t3.review_score AS review_score
          FROM order_itens AS t1
          INNER JOIN orders AS t2 on t1.order_id = t2.order_id
          LEFT JOIN order_reviews AS t3 on t2.order_id = t3.order_id)
        GROUP BY seller_id),

tb_itens AS (
          SELECT order_id, COUNT(*) AS qt_products,
          COUNT(DISTINCT seller_id) AS qt_sellers,
          AVG(seller_score) AS avg_seller_score, SUM(price) AS price,
          SUM(freight_value) AS freight_value,
          COUNT(DISTINCT product_category_name) AS n_categories
          FROM (SELECT t1.order_id,t1.product_id, t1.seller_id,
            t1.price, t1.freight_value, t2.product_category_name, t3.seller_score
            FROM order_itens AS t1
            LEFT JOIN products AS t2 ON t1.product_id = t2.product_id
            LEFT JOIN tb_score_sellers AS t3 ON t1.seller_id = t3.seller_id)
          GROUP BY order_id),
          
tb_joins_olist AS(
          SELECT t1.*,
          t2.qt_products, t2.qt_sellers, t2.avg_seller_score, t2.price,
          t2.freight_value, t2.n_categories
          FROM tb_orders AS t1
          LEFT JOIN tb_itens AS t2
          ON t1.order_id = t2.order_id)

SELECT * from tb_joins_olist
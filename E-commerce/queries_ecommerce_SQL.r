library(tidyverse)
library(sqldf)

transactions_retail <- read.csv("commerce_data.csv", sep =";", stringsAsFactors = FALSE)

transactions_retail <- sqldf("SELECT InvoiceNo,StockCode,Description,
			Quantity,InvoiceDate,UnitPrice,CustomerID,Country,
			Quantity * UnitPrice as Value
			FROM transactions_retail
			WHERE Value > 0 AND StockCode NOT in('POST','M','D','CRUK','C2')
			AND CustomerID IS NOT NULL")

invoices_retail <- sqldf("SELECT InvoiceNo, InvoiceDate,CustomerID, Country,
			SUM(Value) AS TotalValue,
			COUNT(*) AS NTransactions
			FROM transactions_retail
			GROUP BY InvoiceNo,InvoiceDate,CustomerID,Country")
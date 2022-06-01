
library(tidyverse)
library(car)

train_transactions_1=read.csv("D:/TUM study documents/2020WS/Business Analytics/training_data_v2_L8PXAxZ/payments.csv")
summary(train_transactions_1)

train_transactions_1%>%group_by(Ownership_Indicator)

train_transactions_1 %>% group_by(Ownership_Indicator) %>% summarise(count=n())

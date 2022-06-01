# data preprocessing
library(tidyverse)
library(lubridate)
# data exploration
library(summarytools) # for user-friendly html summaries of data
library(ggmap) # for plotting data on a map
# for meta-ml
library(tidymodels)
library(dplyr)

options(dplyr.width = Inf) # show all columns when printing to console
theme_set(theme_minimal()) # set ggplot theme for cleaner plotting

physicians <- read_csv('physicians.csv')
head(physicians)

physicians <- physicians %>% select(-First_Name,-Middle_Name,-Last_Name,-Country)#
head(physicians)
countedLicenseState1 <- physicians %>% 
  group_by(License_State_1) %>% 
  summarise(count=n())

countedLicenseState1

companies <- read_csv('companies.csv')


## Transactions
payments <- read_csv('payments.csv')

payments <- payments %>% select(-Product_Name_1, -Product_Name_2, -Product_Name_3,-Product_Category_1,
                                -Product_Category_2, -Product_Category_3, -Product_Type_2, -Product_Type_3, 
                                -Product_Type_1, -Product_Code_1, -Product_Code_2, -Product_Code_3, -Contextual_Information)
head(payments)

# change ownership interest to bool
payments <- payments %>% mutate(Ownership_Indicator = ifelse(Ownership_Indicator == "Yes", 1,0))
sapply(payments, class)
transform(payments, Ownership_Indicator = as.logical(Ownership_Indicator))

# Transform third party recipient to factor
thirdparty <- payments %>% 
  group_by(Third_Party_Recipient) %>% 
  summarise(count=n())
thirdparty
payments$Third_Party_Recipient <- as.factor(payments$Third_Party_Recipient)


# Transform nature of payment to factor
naturOFPayment <- payments %>% 
  group_by(Nature_of_Payment_or_Transfer_of_Value) %>% 
  summarise(count=n())
naturOFPayment

payments$Nature_of_Payment_or_Transfer_of_Value <- as.factor(payments$Nature_of_Payment_or_Transfer_of_Value)

#Transform FORM of payments
formOfPayment <- payments %>% 
  group_by(Form_of_Payment_or_Transfer_of_Value) %>% 
  summarise(count=n())
formOfPayment

payments$Form_of_Payment_or_Transfer_of_Value <- as.factor(payments$Form_of_Payment_or_Transfer_of_Value)


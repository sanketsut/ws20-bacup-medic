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

### Import physicians and clean up
physicians <- read_csv('physicians.csv')
physicians <- physicians %>% rename(Physician_State = State) %>% rename(Physician_Country = Country)
physicians <- physicians %>% select(-City,-First_Name,-Middle_Name,-Last_Name,-Name_Suffix, -Province, -Primary_Specialty, -Zipcode)
physicians_train=physicians %>% filter(set=='train') %>% mutate(set=NULL)
physicians_test=physicians %>% filter(set=='test') %>% mutate(set=NULL)

#bad command countedLicenseState1 <- physicians %>%  group_by(License_State_1) %>%   summarise(count=n()) countedLicenseState1

companies <- read_csv('companies.csv')
companies <- companies %>% rename(Company_State = State) %>% rename(Company_Country = Country)
companies <- companies %>% select(-Name)
companies = companies %>% mutate(Company_Country=toupper(Company_Country))

## Transactions
payments <- read_csv('payments.csv')
payments <- payments %>% select(-Product_Name_1, -Product_Name_2, -Product_Name_3,-Product_Category_1, -Product_Category_2, -Product_Category_3, -City_of_Travel, -Date,
                                -Product_Type_2, -Product_Type_3, -Product_Type_1, -Product_Code_1, -Product_Code_2, -Product_Code_3, -Contextual_Information)

#make ownership indicator logical
payments=payments%>%mutate(Ownership_Indicator=ifelse(Ownership_Indicator=="Yes",TRUE,FALSE))

#convert to factor
payments$Third_Party_Recipient <- as.factor(payments$Third_Party_Recipient)
payments$Nature_of_Payment_or_Transfer_of_Value <- as.factor(payments$Nature_of_Payment_or_Transfer_of_Value)
payments$Form_of_Payment_or_Transfer_of_Value <- as.factor(payments$Form_of_Payment_or_Transfer_of_Value)


#divide payments data first into training and test, then into yes and no, then divide each into 70% training and 30% validation
payments_train=inner_join(payments,physicians_train,by = c("Physician_ID"="id"))
payments_test=inner_join(payments,physicians_test,by = c("Physician_ID"="id"))
payments_train_OI_yes=payments_train%>%filter(Ownership_Indicator==TRUE)
payments_train_OI_no=payments_train%>%filter(Ownership_Indicator==FALSE)

physicians_train_companies <- aggregate(Company_ID ~ Physician_ID, payments_train, function(x) length(unique(x)))
physicians_train_companies <- physicians_train_companies %>% mutate(Company_Number=Company_ID)
physicians_train_companies <- physicians_train_companies %>% select(-Company_ID)
physicians_train_companies = physicians_train_companies %>% rename(no_of_companies_interacted=Company_Number)



#divide the total payment amount into groups - bagging - try to find relation of these groups with ownership possibility
#grouping for all training transactions
t1=payments_train%>%mutate(cost_discrete=case_when(
  between(Total_Amount_of_Payment_USDollars,0,100)~"<=100",
  between(Total_Amount_of_Payment_USDollars,100,1000)~"<=1000",
  between(Total_Amount_of_Payment_USDollars,1000,10000)~"<=10000",
  between(Total_Amount_of_Payment_USDollars,10000,100000)~"<=100000",
  Total_Amount_of_Payment_USDollars>100000~">100000",
)
)
barplot(table(t1$cost_discrete))
#grouping for training transactions with OI yes
t2=payments_train_OI_yes%>%mutate(cost_discrete=case_when(
  between(Total_Amount_of_Payment_USDollars,0,100)~"<=100",
  between(Total_Amount_of_Payment_USDollars,100,1000)~"<=1000",
  between(Total_Amount_of_Payment_USDollars,1000,10000)~"<=10000",
  between(Total_Amount_of_Payment_USDollars,10000,100000)~"<=100000",
  Total_Amount_of_Payment_USDollars>100000~">100000",
)
)
barplot(table(t2$cost_discrete))

#payments_train_per_phy= and try to find if more no of transactions lead to higher chance of investment in firm
t3=payments_train%>%group_by(Physician_ID)%>%summarise(count=n())

#pivot the data longer to reduce no of columns (states) and tidy the data
t4=payments_train %>% pivot_longer(cols = starts_with("License"),names_to="State_No",values_to="State_Name")
t4$State_Name=as.factor(t4$State_Name)
t4$State_of_Travel=as.factor(t4$State_of_Travel)
t4$Physician_State=as.factor(t4$Physician_State)
t4=t4 %>% drop_na(State_Name)
t4=t4%>%group_by(Physician_ID)%>%mutate(P_has_OwnInt=if(any(Ownership_Indicator == TRUE)) {TRUE} else {FALSE})

# => t4 is payments_train data that underwent tidy

#complete_set <- inner_join(payments, physicians, by = c("Physician_ID" = "id"))
#complete_set <- inner_join(complete_set, companies, by = c("Company_ID" = "Company_ID"))
#complete_set <- complete_set %>% group_by(Physician_ID) %>% 
#  mutate(P_has_OwnInt=if(any(Ownership_Indicator == TRUE)) {TRUE} else {FALSE})

rel_freq_OI_1_by_Phys_State <- payments_train %>% group_by(Physician_State) %>% summarise(ratio = sum(Ownership_Indicator == 1)/n())

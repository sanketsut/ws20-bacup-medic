# data preprocessing
library(tidyverse)
library(lubridate)
# data exploration
library(summarytools) # for user-friendly html summaries of data
library(ggmap) # for plotting data on a map
# for meta-ml
library(tidymodels)
library(dplyr)
library(ggpubr)
library(ranger)

options(dplyr.width = Inf) # show all columns when printing to console
theme_set(theme_minimal()) # set ggplot theme for cleaner plotting

### Import physicians and clean up
physicians <- read_csv('physicians.csv')
physicians <- physicians %>% rename(Physician_State = State) %>% rename(Physician_Country = Country)

physicians <- physicians %>% select(-First_Name,-Middle_Name,-Last_Name,-Physician_Country,-Name_Suffix, -Province)

#countedLicenseState1 <- physicians %>% 
# group_by(License_State_1) %>% 
#  summarise(count=n())
#countedLicenseState1

companies <- read_csv('companies.csv')
companies <- companies %>% rename(Company_State = State) %>% rename(Company_Country = Country)

## Transactions
payments <- read_csv('payments.csv')

payments <- payments %>% select(-Product_Name_1, -Product_Name_2, -Product_Name_3,-Product_Category_1,
                                -Product_Category_2, -Product_Category_3, -Product_Type_2, -Product_Type_3, 
                                -Product_Type_1, -Product_Code_1, -Product_Code_2, -Product_Code_3, -Contextual_Information)


# change ownership interest to bool
payments <- payments %>% mutate(Ownership_Indicator = ifelse(Ownership_Indicator == "Yes", 1,0))
sapply(payments, class)
transform(payments, Ownership_Indicator = as.logical(Ownership_Indicator))

# Transform date to type Date
payments <- transform(payments, Date = as_date(payments$Date, format = "%d/%m/%Y"))


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
#formOfPayment <- payments %>% 
#  group_by(Form_of_Payment_or_Transfer_of_Value) %>% 
#  summarise(count=n())
#formOfPayment

payments$Form_of_Payment_or_Transfer_of_Value <- as.factor(payments$Form_of_Payment_or_Transfer_of_Value)

# Selecting test set physicians only
testSetPhysicians <- physicians %>% filter(set == "test")

# Check if test set matches the submission template
submission_template <- read_csv('submission_template.csv')
template_ids <- submission_template %>% arrange(id) %>% pull(id)
test_ids <- testSetPhysicians %>% arrange(id) %>% pull(id)
if(!all(template_ids == test_ids)) warning("Testset does not match template!")

# Join together all original tables
complete_set <- inner_join(payments, physicians, by = c("Physician_ID" = "id"))
complete_set <- inner_join(complete_set, companies, by = c("Company_ID" = "Company_ID"))
#complete_set %>% dfSummary %>% view('browser')




# Create solution vector
complete_set <- complete_set %>% group_by(Physician_ID) %>% 
  mutate(P_has_OwnInt=if(any(Ownership_Indicator == TRUE)) {TRUE} else {FALSE})

### MICHAEL START #############

#check if company state might influence how many transactions with OI are recorded
#number_of_tx_with_indi <- sum(payments$Ownership_Indicator == 1)
rel_freq_OI_1_by_Phys_State <- complete_set %>% group_by(Physician_State) %>% summarise(ratio = sum(Ownership_Indicator == 1)/n())

ggplot(rel_freq_OI_1_by_Phys_State, aes(x = reorder(Physician_State, -ratio), y = ratio)) +
  geom_bar(fill = "#0073C2FF", stat = "identity") +
  #geom_text(aes(label = ratio), vjust = -0.3) +
  theme_pubclean()

### we see that physicians living in MT have a significantly higher probability to receive a TX with ownership interest
### we therefore set a flag if a physician lives there
complete_set <- complete_set %>% group_by(Physician_State) %>% mutate(P_lives_in_MT = (Physician_State == "MT")) %>% ungroup()

### we also see that for some states (AK, DC, ME, ND, NE, OR, SD, UT, WY, NA) there is not a single transaction with ownership interest. We therefore
### add a feature describing if a physician lives in a state that is not MT or one of the above
rel_freq_larger_0 <- rel_freq_OI_1_by_Phys_State %>% filter(ratio > 0, Physician_State != 'MT')
complete_set <- complete_set %>% group_by(Physician_State) %>%  mutate(P_lives_in_common_OI_state = Physician_State %in% rel_freq_larger_0$Physician_State) %>% ungroup()

# repeat the same approach for 

rel_freq_OI_1_by_License_State1 <- complete_set %>% group_by(License_State_1) %>% mutate(ratio_L1 = sum(Ownership_Indicator == 1)) %>% ungroup() %>% select(License_State_1, ratio_L1) %>% distinct()
rel_freq_OI_1_by_License_State2 <- complete_set %>% group_by(License_State_2) %>% mutate(ratio_L2 = sum(Ownership_Indicator == 1)) %>% ungroup() %>% select(License_State_2, ratio_L2) %>% distinct()
rel_freq_OI_1_by_License_State3 <- complete_set %>% group_by(License_State_3) %>% mutate(ratio_L3 = sum(Ownership_Indicator == 1)) %>% ungroup() %>% select(License_State_3, ratio_L3) %>% distinct()
rel_freq_OI_1_by_License_State4 <- complete_set %>% group_by(License_State_4) %>% mutate(ratio_L4 = sum(Ownership_Indicator == 1)) %>% ungroup() %>% select(License_State_4, ratio_L4) %>% distinct()
rel_freq_OI_1_by_License_State5 <- complete_set %>% group_by(License_State_5) %>% mutate(ratio_L5 = sum(Ownership_Indicator == 1)) %>% ungroup() %>% select(License_State_5, ratio_L5) %>% distinct()

rel_freq_all <- inner_join(rel_freq_OI_1_by_License_State1, rel_freq_OI_1_by_License_State2, by = c("License_State_1" = "License_State_2")) %>% 
  full_join(rel_freq_OI_1_by_License_State3, by = c("License_State_1" = "License_State_3")) %>% 
  full_join(rel_freq_OI_1_by_License_State4, by = c("License_State_1" = "License_State_4")) %>% 
  full_join(rel_freq_OI_1_by_License_State5, by = c("License_State_1" = "License_State_5")) %>% 
  filter(!is.na(License_State_1)) %>% 
  # fill NaNs with 0
  mutate_if(is.numeric, ~replace(., is.na(.), 0))

rel_freq_all$total = rowSums(rel_freq_all[2:6])


ggplot(rel_freq_all, aes(x = reorder(License_State_1, -total), y = total)) +
    geom_bar(fill = "#0073C2FF", stat = "identity") +
    #geom_text(aes(label = ratio), vjust = -0.3) +
    theme_pubclean()

# checks for every row in the df, how many of instances of License_State_1,2,... are contained in the compare vector and adds a new column with categorized values
categorize_license_state_frequency <- function(df_with_license_states, compare_vector, new_col_name) {
  license_state_column_names <- grep("License", names(df_with_license_states), ignore.case = T)
    for (j in 1:nrow(df_with_license_states)) {
      counter = 0
      for (i in license_state_column_names) {
        if (df_with_license_states[j, i] %in% compare_vector) {
          counter = counter + 1
        }
      }
      df_with_license_states[j, new_col_name] = case_when(counter > 1.0 ~ "High",
                                                          counter == 1.0 ~ "Medium",
                                                          TRUE ~ "None"
                                                          )
    }
    return(df_with_license_states)
  }
  
# categorize_license_state_frequency(head(physicians), c("GA", "SC", "NY"), "testCount2")

### MICHAEL END##########

# Count total transactions
complete_set <- complete_set %>% group_by(Physician_ID) %>% mutate(total_transactions = n()) %>% ungroup()


# Only select total TX for ownership indicator TRUE or FALSE
## first select only transactions that have NO ownership indicator to simulate test set
tmp <- complete_set %>% group_by(Physician_ID) %>% filter(Ownership_Indicator == 0) %>%select(P_has_OwnInt, total_transactions) %>% distinct

tmp_only_FALSE <- complete_set %>% select(P_has_OwnInt, total_transactions) %>% filter(P_has_OwnInt == FALSE) %>% select(total_transactions) %>% distinct()
tmp_only_TRUE <- complete_set %>% select(P_has_OwnInt, total_transactions) %>% filter(P_has_OwnInt == TRUE) %>% select(total_transactions) %>% distinct()

# View the boxplots
tmp %>% ggplot(aes(y=total_transactions, x=P_has_OwnInt, fill = P_has_OwnInt)) +
  geom_boxplot()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# barplots showing the distribution of number for transactions for both subsets
barplot(table(cut(tmp_only_FALSE$total_transactions, seq(0, 3000, 200)))/nrow(tmp_only_FALSE))
barplot(table(cut(tmp_only_TRUE$total_transactions, seq(0, 3000, 200)))/nrow(tmp_only_TRUE))

# Get precise data
#summary(tmp_only_FALSE)
#summary(tmp_only_TRUE)

# We see that the total number of transactions is generally higher for Physicians that
# have an Ownership Interest
# If a physician has received more than 825 transactions (3rd Quartile of tmp_only_FALSE), it seems likelier that the physician
# has an ownership interest in a company.
# We therefore set this value as a cutoff for a binominal variable 'High_Amount_of_TX', which is TRUE for values above the cutoff
nr_TX_cutoff_3qt_FALSE <- quantile(tmp_only_FALSE$total_transactions, 0.75)
nr_TX_cutoff_3qt_TRUE <- quantile(tmp_only_TRUE$total_transactions, 0.75)

complete_set <- complete_set %>% mutate(total_transactions = cut(total_transactions, breaks = c(0, nr_TX_cutoff_3qt_FALSE, nr_TX_cutoff_3qt_TRUE, Inf)))


### Difference in NATURE of Payment when comparing subsets P_has_OwnInt = TRUE/FALSE

allOwnInd <- complete_set %>% filter(Ownership_Indicator == 0) %>%
  select(Nature_of_Payment_or_Transfer_of_Value) %>% group_by(Nature_of_Payment_or_Transfer_of_Value) %>% mutate(count = n()) %>% distinct() %>%
  mutate(relFreq = count/sum(.$count))

onlyTRUEOwnInd <- complete_set %>% filter(P_has_OwnInt == TRUE) %>% filter(Ownership_Indicator == 0) %>%
  select(Nature_of_Payment_or_Transfer_of_Value) %>% group_by(Nature_of_Payment_or_Transfer_of_Value) %>% mutate(count = n()) %>% distinct() %>%
  mutate(relFreq = count/sum(.$count))

onlyFALSEOwnInd <- complete_set %>% filter(P_has_OwnInt == FALSE) %>%
  select(Nature_of_Payment_or_Transfer_of_Value) %>% group_by(Nature_of_Payment_or_Transfer_of_Value) %>% mutate(count = n()) %>% distinct() %>%
  mutate(relFreq = count/sum(.$count))

#### MICHAEL START #####
plot(allOwnInd$Nature_of_Payment_or_Transfer_of_Value, allOwnInd$relFreq, ylim = c(0,1))
plot(onlyTRUEOwnInd$Nature_of_Payment_or_Transfer_of_Value, onlyTRUEOwnInd$relFreq, ylim = c(0,1), xlab = "TRUE OI")
plot(onlyFALSEOwnInd$Nature_of_Payment_or_Transfer_of_Value, onlyFALSEOwnInd$relFreq, ylim = c(0,1), xlab = "FALSE OI")

## Check for each physician, if rel frequency of nature of payment is higher/lower than average. This was done for the three most different w.r.t. Ownship_Interest
# check for Travel and Lodging
complete_set <- complete_set %>% group_by(Physician_ID) %>%
  mutate(high_Travel_Lodg = sum(Nature_of_Payment_or_Transfer_of_Value == "Travel and Lodging")/n() >= allOwnInd$relFreq[allOwnInd$Nature_of_Payment_or_Transfer_of_Value == "Travel and Lodging"])

# check for Food and Beverage
complete_set <- complete_set %>% group_by(Physician_ID) %>%
  mutate(high_Food_Bev = sum(Nature_of_Payment_or_Transfer_of_Value == "Food and Beverage")/n() >= allOwnInd$relFreq[allOwnInd$Nature_of_Payment_or_Transfer_of_Value == "Food and Beverage"]) %>% ungroup()

# check for compensation for services other than consulting...
complete_set <- complete_set %>% group_by(Physician_ID) %>%
  mutate(high_Compensation_other_than_consult = sum(Nature_of_Payment_or_Transfer_of_Value == "Compensation for services other than consulting, including serving as faculty or as a speaker at a venue other than a continuing education program")/n() >= allOwnInd$relFreq[allOwnInd$Nature_of_Payment_or_Transfer_of_Value == "Compensation for services other than consulting, including serving as faculty or as a speaker at a venue other than a continuing education program"]) %>% ungroup()

### Repeat approach for Difference in FORM of Payment when comparing subsets P_has_OwnInft = TRUE/FALSE
allOwnInd_FORM <- complete_set %>% filter(Ownership_Indicator == 0) %>%
  select(Form_of_Payment_or_Transfer_of_Value) %>% group_by(Form_of_Payment_or_Transfer_of_Value) %>% mutate(count = n()) %>% distinct() %>%
  mutate(relFreq = count/sum(.$count))

onlyTRUEOwnInd_FORM <- complete_set %>% filter(P_has_OwnInt == TRUE) %>% filter(Ownership_Indicator == 0) %>%
  select(Form_of_Payment_or_Transfer_of_Value) %>% group_by(Form_of_Payment_or_Transfer_of_Value) %>% mutate(count = n()) %>% distinct() %>%
  mutate(relFreq = count/sum(.$count))

onlyFALSEOwnInd_FORM <- complete_set %>% filter(P_has_OwnInt == FALSE) %>%
  select(Form_of_Payment_or_Transfer_of_Value) %>% group_by(Form_of_Payment_or_Transfer_of_Value) %>% mutate(count = n()) %>% distinct() %>%
  mutate(relFreq = count/sum(.$count))

plot(allOwnInd_FORM$Form_of_Payment_or_Transfer_of_Value, allOwnInd_FORM$relFreq, ylim = c(0,1), xlab = "entire_set")
plot(onlyTRUEOwnInd_FORM$Form_of_Payment_or_Transfer_of_Value, onlyTRUEOwnInd_FORM$relFreq, ylim = c(0,1), xlab = "TRUE OI")
plot(onlyFALSEOwnInd_FORM$Form_of_Payment_or_Transfer_of_Value, onlyFALSEOwnInd_FORM$relFreq, ylim = c(0,1), xlab = "FALSE OI")

# We see that for transactions that have OI, the rel freq is almost double than that of TX that have no OI. Thus, let's transform it in a feature
complete_set <- complete_set %>% group_by(Physician_ID) %>%
  mutate(high_Amount_of_Cash_TX = sum(Form_of_Payment_or_Transfer_of_Value == "Cash or cash equivalent")/n() >= allOwnInd_FORM$relFreq[allOwnInd_FORM$Form_of_Payment_or_Transfer_of_Value == "Cash or cash equivalent"])


# Aggregate on physician level: Only select features and License_states (the latter are necessary for the step below)
complete_set_physician_aggregated <- complete_set %>% select(Physician_ID, set, P_has_OwnInt, P_lives_in_MT, P_lives_in_common_OI_state, total_transactions, high_Travel_Lodg, high_Food_Bev, high_Amount_of_Cash_TX, starts_with("License_State")) %>% distinct()

## Add features for License State on physician level (done here since it would take even longer when doing it on "complete_set")
## THIS TAKES SOME TIME
levels_for_factorizing <- c("None", "Medium", "High")

complete_set_physician_aggregated <- categorize_license_state_frequency(complete_set_physician_aggregated, rel_freq_all %>% filter(total > 200) %>% .$License_State_1, "Amount_top_prob_Licenses") %>% mutate(Amount_top_prob_Licenses = factor(Amount_top_prob_Licenses, ordered = TRUE, levels = levels_for_factorizing))
complete_set_physician_aggregated <- categorize_license_state_frequency(complete_set_physician_aggregated, rel_freq_all %>% filter(total <= 200, total > 100) %>% .$License_State_1, "Amount_middle_prob_Licenses") %>% mutate(Amount_middle_prob_Licenses = factor(Amount_middle_prob_Licenses, ordered = TRUE, levels = levels_for_factorizing))
complete_set_physician_aggregated <- categorize_license_state_frequency(complete_set_physician_aggregated, rel_freq_all %>% filter(total <= 100) %>% .$License_State_1, "Amount_low_prob_Licenses") %>% mutate(Amount_low_prob_Licenses = factor(Amount_low_prob_Licenses, ordered = TRUE, levels = levels_for_factorizing))

#remove now unnecessary license states
complete_set_physician_aggregated <- complete_set_physician_aggregated %>% select(-starts_with("License_State")) %>% mutate(P_has_OwnInt = as.factor(P_has_OwnInt))

### trying out stuff
#cols <- c("P_lives_in_MT", "P_lives_in_common_OI_state", "high_Travel_Lodg", "high_Food_Bev", "high_Amount_of_Cash_TX")

#complete_set_physician_aggregated[cols] <- lapply(complete_set_physician_aggregated[cols], factor)

############# IMPORTANT FOR ML ####################
# removes all rows where there is NA in any column. I don't know why, but for one physician there is an NA for feature "P_lives_in_MT"
# we thus end up with 5999 entries

complete_set_physician_aggregated <- na.omit(complete_set_physician_aggregated)
#####################################################


# Construct test and training set
test <- semi_join(complete_set_physician_aggregated, testSetPhysicians, by = c("Physician_ID" = "id")) %>% select(-set)
train <- anti_join(complete_set_physician_aggregated, test, by = c("Physician_ID" = "Physician_ID")) %>% select(-set)

###########################################################################################################################################
######################################                                 ####################################################################
######################################         Model generation        ####################################################################
###########################################################################################################################################




rec <- recipe(
  #specift predictors, target and data to enable code autocompletion
  P_has_OwnInt ~ ., data = train) %>% 
  # tell tidymodels that `id` is an ID and should not be used in any model
  update_role(Physician_ID, new_role = "ID") #%>%
  
  
  # determine what happens when a new nominal value is encountered in test data (which was missing from the trianing set)
  #step_novel(all_nominal(), -has_role("ID"), new_level="new") %>% 
  # impute all other nominal (character + factor) columns with the value "none"
  #step_unknown(all_nominal(), new_level = "none") %>% 
  
  # convert all strings to factors
  #step_string2factor(all_nominal(), -all_outcomes(), -has_role("ID")) %>% 
  # remove constant columns
  #step_zv(all_predictors())


# k-fold cross validation
folds <- train %>% vfold_cv(v=4)

# model specification
model <- rand_forest(
  mode = 'classification'
) %>% set_engine("ranger",
                 importance = "impurity" #needed for feature importance plot below
)

model_logit <- logistic_reg() %>% 
  set_engine("glm") %>% 
  set_mode("classification") %>% 
  translate()

# training workflow
training_workflow <- 
  # start with a blank workflow() object
  workflow() %>% 
  # add our preprocessing recipe
  add_recipe(rec) %>% 
  # add our model specification
  
  ### REPLACE MODEL HERE WITH "model_logit"  IF YOU WANT TO TRY LOGISTIC REGRESSION
  add_model(model)

# training
cv_fits <- 
  training_workflow %>% 
  fit_resamples(folds,
                metrics = metric_set(yardstick::bal_accuracy)
  )

cv_fits %>% collect_metrics()

### MICHAEL END
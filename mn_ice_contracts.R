library(ggplot2)
library(tidyverse)
library(lubridate)
library(plotly)

setwd("/Users/alyssachen/Desktop/Projects/minnesota")

# All contracts with ICE with recipient location in MN
minnesota <- read.csv("mn_all/Contracts_PrimeAwardSummaries_2025-03-21_H19M57S23_1.csv")

# All contracts with ICE after 2020-01-01
all_after_2020 <- read.csv("all_after_2020/Contracts_PrimeAwardSummaries_2025-03-28_H03M21S32_1.csv")

# University of Minnesota contracts with ICE
umn <- read.csv("umn/Contracts_PrimeAwardSummaries_2025-03-28_H02M59S08_1.csv")


# convert relevant dates using lubridate package
process_data <- function(data){
  data <- data %>%
    mutate(period_of_performance_current_end_date = ymd(period_of_performance_current_end_date),
           award_base_action_date=ymd(award_base_action_date))
  
  return(data)
}

all_after_2020 <- process_data(all_after_2020)



# Q: Check to see educational institutions receiving ICE contracts. 
# A: UMN is the only university to receive contracts since 2023. 
all_after_2020 %>%
  filter(educational_institution=="t"  & period_of_performance_current_end_date>ymd("2022-01-01") ) %>%
  select(recipient_name, period_of_performance_current_end_date) %>%
  arrange(period_of_performance_current_end_date)

# Q: How much has UMN received in obligations? 
# A: $60,374.50
umn %>% 
  summarise(obligations = sum(total_obligated_amount))

umn %>%
  select(period_of_performance_start_date, period_of_performance_current_end_date,
         total_obligated_amount)

# Q: How much has UMN received in outlays? 
# A: $17,395
umn%>%
  summarise(outlays = sum(total_outlayed_amount, na.rm=TRUE))

# Q: How does Minnesota compare to other states in total obligations in the past five years?
# A: Minnesota was 17th, receiving $43,894,456 in contracts since Jan 2020. 
all_after_2020 %>% 
  filter(award_base_action_date > ymd("2020-01-01")) %>%
  group_by(recipient_state_name) %>%
  summarize(obligations = sum(total_obligated_amount)) %>%
  arrange(desc(obligations)) %>%
  print(n=20)

# Q: How does Minnesota compare to other states in total obligations in the past year?
# A: Minnesota was 18th, receiving $7,009,381 in contracts since Jan 2024.  
# (compare to VA with $555m or AZ with $17m)
all_after_2020 %>% 
  filter(award_base_action_date > ymd("2024-01-01")) %>%
  group_by(recipient_state_name) %>%
  summarize(obligations = sum(total_obligated_amount)) %>%
  arrange(desc(obligations)) %>%
  print(n=20)

# Q: How does Minnesota compare to other states in receiving contracts for ammunition? 
# (see https://www.acquisition.gov/psc-manual)
# A: Minnesota receives by far the most in contracts for ammunition. 
# $28,545,798 since 2020, and $6,636,805 since 2024. 
ammunition_codes <- c("1305","1310","1315","1320")
ammunition_by_state <- all_after_2020 %>% 
  filter(award_base_action_date > ymd("2020-01-01") & product_or_service_code %in% ammunition_codes) %>%
  group_by(recipient_state_name) %>%
  summarize(obligations = sum(total_obligated_amount)) %>%
  arrange(desc(obligations)) 

write.csv(ammunition_by_state, "ammunition_by_state.csv")

# Q: Same as above, but for Vista Outdoors
# A: $6,636,805 in the past year
all_after_2020 %>% 
  filter(award_base_action_date > ymd("2024-01-01") & product_or_service_code %in% ammunition_codes) %>%
  group_by(recipient_name) %>%
  summarize(obligations = sum(total_obligated_amount)) %>%
  arrange(desc(obligations)) 

# Vista Outdoor/MN has $5 million in active contracts
all_after_2020 %>% 
  filter(period_of_performance_current_end_date > ymd("2025-03-28") & product_or_service_code %in% ammunition_codes) %>%
  group_by(recipient_state_name) %>%
  summarize(obligations = sum(total_obligated_amount)) %>%
  arrange(desc(obligations)) 


# Q: How does Minnesota compare to other states in receiving contracts for gun range rentals? 
all_after_2020 %>% 
  filter(award_base_action_date > ymd("2020-01-01") & product_or_service_code =="X1EA") %>%
  group_by(recipient_state_name) %>%
  summarize(obligations = sum(total_obligated_amount)) %>%
  arrange(desc(obligations)) 

all_after_2020 %>% 
  filter(award_base_action_date > ymd("2020-01-01") & product_or_service_code =="X1EA") %>%
  group_by(recipient_name) %>%
  summarize(obligations = sum(total_obligated_amount)) %>%
  arrange(desc(obligations)) 

# active gun range contracts
active_gun_range_contracts <- all_after_2020 %>% 
  filter(period_of_performance_current_end_date > ymd("2025-01-01") & product_or_service_code =="X1EA") %>%
  group_by(recipient_name) %>%
  summarize(obligations = sum(total_obligated_amount)) %>%
  arrange(desc(obligations)) 

write.csv(active_gun_range_contracts, "active_gun_range_contracts.csv")





dim(data)
colnames(data)

# cut down columns to potentially relevant ones
contracts <- data[,c(11:82,91,96:103,286)]
dim(contracts)
colnames(contracts)

# cut down for display
pared <- contracts[,c(36,75,1,3,5,7,9,10,66,81,73)]
head(pared)

pared$award_base_action_date <- ymd(pared$award_base_action_date)

write.csv(pared, "mn_contracts_selectedcols.csv")


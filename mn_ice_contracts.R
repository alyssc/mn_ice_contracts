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
# A: Minnesota receives by far the most in contracts for ammunition. 
# $28,983,996 since 2020, and $6,655,672 since 2024. 
all_after_2020 %>% 
  filter(award_base_action_date > ymd("2024-01-01") & grepl("AMMUNITION", product_or_service_code_description, fixed=TRUE)) %>%
  group_by(recipient_state_name) %>%
  summarize(obligations = sum(total_obligated_amount)) %>%
  arrange(desc(obligations)) 

# Q: Same as above, but for Vista Outdoors
# A: $6,636,805 in the past year
all_after_2020 %>% 
  filter(award_base_action_date > ymd("2024-01-01") & grepl("AMMUNITION", product_or_service_code_description, fixed=TRUE)) %>%
  group_by(recipient_name) %>%
  summarize(obligations = sum(total_obligated_amount)) %>%
  arrange(desc(obligations)) 




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


library(ggplot2)
library(tidyverse)
library(lubridate)
library(plotly)

pol_dat <- read.csv("Police_Use_of_Force.csv")

pol_dat <- pol_dat %>% 
  filter(!str_detect(force, "De Escalation")) %>%
  filter(force != "No Force Reported")%>%
  mutate(date=ymd_hms(reportedDate),
         month = floor_date(as_date(date), "month"),
         wkday=wday(date, label=TRUE)) 

pol_dat %>% 
  group_by(force) %>%
  tally() %>% 
  arrange(desc(n)) %>%
  print(n=25)

by_month <- pol_dat %>% 
  group_by(month) %>%
  tally() 

p <- by_month %>%
  ggplot( aes(x=month, y=n)) +
  geom_area(fill="#69b3a2", alpha=0.5) +
  geom_line(color="#69b3a2") +
  ylab("# of incidents")
p

# interactive
p <- ggplotly(p)
p


# minors

by_month_minors <- pol_dat %>% 
  filter(eventAge < 18) %>%
  group_by(month) %>%
  tally()

p_minors <- by_month_minors %>%
  ggplot( aes(x=month, y=n)) +
  geom_area(fill="#69b3a2", alpha=0.5) +
  geom_line(color="#69b3a2") +
  ylab("# of incidents")
p_minors


# Filter by force
force_filter <- function(force_name){
  by_force <- pol_dat %>% 
    filter(force == force_name) %>% 
    group_by(month) %>%
    tally() 
  
  p <- by_force %>%
    ggplot( aes(x=month, y=n)) +
    geom_area(fill="#69b3a2", alpha=0.5) +
    geom_line(color="#69b3a2") +
    ylab("# of incidents")
  return(p)
}

force_filter(force = "Bodily Force")
force_filter(force = "Firearm")
force_filter(force = "Handcuffs")



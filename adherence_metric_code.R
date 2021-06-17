# this script reads in headspace user data and derives usage statistics

# load packages
library(tidyverse)
library(readxl)

# read in data
dat <- read_excel("User Data ALL COHORTS.xlsx",
                  sheet = "ALL Packs and Sessions")

# derive total minutes/sessions adherence metrics for mindfulness sessions
# and all headspace sessions
usage_dat <-
  dat %>%
    filter(`Notes, Questions` == "Mindfulness") %>% # filter only for mindfulness sessions
    group_by(`Voucher Code`) %>%                    # group by each participant
    summarize(mindfulness_minutes = sum(Duration, na.rm = TRUE),
              mindfulness_sessions = n()) %>%       # derive total sessions and minutes
  full_join(
    dat %>%
      group_by(`Voucher Code`) %>%
      summarize(all_headspace_minutes = sum(Duration, na.rm = TRUE),
                all_headspace_sessions = n()),      # derive total sessions and minutes for all sessions
    by = "Voucher Code")

# save csv file
write_csv(usage_dat, "adherence_metric_data.csv")

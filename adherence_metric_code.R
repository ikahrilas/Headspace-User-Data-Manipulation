# this script reads in headspace user data and derives usage statistics

# load packages
library(tidyverse)
library(lubridate)
library(readxl)

# read in data
## usage data
usage_dat <- read_excel("User Data ALL COHORTS.xlsx",
                        sheet = "ALL Packs and Sessions")

## timeline data
timeline_dat <- read_excel("User Data ALL COHORTS.xlsx",
                           sheet = "Timeline") %>%
  mutate(
    across(.cols = c(`Midpoint Ax`, `Post Ax`, `1-Mo F/U Ax`),
           .fns = ~ as_datetime(as_date(as.numeric(.x),
                                origin = "1899-12-30")))
  )

## merge two data sets together
dat <- full_join(usage_dat, timeline_dat, by = c("Voucher Code" = "HS Access Code"))

# convert UTC to central time
dat <- dat %>%
  mutate(`Sessions Time` = with_tz(`Sessions Time`, tzone = "America/Chicago"))

# derive adherence metric variables
## define vector of unique PSA pids
pids <- timeline_dat %>%
  distinct(`Participant Study ID`) %>%
  pull()

adherence_dat <- map_df(pids, ~ {
  all_mm <- dat %>%
    filter(`Participant Study ID` == .x,
           `Notes, Questions` == "Mindfulness") %>%
    group_by(`Participant Study ID`) %>%
    summarize(all_mind_min = sum(Duration, na.rm = TRUE),
              all_mind_sess = n())

  all_hs <- dat %>%
    filter(`Participant Study ID` == .x) %>%
    group_by(`Participant Study ID`) %>%
    summarize(all_hs_sess = n())

  mo1_mm <- dat %>%
    filter(`Participant Study ID` == .x,
           `Notes, Questions` == "Mindfulness",
           between(`Sessions Time`,
                   timeline_dat[timeline_dat$`Participant Study ID` == .x,]$`Code Activation`,
                   timeline_dat[timeline_dat$`Participant Study ID` == .x,]$`Midpoint Ax`)) %>%
    group_by(`Participant Study ID`) %>%
    summarize(m1_mind_min = sum(Duration, na.rm = TRUE),
              m1_mind_sess = n())

  mo1_all <- dat %>%
    filter(`Participant Study ID` == .x,
           between(`Sessions Time`,
                   timeline_dat[timeline_dat$`Participant Study ID` == .x,]$`Code Activation`,
                   timeline_dat[timeline_dat$`Participant Study ID` == .x,]$`Midpoint Ax`)) %>%
    group_by(`Participant Study ID`) %>%
    summarize(m1_all_sess = n())

  mo2_mm <- dat %>%
    filter(`Participant Study ID` == .x,
           `Notes, Questions` == "Mindfulness",
           between(`Sessions Time`,
                   timeline_dat[timeline_dat$`Participant Study ID` == .x,]$`Midpoint Ax`,
                   timeline_dat[timeline_dat$`Participant Study ID` == .x,]$`Post Ax`)) %>%
    group_by(`Participant Study ID`) %>%
    summarize(m2_mind_min = sum(Duration, na.rm = TRUE),
              m2_mind_sess = n())

  mo2_all <- dat %>%
    filter(`Participant Study ID` == .x,
           between(`Sessions Time`,
                   timeline_dat[timeline_dat$`Participant Study ID` == .x,]$`Midpoint Ax`,
                   timeline_dat[timeline_dat$`Participant Study ID` == .x,]$`Post Ax`)) %>%
    group_by(`Participant Study ID`) %>%
    summarize(m2_all_sess = n())

  mo3_mm <- dat %>%
    filter(`Participant Study ID` == .x,
           `Notes, Questions` == "Mindfulness",
           between(`Sessions Time`,
                   timeline_dat[timeline_dat$`Participant Study ID` == .x,]$`Post Ax`,
                   timeline_dat[timeline_dat$`Participant Study ID` == .x,]$`1-Mo F/U Ax`)) %>%
    group_by(`Participant Study ID`) %>%
    summarize(m3_mind_min = sum(Duration, na.rm = TRUE),
              m3_mind_sess = n())

  mo3_all <- dat %>%
    filter(`Participant Study ID` == .x,
           between(`Sessions Time`,
                   timeline_dat[timeline_dat$`Participant Study ID` == .x,]$`Post Ax`,
                   timeline_dat[timeline_dat$`Participant Study ID` == .x,]$`1-Mo F/U Ax`)) %>%
    group_by(`Participant Study ID`) %>%
    summarize(m3_all_sess = n())

  t3_cum_mm <- dat %>%
    filter(`Participant Study ID` == .x,
           `Notes, Questions` == "Mindfulness",
           between(`Sessions Time`,
                   timeline_dat[timeline_dat$`Participant Study ID` == .x,]$`Code Activation`,
                   timeline_dat[timeline_dat$`Participant Study ID` == .x,]$`Post Ax`)) %>%
    group_by(`Participant Study ID`) %>%
    summarize(t3_cum_mind_min = sum(Duration, na.rm = TRUE),
              t3_cum_mind_sess = n())

  t3_all <- dat %>%
    filter(`Participant Study ID` == .x,
           between(`Sessions Time`,
                   timeline_dat[timeline_dat$`Participant Study ID` == .x,]$`Code Activation`,
                   timeline_dat[timeline_dat$`Participant Study ID` == .x,]$`Post Ax`)) %>%
    group_by(`Participant Study ID`) %>%
    summarize(t3_all_sess = n())

  t4_cum_mm <- dat %>%
    filter(`Participant Study ID` == .x,
           `Notes, Questions` == "Mindfulness",
           between(`Sessions Time`,
                   timeline_dat[timeline_dat$`Participant Study ID` == .x,]$`Code Activation`,
                   timeline_dat[timeline_dat$`Participant Study ID` == .x,]$`1-Mo F/U Ax`)) %>%
    group_by(`Participant Study ID`) %>%
    summarize(t4_cum_mind_min = sum(Duration, na.rm = TRUE),
              t4_cum_mind_sess = n())

  t4_all <- dat %>%
    filter(`Participant Study ID` == .x,
           between(`Sessions Time`,
                   timeline_dat[timeline_dat$`Participant Study ID` == .x,]$`Code Activation`,
                   timeline_dat[timeline_dat$`Participant Study ID` == .x,]$`1-Mo F/U Ax`)) %>%
    group_by(`Participant Study ID`) %>%
    summarize(t4_all_sess = n())

  full_join(all_mm, all_hs, by = "Participant Study ID") %>%
    full_join(mo1_mm, by = "Participant Study ID") %>%
    full_join(mo2_mm, by = "Participant Study ID") %>%
    full_join(mo3_mm, by = "Participant Study ID") %>%
    full_join(t3_cum_mm, by = "Participant Study ID") %>%
    full_join(t4_cum_mm, by = "Participant Study ID") %>%
    mutate(
      across(.cols = everything(),
             .fns = ~ replace_na(.x, 0))
    )
})

# save csv file
write_csv(adherence_dat, "adherence_metric_data.csv")

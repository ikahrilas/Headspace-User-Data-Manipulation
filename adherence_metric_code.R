# this script reads in headspace user data and derives usage statistics

# load packages
library(tidyverse)
library(lubridate)
library(readxl)

# read in data
## usage data
usage_dat <- read_excel("User Data ALL COHORTS-2.xlsx",
                        sheet = "ALL Packs and Sessions")

## timeline data
timeline_dat <-
  read_excel("User Data ALL COHORTS-2.xlsx", sheet = "Timeline - ALL HS Users")
    # mutate(
    #   across(.cols = c(`Midpoint Ax`, `Post Ax`, `1-Mo F/U Ax`),
    #          .fns = ~ as_datetime(as_date(as.numeric(.x),
    #                               origin = "1899-12-30"))))

## merge two data sets together
dat <- full_join(usage_dat, timeline_dat, by = c("Voucher Code" = "HS Access Code"))

# convert UTC to central time
dat <- dat %>% mutate(`Sessions Time` = with_tz(`Sessions Time`, tzone = "America/Chicago"))

# derive adherence metric variables
## define vector of unique PSA pids
pids <- timeline_dat %>%
  distinct(`Participant Study ID`) %>%
  pull()

adherence_dat <-
  map_df(pids, ~ {
    all_mm <- dat %>%
      filter(`Participant Study ID` == .x,
             `Mindfulness or Other (new column)` == "Mindfulness") %>%
      group_by(`Participant Study ID`) %>%
      summarize(all_mind_min = sum(Duration, na.rm = TRUE),
                all_mind_sess = n())

    all_hs <- dat %>%
      filter(`Participant Study ID` == .x) %>%
      group_by(`Participant Study ID`) %>%
      summarize(all_hs_sess = n())

    mo1_mm <- dat %>%
      filter(`Participant Study ID` == .x,
             `Mindfulness or Other (new column)` == "Mindfulness",
             between(`Sessions Time`,
                     timeline_dat[timeline_dat$`Participant Study ID` == .x,][1,]$`Code Activation`,
                     timeline_dat[timeline_dat$`Participant Study ID` == .x,][1,]$`Midpoint Ax`)) %>%
      group_by(`Participant Study ID`) %>%
      summarize(m1_mind_min = sum(Duration, na.rm = TRUE),
                m1_mind_sess = n())

    mo1_all <- dat %>%
      filter(`Participant Study ID` == .x,
             between(`Sessions Time`,
                     timeline_dat[timeline_dat$`Participant Study ID` == .x,][1,]$`Code Activation`,
                     timeline_dat[timeline_dat$`Participant Study ID` == .x,][1,]$`Midpoint Ax`)) %>%
      group_by(`Participant Study ID`) %>%
      summarize(m1_all_hs_sess = n())

    mo2_mm <- dat %>%
      filter(`Participant Study ID` == .x,
             `Mindfulness or Other (new column)` == "Mindfulness",
             between(`Sessions Time`,
                     timeline_dat[timeline_dat$`Participant Study ID` == .x,][1,]$`Midpoint Ax`,
                     timeline_dat[timeline_dat$`Participant Study ID` == .x,][1,]$`Post Ax`)) %>%
      group_by(`Participant Study ID`) %>%
      summarize(m2_mind_min = sum(Duration, na.rm = TRUE),
                m2_mind_sess = n())

    mo2_all <- dat %>%
      filter(`Participant Study ID` == .x,
             between(`Sessions Time`,
                     timeline_dat[timeline_dat$`Participant Study ID` == .x,][1,]$`Midpoint Ax`,
                     timeline_dat[timeline_dat$`Participant Study ID` == .x,][1,]$`Post Ax`)) %>%
      group_by(`Participant Study ID`) %>%
      summarize(m2_all_hs_sess = n())

    mo3_mm <- dat %>%
      filter(`Participant Study ID` == .x,
             `Mindfulness or Other (new column)` == "Mindfulness",
             between(`Sessions Time`,
                     timeline_dat[timeline_dat$`Participant Study ID` == .x,][1,]$`Post Ax`,
                     timeline_dat[timeline_dat$`Participant Study ID` == .x,][1,]$`1-Mo F/U Ax`)) %>%
      group_by(`Participant Study ID`) %>%
      summarize(m3_mind_min = sum(Duration, na.rm = TRUE),
                m3_mind_sess = n())

    mo3_all <- dat %>%
      filter(`Participant Study ID` == .x,
             between(`Sessions Time`,
                     timeline_dat[timeline_dat$`Participant Study ID` == .x,][1,]$`Post Ax`,
                     timeline_dat[timeline_dat$`Participant Study ID` == .x,][1,]$`1-Mo F/U Ax`)) %>%
      group_by(`Participant Study ID`) %>%
      summarize(m3_all_hs_sess = n())

    t3_cum_mm <- dat %>%
      filter(`Participant Study ID` == .x,
             `Mindfulness or Other (new column)` == "Mindfulness",
             between(`Sessions Time`,
                     timeline_dat[timeline_dat$`Participant Study ID` == .x,][1,]$`Code Activation`,
                     timeline_dat[timeline_dat$`Participant Study ID` == .x,][1,]$`Post Ax`)) %>%
      group_by(`Participant Study ID`) %>%
      summarize(t3_cum_mind_min = sum(Duration, na.rm = TRUE),
                t3_cum_mind_sess = n())

    t3_all <- dat %>%
      filter(`Participant Study ID` == .x,
             between(`Sessions Time`,
                     timeline_dat[timeline_dat$`Participant Study ID` == .x,][1,]$`Code Activation`,
                     timeline_dat[timeline_dat$`Participant Study ID` == .x,][1,]$`Post Ax`)) %>%
      group_by(`Participant Study ID`) %>%
      summarize(t3_all_hs_sess = n())

    t4_cum_mm <- dat %>%
      filter(`Participant Study ID` == .x,
             `Mindfulness or Other (new column)` == "Mindfulness",
             between(`Sessions Time`,
                     timeline_dat[timeline_dat$`Participant Study ID` == .x,][1,]$`Code Activation`,
                     timeline_dat[timeline_dat$`Participant Study ID` == .x,][1,]$`1-Mo F/U Ax`)) %>%
      group_by(`Participant Study ID`) %>%
      summarize(t4_cum_mind_min = sum(Duration, na.rm = TRUE),
                t4_cum_mind_sess = n())

    t4_all <- dat %>%
      filter(`Participant Study ID` == .x,
             between(`Sessions Time`,
                     timeline_dat[timeline_dat$`Participant Study ID` == .x,][1,]$`Code Activation`,
                     timeline_dat[timeline_dat$`Participant Study ID` == .x,][1,]$`1-Mo F/U Ax`)) %>%
      group_by(`Participant Study ID`) %>%
      summarize(t4_all_hs_sess = n())

    full_join(all_mm, all_hs, by = "Participant Study ID") %>%
      full_join(mo1_mm, by = "Participant Study ID") %>%
      full_join(mo1_all, by = "Participant Study ID") %>%
      full_join(mo2_mm, by = "Participant Study ID") %>%
      full_join(mo2_all, by = "Participant Study ID") %>%
      full_join(mo3_mm, by = "Participant Study ID") %>%
      full_join(mo3_all, by = "Participant Study ID") %>%
      full_join(t3_cum_mm, by = "Participant Study ID") %>%
      full_join(t3_all, by = "Participant Study ID") %>%
      full_join(t4_cum_mm, by = "Participant Study ID") %>%
      full_join(t4_all, by = "Participant Study ID") %>%
      mutate(
        across(.cols = everything(),
               .fns = ~ replace_na(.x, 0))
      )
  })

# merge with timeline data
adherence_dat <- full_join(adherence_dat, timeline_dat, by = "Participant Study ID")

# deal with missing values
adherence_dat <- adherence_dat %>%
  mutate(
   across(
    .cols = c(m1_mind_min, m1_mind_sess, m1_all_hs_sess),
    .fns = ~ ifelse(is.na(`Midpoint Ax`), NA, .x)),
   across(
    .cols = c(m2_mind_min, m2_mind_sess, m2_all_hs_sess, t3_cum_mind_min, t3_cum_mind_sess, t3_all_hs_sess),
    .fns = ~ ifelse(
      is.na(`Post Ax`), NA, .x)),
   across(
     .cols = c(m3_mind_min, m3_mind_sess, m3_all_hs_sess, t4_cum_mind_min, t4_cum_mind_sess, t4_all_hs_sess),
     .fns = ~ ifelse(
       is.na(`1-Mo F/U Ax`),
       NA,
       .x
  ))
)

# create average variables
adherence_dat <- adherence_dat %>%
  mutate(all_avg_mind_sess_len = all_mind_min / all_mind_sess,
         m1_avg_mind_sess_len = m1_mind_min / m1_mind_sess,
         m2_avg_mind_sess_len = m2_mind_min / m2_mind_sess,
         m3_avg_mind_sess_len = m3_mind_min / m3_mind_sess,
         t3_avg_mind_sess_len = t3_cum_mind_min / t3_cum_mind_sess,
         t4_avg_mind_sess_len = t4_cum_mind_min / t4_cum_mind_sess,
         all_avg_mind_sess_len = all_mind_min / all_mind_sess) %>%
  relocate(`Participant Study ID`, all_mind_min, all_mind_sess, all_avg_mind_sess_len,
           all_hs_sess, m1_mind_min, m1_mind_sess, m1_all_hs_sess,
           m1_avg_mind_sess_len, m2_mind_min, m2_mind_sess, m2_all_hs_sess, m2_avg_mind_sess_len,
           m3_mind_min, m3_mind_sess, m3_all_hs_sess, m3_avg_mind_sess_len,
           t3_cum_mind_min, t3_cum_mind_sess, t3_all_hs_sess, t3_avg_mind_sess_len,
           t4_cum_mind_min, t4_cum_mind_sess, t4_all_hs_sess, t4_avg_mind_sess_len) %>%
  mutate(across(.cols = c(all_avg_mind_sess_len,
                          m1_avg_mind_sess_len,
                          m2_avg_mind_sess_len,
                          m3_avg_mind_sess_len,
                          t3_avg_mind_sess_len,
                          t4_avg_mind_sess_len,
                          all_avg_mind_sess_len), .fns = ~ ifelse(is.nan(.x), 0, .x)))

# remove last row, which is a dublicate
adherence_dat <- adherence_dat[-nrow(adherence_dat),]

# save csv file
write_csv(adherence_dat, "adherence_metric_data.csv")

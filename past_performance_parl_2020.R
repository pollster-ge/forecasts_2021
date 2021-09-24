library(tidyverse)
library(stringr)
library(extrafont)
library(lubridate)
library(readxl)
library(mgcv)
library(extrafont)
library(ggiraph)

options(mc.cores = parallel::detectCores())

setwd("D:\\Dropbox\\pollster.ge\\Geo Local 2021\\pollster_quality")

# polls <- 
read_xlsx("presidential_polls.xlsx", sheet=1) %>%
  mutate(wave_id = as.numeric(row.names(.)))%>%
  pivot_longer(-c(wave_id, `Field last day`, Pollster, Lean))%>%
  setNames(., c("field_last_day", "pollster", "lean", "wave_id", "party", "proportion"))%>%
  mutate(
    party = str_replace(party, "GD Candidate / Salome Zourabichvili", "GD"),
    party = str_replace(party, "UNM Candidate / Grigol Vashadze", "UNM"),
    party = str_replace(party, "MLEG Candidate / Davit Bakradze", "EUROGEO"),
    party = str_replace(party, "Zurab Japaridze", "GIRCHI"),
    party = str_replace(party, "Davit Usupashvili", "LELO"),
    party = str_replace(party, "Giorgi Margvelashvili", "OTHER"),
    party = str_replace(party, "Shalva Natelashvili", "LABOR"),
    party = str_replace(party, "APG Candidate\\(s\\)", "APG"),
    party = str_replace(party, "Other", "OTHER"),
    party = str_replace(party, "Undecided", "UNDECIDED"),
    field_last_day = as.Date(field_last_day),
  )%>%
  filter(!party %in% "UNDECIDED")%>%
  group_by(wave_id)%>%
  mutate(
    ## Calculate proportions per each poll, by excluding item nonresponse
    ## No Party, Don't Know, refusal, undecided
    proportion = proportion/sum(proportion, na.rm=T),
  )%>%
  ungroup()%>%
  ## Only select GD, UNM, ERUOGEO, GIRCHI, LABOR, and LELO as they (or their parents properly
  ## fielded candidates in the 2018 presidential elections)
  filter(party %in% c("GD", "UNM", "EUROGEO", "GIRCHI", "LELO", "LABOR")) %>%
  group_by(pollster, party)%>%
  arrange(field_last_day) %>%
  ## Do not use exist polls for evaluating the quality of polls
  ## Use polls that were conducted since last national elections (2017 municipal)
  filter(field_last_day > "2017-10-21" & field_last_day <= "2018-10-28")%>%
  slice(which.max(as.Date(field_last_day))) %>%
  ungroup() -> polls

## Add election results of the first round

election_results <- data.frame(
  "GD" = 0.3864,
  "UNM" = 0.3773,
  "EUROGEO" = 0.1097,
  "LABOR" = 0.0374,
  "LELO" = 0.0226,
  "GIRCHI" = 0.0226
) %>%
  mutate(id = n())%>%
  pivot_longer(-id, names_to = "party", values_to = "result")

## 1. Weight to compensate for pollster bias per each political party

#### Weight factor is election value/last estimate for pollsters

#### Weight factor is election value/average of last estimates for lean

polls %>%
  left_join(election_results, by = "party") %>%
  group_by(lean, party)%>%
  mutate(
    weight = result/mean(proportion, na.rm=T)
  ) %>% 
  select(lean, party, weight) %>%
  distinct() %>%
  rename(pollster=lean) -> pollster_lean

polls %>%
  left_join(election_results, by = "party") %>%
  group_by(pollster)%>% # Use these for existing polls
  mutate(
    weight = result/mean(proportion, na.rm=T)
  ) %>% 
  select(pollster, party, weight) %>%
  distinct() -> pollster_own

pollster_lean %>%
  bind_rows(pollster_own) %>%
  write_csv("pollster_weight_2018.csv")

### Pollster bias, normalized by election results

polls %>%
  left_join(election_results, by = "party") %>%
  mutate(
    error = result-proportion,
  ) %>%
  group_by(pollster) %>%
  summarize(
    rmse_norm = sqrt(mean((result-proportion)^2, na.rm=T))/mean(result),
    weight = 1-rmse_norm,
  )%>%write.csv()

polls %>%
  left_join(election_results, by = "party") %>%
  mutate(
    error = result-proportion,
  ) %>%
  group_by(lean) %>%
  summarize(
    rmse_norm = sqrt(mean((result-proportion)^2, na.rm=T))/mean(result),
    weight = 1-rmse_norm,
  )%>%write.csv()



### Polls including exit polls
polls %>%
  left_join(election_results, by = "party") %>%
  # Calculate errors by 1%+parties
  mutate(
    error = result-proportion,
  ) %>%
  group_by(pollster) %>%
  summarize(
    rmse = sqrt(mean((result-proportion)^2, na.rm=T))
  )%>%write.csv()




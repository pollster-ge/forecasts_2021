library(tidyverse)
library(stringr)
library(extrafont)
library(lubridate)
library(readxl)
library(mgcv)
library(extrafont)
library(ggiraph)
library(KFAS)

options(mc.cores = parallel::detectCores())

setwd("D:\\Dropbox\\pollster.ge\\Geo Local 2021\\polls")
### 

### Calculate point estimates using GAM smoothing
### (incorporates election results and all publicly available opinion polls)
### First set up full data

polls <- read_excel("Parliamentary-2020_Local_2021_exitpolls-updated_09.09.2021.xlsx", sheet = "data")%>%
  # This filters out elections: 
  # filter(!WAVEID %in% c("W1", "W5"))%>%
  filter(!PARTYCODE %in% c("DK",
                           "RA",
                           "NOPARTY",
                           "NOTASKED",
                           "NOTDECIDED",
                           "REFUSE",
                           "AGAINST",
                           "UNDECIDED"
  ))%>%
  mutate(
    PARTYCODE = case_when(
    ### Shenebis Modzraoba merged to Lelo
      PARTYCODE == "SHENEBA" ~ "LELO",
      T ~ as.character(PARTYCODE)
    ),
    PARTYCODE = case_when(
    ### Free Democrats merged with European Georgia 
      PARTYCODE == "FREEDEM|Free Democrats" ~ "EUROGEO",
      T ~ as.character(PARTYCODE)
    ),
    field_last_day = as.Date(`Field_last_day`),
    year = lubridate::year(field_last_day),
    month = lubridate::month(field_last_day),
    day = lubridate::day(field_last_day),
    time = as.numeric(field_last_day),
    mode = case_when(
      Methods == "CATI" ~ "telephone",
      T ~ "f2f"
    ),
  )%>%
  group_by(WAVEID)%>%
  mutate(
  ## Calculate proportions per each poll, by excluding item nonresponse
  ## No Party, Don't Know, refusal, undecided
    proportion = Percent/sum(Percent),
  )%>%
  group_by(PARTYCODE)%>%
  arrange(field_last_day)%>%
  ungroup()%>%
  rename(
    "pollster"=`Source...7`,
    "data_source"=`Source...16`
  )%>%
  mutate(pollster = case_when(
    grepl("IPSOS", pollster) ~ "IPSOS",
    grepl("Edison", pollster) ~ "ER",
    grepl("NDI", pollster) ~ "NDI",
    grepl("CRRC", pollster) ~ "CRRC",
    grepl("IRI", pollster) ~ "IRI",
    grepl("GfK", pollster) ~ "GfK",
    grepl("GORBI|Gorbi", pollster) ~ "Gorbi",
    grepl("Survation", pollster) ~ "Survation",
    grepl("CEC", pollster) ~ "CEC",
    T ~ "Other"
  ))%>%
  select(!c("Field_last_day", "Percent", "Avarege_ME", "Source_KA", "Source_EN", "Date_KA"))

## Post-mortem analysis of the 2018 polls

pollsters_pa <- read_excel("new_pollsters_weights_parl.xlsx", sheet = "pollsters")

weights_pa <- read_excel("new_pollsters_weights_parl.xlsx", sheet = "weights")

## Join with pollster weights data; pre 2020 parliamentary

polls %>%
  filter(field_last_day < '2020-10-31')%>%
  left_join(pollsters_pa, by=c("Polling_initiative")) %>%
  left_join(weights_pa, by=c("weight_type", "PARTYCODE")) %>%
  group_by(Polling_initiative)%>%
  mutate(
    weight = ifelse(is.na(weight), mean(weight, na.rm=T), weight)
  ) %>%
  ungroup()-> polls_wt_pa

polls %>%
  filter(field_last_day < '2020-10-31' & field_last_day >= "2019-10-31")%>%
  group_by(WAVEID)%>%
  count()%>%write.csv()
  
### Weighted sum by date

polls_wt_pa %>%
  mutate(
    weight_dt=as.Date("2020-10-31")-field_last_day
  )%>%
  group_by(PARTYCODE)%>%
  summarize(
    proportion = weighted.mean(proportion, 1/as.numeric(weight_dt), na.rm=T)
  )%>%
  filter(PARTYCODE %in% c(c("GD", "UNM", "LELO", "EUROGEO", "GIRCHI", "LABOR", "APG", "NEWGEORGIA", "CIVICMO")))%>%
  arrange(desc(proportion))

polls_wt_pa %>%
  mutate(
    weight_dt=1/(as.numeric(as.Date("2020-10-31")-field_last_day)),
    weight_dt = weight_dt/mean(weight_dt, na.rm=T), # Normalized towards mean
    weight_comp = weight*as.numeric(weight_dt),
    weight_comp = weight_comp/mean(weight_comp, na.rm=T), # Normalized towards mean
    weight_bias = weight/mean(weight, na.rm=T),
    weight_dt_pollster = pollster_weight*as.numeric(weight_dt),
    weight_dt_pollster_bias = pollster_weight*weight/mean(pollster_weight*weight, na.rm=T)
  ) -> polls_wt_pa

### Read covariates

usd_rate <- read_csv("covariates/rates.csv")%>%
  setNames(., c("currency", "field_last_day", "rate"))

inflation <- read_csv("covariates/inflation.csv")%>%
  setNames(., c("year", "month", "inflation"))

covid_deaths <- readxl::read_excel("covariates/src.xlsx", sheet="detailed")%>%   select(date, new_cases, new_deaths)%>%  rename(field_last_day=date)

polls_wt_pa %>%
  left_join(usd_rate, by="field_last_day")%>%
  left_join(covid_deaths, by="field_last_day")%>%
  left_join(inflation, by=c("year", "month"))%>%
  mutate(new_cases = ifelse(is.na(new_cases), 0, as.numeric(new_cases)),
         new_deaths = ifelse(is.na(new_deaths), 0, as.numeric(new_deaths)),
         )-> polls_wt_pa



### Only time series, weighted by pollster errors

predict_value <- function(x) {
  polls_wt_pa%>%
    filter(field_last_day < '2020-10-31')%>%
    filter(PARTYCODE == x) -> party_subset
  
  tp_pollster <- gam(proportion ~ s(time, bs = "cs", fx=T)+Polling_initiative+mode+rate+inflation+new_deaths,
                     data = party_subset, weights = weight_dt_pollster)
  
  fitted_values <- tp_pollster$fitted.values %>%
    data.frame()
  
  fitted_values <- cbind(fitted_values, party_subset$field_last_day)%>%
    setNames(c("fitted_values", "field_last_day"))
  
  ## Predict value for the last day of the last poll's fieldwork,
  ## Face-to-face mode and most accurate pollster (Edison Research)
  test_data = data.frame(
    year = lubridate::year(party_subset$field_last_day[nrow(party_subset)]),
    month = lubridate::month(party_subset$field_last_day[nrow(party_subset)]),
    day = lubridate::day(party_subset$field_last_day[nrow(party_subset)]),
    time = as.numeric(party_subset$time[nrow(party_subset)]),
    rate = as.numeric(party_subset$rate[nrow(party_subset)]),
    new_deaths = as.numeric(party_subset$new_deaths[nrow(party_subset)]),
    inflation = as.numeric(party_subset$inflation[nrow(party_subset)]),
    Polling_initiative="Edison Research / Formula",
    mode = "f2f"
  )
  
  predict(tp_pollster, test_data, se.fit = T)
  
}

gd <- predict_value("GD") %>%data.frame() %>% mutate(party = "GD")
unm <- predict_value("UNM")  %>%data.frame()  %>% mutate(party = "UNM")
lelo <- predict_value("LELO")  %>%data.frame()  %>% mutate(party = "Lelo")
eurogeo <- predict_value("EUROGEO")  %>%data.frame()  %>% mutate(party = "European Georgia")
girchi <- predict_value("GIRCHI")  %>%data.frame()  %>% mutate(party = "Girchi")
labor <- predict_value("LABOR")  %>%data.frame()  %>% mutate(party = "Labor")
apg <- predict_value("APG")  %>%data.frame()  %>% mutate(party = "APG")
agmashenebeli <- predict_value("NEWGEORGIA")  %>%data.frame()  %>% mutate(party = "Aghmashenebeli")
civicmo <- predict_value("CIVICMO")  %>%data.frame()  %>% mutate(party = "Mokalakeebi")

parties <- rbind(gd, unm, lelo, eurogeo, girchi, labor, apg, agmashenebeli)

write.csv(parties)

election_results <- data.frame("TETREBI"=0.11, "EUROGEO"=3.79, "DMUG"=0.85, "TRIBUNE"=0.51, "UNM"=27.18, "FUTGEOR"=0.11, "MECHIAURI"=0.14, "APG"=3.14, "GREENS"=0.07, "LABOR"=1, "LABSOC"=0.03, "MOVFRGEO"=0.04, "REFORMER"=0.11, "GUNAVA"=0.11, "NEWCDM"=0.02, "VICTORIOUSGEORGIA"=0.19, "INDUSTRIALISTS"=0.05, "SOLIDARITY"=0.43, "GEORGIA"=0.06, "FREEGEO"=0.33, "NEWFORCE"=0.08, "CIVICMO"=1.33, "Free Democrats"=0.27, "FORJUSTICE"=0.1, "NEWGEORGIA"=3.15, "GEOPESV"=0.1, "CHANGGEO"=0.07, "FREEDOM"=0.15, "PPARTY"=0.05, "EDP"=0.02, "SOCJUST"=0.15, "GIRCHI"=2.89, "GD"=48.22, "REFORMERS"=0.09, "ZVIADI"=0.08, "GEOIDEE"=0.43, "ERDEMMO"=0.25, "SOCDECS"=0.23, "CONSERVATIVE"=0.16, "ARCHSAMSH"=0.03, "GEOTROUPE"=0.05, "PROGGEO"=0.05, "VETERANS"=0.17, "VECTOR"=0.02, "TRADITIONALISTS"=0.02, "POPCDU"=0.02, "GEORGIANMARCH"=0.25, "LELO"=3.15, "ORDER"=0.03, "GEODEVELP"=0.08) %>%
  mutate(id = n())%>%
  pivot_longer(-id, names_to = "party", values_to = "result") %>%
  mutate(
    result = result/100
  )

parties %>%
  filter(party %in% c("GD", "UNM")) %>%
  left_join(election_results, by = "party")%>%
  mutate(
    error = result-fit,
  ) %>%
  summarize(
    rmse = sqrt(mean((result-fit)^2, na.rm=T))
  )
  
  
parties %>%
  mutate(party=forcats::fct_reorder(party, fit))%>%
  ggplot(aes(fit, party, fill=party, label=round(fit*100, 0)))+
  geom_col()+
  # geom_linerange(aes(xmin=fit-se.fit*1.96, xmax=fit+se.fit*1.96),
  #               position = position_stack())+
  geom_text(position = position_stack(vjust = 0.5), family="FiraGO", color= "white",size=4, fontface = "bold")+
  scale_fill_manual(values=rev(c("#195ea2", "#dc082b", "#f0ce0d","#e7b031","#327f37",
                                 "#e1073b", "#fc5000", "#0f5cbb")))+
  theme_bw()

## Bayes

test_data = data.frame(
  year = lubridate::year(party_subset$field_last_day[nrow(party_subset)]),
  month = lubridate::month(party_subset$field_last_day[nrow(party_subset)]),
  day = lubridate::day(party_subset$field_last_day[nrow(party_subset)]),
  time = as.numeric(party_subset$time[nrow(party_subset)]),
  Polling_initiative="Edison Research / Formula",
  rate = as.numeric(party_subset$rate[nrow(party_subset)]),
  new_deaths = as.numeric(party_subset$new_deaths[nrow(party_subset)]),
  inflation = as.numeric(party_subset$inflation[nrow(party_subset)]),
  mode = "f2f"
)

library(rstanarm)

predit_bayes <- function(x) {
  polls_wt_pa%>%
    filter(field_last_day < '2020-10-31')%>%
    filter(PARTYCODE == x) -> party_subset
  
  mod <- stan_gamm4(proportion ~ s(time)+Polling_initiative+mode+rate+inflation+new_deaths,
                    data = party_subset, weights = party_subset$weight_dt_pollster,
                    chains = 4, iter = 10000)
  posterior_predict(mod, newdata = test_data) %>%
    data.frame()%>%
    setNames(., "preds")%>%
    summarize(
      median(preds)
    )
}

gd <- predit_bayes("GD") %>%data.frame() %>% mutate(party = "GD")
unm <- predit_bayes("UNM")  %>%data.frame()  %>% mutate(party = "UNM")
lelo <- predit_bayes("LELO")  %>%data.frame()  %>% mutate(party = "Lelo")
eurogeo <- predit_bayes("EUROGEO")  %>%data.frame()  %>% mutate(party = "European Georgia")
girchi <- predit_bayes("GIRCHI")  %>%data.frame()  %>% mutate(party = "Girchi")
labor <- predit_bayes("LABOR")  %>%data.frame()  %>% mutate(party = "Labor")
apg <- predit_bayes("APG")  %>%data.frame()  %>% mutate(party = "APG")
agmashenebeli <- predit_bayes("NEWGEORGIA")  %>%data.frame()  %>% mutate(party = "Aghmashenebeli")
civicmo <- predit_bayes("CIVICMO")  %>%data.frame()  %>% mutate(party = "Mokalakeebi")

parties_bayes <- rbind(gd, unm, lelo, eurogeo, girchi, labor, apg, agmashenebeli, civicmo) %>%
  setNames(., c("pred", "party"))

write.csv(parties_bayes)

parties_bayes %>%
  filter(party %in% c("GD", "UNM")) %>%
  left_join(election_results, by = "party")%>%
  mutate(
    error = result-pred,
  ) %>%
  summarize(
    rmse = sqrt(mean((result-pred)^2, na.rm=T))
  )

posterior_predict(mod, newdata = test_data) %>%
  data.frame()%>%
  setNames(., "preds")%>%
  summarize(
    median(preds)
  )

posterior_predict(mod, newdata = test_data) %>%
  data.frame()%>%
  setNames(., "preds")%>%
  ggplot(aes(preds))+
  geom_density()+
  annotate("text", x = median(y_nohs$preds), y = 0.009,  color="#0077be", label = sprintf("%0.f", round(median(y_nohs$preds)*100, digits = 0)))+
  geom_vline(xintercept = median(y_nohs$preds), color="#0077be", linetype = "longdash")



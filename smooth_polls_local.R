library(tidyverse)
library(stringr)
library(lubridate)
library(readxl)
library(mgcv)
library(dlm)
library(extrafont)
library(ggiraph)

options(mc.cores = parallel::detectCores())

setwd("D:\\Dropbox\\pollster.ge\\Geo Local 2021\\polls")

theme_ef <- function () { 
  theme_minimal(base_size=12) %+replace%
    theme(
      axis.title = element_blank(),
      title = element_blank(),
      axis.text.x = element_blank(),
      panel.grid = element_blank(),
      strip.text = element_text(family = "FiraGO", face="bold", size = 14),
      text = element_text(family= "FiraGO"),
      plot.title = element_text(size=14, face="bold", family="FiraGO"),
      plot.title.position = "plot",
      plot.caption.position =  "plot",
      plot.subtitle = element_text(size=12, family="FiraGO"),
      axis.text = element_text(size=12, family="FiraGO", color = "black"),
      legend.position = "none"
    )
}

### 

### Calculate point estimates using GAM smoothing
### (incorporates election results and all publicly available opinion polls)
### First set up full data

polls <- read_excel("Parliamentary-2020_Local_2021_exitpolls-updated_09.28.2021.xlsx", sheet = "data")%>%
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
  filter(field_last_day <= '2020-11-01')%>%
  left_join(pollsters_pa, by=c("Polling_initiative")) %>%
  left_join(weights_pa, by=c("weight_type", "PARTYCODE")) %>%
  group_by(Polling_initiative)%>%
  ungroup()-> polls_wt_pa


### Change this

today <- "2021-10-02"

### Weighted sum by date

polls_wt_pa %>%
  mutate(
    weight_dt=1/(as.numeric(as.Date(today)-field_last_day)),
    weight_dt_pollster = pollster_weight*as.numeric(weight_dt),
  ) -> polls_wt_pa


### Use parliamentary pollster ratings for the 2021 local polls

pollsters_lc <- read_excel("new_pollsters_weights_loc.xlsx", sheet = "pollsters")

weights_lc <- read_excel("new_pollsters_weights_loc.xlsx", sheet = "weights")

### post election polls

polls %>%
  filter(field_last_day > '2020-11-01')%>%
  # Assign GIRCHI_MF previous scores
  mutate(
    PARTYCODE = case_when(
      PARTYCODE == "GIRCHI" ~ "GIRCHI_VK",
      PARTYCODE == "GIRCHI_MF" ~ "GIRCHI",
      T ~ PARTYCODE
    ) 
  ) %>%
  # USE CRRC/NDI weights for CB 2020
  mutate(Polling_initiative = str_replace(Polling_initiative, "CRRC / CB 2020", "NDI/CRRC")) %>%
  left_join(weights_lc, by=c("Polling_initiative")) %>%
  ## In case a party does not have a weight, assign the average of corresponding
  ## pollster's weights
  group_by(Polling_initiative)%>%
  mutate(
    weight_dt=1/(as.numeric(as.Date(today)-field_last_day)),
    weight_dt_pollster = pollster_weight*as.numeric(weight_dt),
    
  ) %>%
  ungroup()-> polls_wt_lc

polls_wt_pa %>%
  bind_rows(polls_wt_lc) -> polls_wt


### Read covariates

usd_rate <- read_csv("covariates/rates.csv")%>%
  setNames(., c("currency", "field_last_day", "rate"))

inflation <- read_csv("covariates/inflation.csv")%>%
  setNames(., c("year", "month", "inflation"))

covid_deaths <- readxl::read_excel("covariates/src.xlsx", sheet="detailed")%>%
  select(date, new_cases, new_deaths)%>%  rename(field_last_day=date)

polls_wt %>%
  left_join(usd_rate, by="field_last_day")%>%
  left_join(covid_deaths, by="field_last_day")%>%
  left_join(inflation, by=c("year", "month"))%>%
  mutate(new_deaths = ifelse(is.na(new_deaths), 0, as.numeric(new_deaths)),
         new_cases = ifelse(is.na(new_cases), 0, as.numeric(new_cases)),
         inflation = ifelse(is.na(inflation), 101.3492, as.numeric(101.3492)),
  )-> polls_wt

### Only time series, weighted by pollster errors

predict_value <- function(x) {
  polls_wt%>%
    filter(PARTYCODE == x) -> party_subset
  
  tp_pollster <- gam(proportion ~ s(time, bs = "cs", fx=T)+Polling_initiative+mode+rate+inflation+new_deaths+new_cases,
                     data = party_subset, weights=weight_dt_pollster)
  
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
    # time = as.numeric(party_subset$time[nrow(party_subset)]),
    time = as.numeric(as.Date(today)),
    rate = as.numeric(party_subset$rate[nrow(party_subset)]),
    new_cases = as.numeric(party_subset$new_cases[nrow(party_subset)]),
    new_deaths = as.numeric(party_subset$new_deaths[nrow(party_subset)]),
    # new_cases = as.numeric(2000),
    # new_deaths = as.numeric(35),
    inflation = as.numeric(party_subset$inflation[nrow(party_subset)]),
    Polling_initiative="Edison Research / Formula",
    mode = "f2f"
  )
  
  predict(tp_pollster, test_data, se.fit = T)
  
}

predict_small <- function(x) {
  polls_wt%>%
    # filter(field_last_day < '2021-07-25')%>%
    filter(PARTYCODE == x) -> party_subset
  
  tp_pollster <- gam(proportion ~ s(time, bs = "cs", k=3),
                     data = party_subset, weights=weight_dt)
  
  fitted_values <- tp_pollster$fitted.values %>%
    data.frame()
  
  fitted_values <- cbind(fitted_values, party_subset$field_last_day)%>%
    setNames(c("fitted_values", "field_last_day"))
  
  test_data = data.frame(
    year = lubridate::year(party_subset$field_last_day[nrow(party_subset)]),
    month = lubridate::month(party_subset$field_last_day[nrow(party_subset)]),
    day = lubridate::day(party_subset$field_last_day[nrow(party_subset)]),
    # time = as.numeric(party_subset$time[nrow(party_subset)]),
    time = as.numeric(as.Date(today)),
    rate = as.numeric(party_subset$rate[nrow(party_subset)]),
    new_cases = as.numeric(party_subset$new_cases[nrow(party_subset)]),
    new_deaths = as.numeric(party_subset$new_deaths[nrow(party_subset)]),
    # new_cases = as.numeric(2000),
    # new_deaths = as.numeric(35),
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
agmashenebeli <- predict_value("NEWGEORGIA")  %>%data.frame()  %>% mutate(party = "Third Way - Aghmashenebeli")
civicmo <- predict_value("CIVICMO")  %>%data.frame()  %>% mutate(party = "Mokalakeebi")
forgeo <- predict_small("FORGEO")  %>%data.frame()  %>% mutate(party = "For Georgia")
forpeople <- predict_small("FORPEOPLE")  %>%data.frame()  %>% mutate(party = "For People")

parties <- rbind(gd, unm, lelo, eurogeo, girchi, labor, apg, agmashenebeli, civicmo, forgeo, forpeople)

write.csv(parties)

# EN model
parties %>%
  mutate(party=forcats::fct_reorder(party, fit))%>%
  ggplot(aes(fit, party, fill=party, label=round(fit*100, 0)))+
  geom_col_interactive(aes(tooltip=paste0(party, ": ", round(fit*100, 0), "%")))+
  geom_text(position = position_stack(vjust = 0.5), family="FiraGO", color= "white",size=4, fontface = "bold")+
  scale_fill_manual(values=rev(c("#195ea2", "#dc082b", "#250244", "#f0ce0d", "#e1073b", "#327f37", "#ea6b24",
                                 "#0f5cbb", "#fc5000", "#e7b031", "#8ac650")))+
  theme_ef() -> polling_model

polling_model

tooltip_css <- "background-color:white;font-family:FiraGO;padding:10px;border-radius:10px 20px 10px 20px;"

gg_a <- girafe(ggobj=polling_model, width_svg = 9, height_svg = 6)
gg_a <- girafe_options(gg_a, opts_tooltip(css = tooltip_css, opacity = .75))
htmlwidgets::saveWidget(gg_a, "D:\\Dropbox\\pollster.ge\\Geo Local 2021\\polls\\interactive\\polling_model_en.html")

## KA model

parties %>%
  mutate(
    party = factor(party, levels=c("GD", "UNM", "Lelo", "European Georgia", "Girchi", "Labor", "APG", "Third Way - Aghmashenebeli",
                                   "Mokalakeebi", "For Georgia", "For People"),
                   labels = c("ქართული ოცნება", "ენმ", "ლელო", "ევროპული საქართველო", "გირჩი", "ლეიბორისტები",
                              "პატრიოტთა ალიანსი", "მესამე გზა-აღმაშენებელი", "მოქალაქეები", "საქართველოსთვის", "ხალხისთვის")),
    party=forcats::fct_reorder(party, fit),
         )%>%
  ggplot(aes(fit, party, fill=party, label=round(fit*100, 0)))+
  geom_col_interactive(aes(tooltip=paste0(party, ": ", round(fit*100, 0), "%")))+
  # geom_linerange(aes(xmin=fit-se.fit*1.96, xmax=fit+se.fit*1.96),
  #               position = position_stack())+
  geom_text(position = position_stack(vjust = 0.5), family="FiraGO", color= "white",size=4, fontface = "bold")+
  scale_fill_manual(values=rev(c("#195ea2", "#dc082b", "#250244", "#f0ce0d", "#e1073b", "#327f37", "#ea6b24",
                                 "#0f5cbb", "#fc5000", "#e7b031", "#8ac650")))+
  theme_ef() -> polling_model

polling_model

tooltip_css <- "background-color:white;font-family:FiraGO;padding:10px;border-radius:10px 20px 10px 20px;"

gg_a <- girafe(ggobj=polling_model, width_svg = 9, height_svg = 6)
gg_a <- girafe_options(gg_a, opts_tooltip(css = tooltip_css, opacity = .75))
htmlwidgets::saveWidget(gg_a, "D:\\Dropbox\\pollster.ge\\Geo Local 2021\\polls\\interactive\\polling_model_ka.html")


## EN trends
polls_wt%>%
  filter(PARTYCODE %in% c("GD", "UNM", "FORGEO", "LABOR", "GIRCHI", "LELO") & field_last_day >= "2018-10-01") %>%
  mutate(PARTYCODE = factor(PARTYCODE, levels=c("GD", "UNM", "FORGEO", "LABOR", "GIRCHI", "LELO"),
                            labels = c("Georgian Dream", "UNM","For Georgia", "Labor", "Girchi (More Freedom)", "Lelo")))%>%
  ggplot(aes(as.Date(field_last_day),  proportion, group=PARTYCODE, color=PARTYCODE, fill=PARTYCODE))+
    geom_smooth(method="gam", formula = y~s(x, k=5, bs = "cs", fx=T), alpha=0.2)+
  scale_y_continuous(labels = scales::percent_format(accuracy = 1), limits = c(0, 1))+
  scale_color_manual(values = c("#195ea2", "#dc082b", "#250244", "#e1073b", "#327f37", "#f0ce0d"))+
  scale_fill_manual(values = c("#195ea2", "#dc082b", "#250244", "#e1073b", "#327f37", "#f0ce0d"))+
  geom_vline(xintercept=as.Date("2020-10-31"),
             color="black", linetype = "longdash")+
  annotate("text", x=as.Date("2020-10-20"), y=0.8, angle=90, 
           label="Parliamentary elections", family = "FiraGO")+
  geom_vline(xintercept=as.Date("2021-02-18"),
             color="black", linetype = "longdash")+
  annotate("text", x=as.Date("2021-02-10"), y=0.8, angle=90, 
           label="Gakharia resignes", family = "FiraGO")+
  geom_vline(xintercept=as.Date("2021-05-29"),
             color="black", linetype = "longdash")+
  annotate("text", x=as.Date("2021-05-19"), y=0.7, angle=90, 
           label="Gakharia starts For Georgia party", family = "FiraGO")+
  geom_vline(xintercept=as.Date("2021-07-28"),
             color="black", linetype = "longdash")+
  annotate("text", x=as.Date("2021-07-18"), y=0.7, angle=90, 
           label="GD leaves Charles Michel Agreement", family = "FiraGO")+
  geom_vline(xintercept=as.Date("2021-09-13"),
             color="black", linetype = "longdash")+
  annotate("text", x=as.Date("2021-09-03"), y=0.7, angle=90, 
           label="SSS leaks", family = "FiraGO")+
  geom_hline(yintercept=0.43,
             color="black", linetype = "longdash")+
  annotate("text", x=as.Date("2020-01-01"), y=0.45,
           label="43% threshold as per Charles Michel agreement", family = "FiraGO")+
  geom_point_interactive(aes(tooltip=paste0(PARTYCODE, ": ", round(proportion*100, 0), "%\n", Polling_initiative, "; ", Date)))+
  theme_ef()+
  theme(
    legend.position = "bottom",
    axis.text.x = element_text()
  ) -> trendline

trendline
gg_a <- girafe(ggobj=trendline, width_svg = 9, height_svg = 6)
gg_a <- girafe_options(gg_a,
                       opts_tooltip(css = tooltip_css, opacity = .75),
                       opts_selection(only_shiny = FALSE, type = "single", css = "stroke:yellow;"),
                       opts_zoom(min = .7, max = 2))
htmlwidgets::saveWidget(gg_a, "D:\\Dropbox\\pollster.ge\\Geo Local 2021\\polls\\interactive\\trendline_en.html")

## KA trends

polls_wt%>%
  filter(PARTYCODE %in% c("GD", "UNM", "FORGEO", "LABOR", "GIRCHI", "APG", "LELO") & field_last_day >= "2018-10-01") %>%
  mutate(PARTYCODE = factor(PARTYCODE, levels=c("GD", "UNM", "FORGEO", "LABOR", "GIRCHI", "APG", "LELO"),
                            labels = c("ქართული ოცნება", "ენმ","საქართველოსთვის", "ლეიბორისტები", "გირჩი (მეტი თავისუფლება)", "პატრიოტთა ალიანსი", "ლელო")))%>%
  ggplot(aes(as.Date(field_last_day),  proportion, group=PARTYCODE, color=PARTYCODE, fill=PARTYCODE))+
  geom_smooth(method="gam", formula = y~s(x, k=5, bs = "cs", fx=T), alpha=0.2)+
  scale_y_continuous(labels = scales::percent_format(accuracy = 1), limits = c(0, 1))+
  scale_color_manual(values = c("#195ea2", "#dc082b", "#250244", "#e1073b", "#327f37", "#e7b031", "#f0ce0d"))+
  scale_fill_manual(values = c("#195ea2", "#dc082b", "#250244", "#e1073b", "#327f37", "#e7b031", "#f0ce0d"))+
  geom_vline(xintercept=as.Date("2020-10-31"),
             color="black", linetype = "longdash")+
  annotate("text", x=as.Date("2020-10-20"), y=0.8, angle=90, 
           label="საპარლამენტო არჩევნები", family = "FiraGO")+
  geom_vline(xintercept=as.Date("2021-02-18"),
             color="black", linetype = "longdash")+
  annotate("text", x=as.Date("2021-02-10"), y=0.8, angle=90, 
           label="გახარია გადადგა", family = "FiraGO")+
  geom_vline(xintercept=as.Date("2021-05-29"),
             color="black", linetype = "longdash")+
  annotate("text", x=as.Date("2021-05-19"), y=0.7, angle=90, 
           label="გახარია ახალ პარტიას აარსებს", family = "FiraGO")+
  geom_vline(xintercept=as.Date("2021-07-28"),
             color="black", linetype = "longdash")+
  annotate("text", x=as.Date("2021-07-18"), y=0.7, angle=90, 
           label="ოცნება ტოვებს მიშელის შეთანხმებას", family = "FiraGO")+
  geom_vline(xintercept=as.Date("2021-09-13"),
             color="black", linetype = "longdash")+
  annotate("text", x=as.Date("2021-09-03"), y=0.7, angle=90, 
           label="სუს-ის ფაილების გაჟონვა", family = "FiraGO")+
  geom_hline(yintercept=0.43,
             color="black", linetype = "longdash")+
  annotate("text", x=as.Date("2020-01-01"), y=0.45,
           label="შარლ მიშელის შეთანხმების 43%-იანი ბარიერის დათქმა", family = "FiraGO")+
  geom_point_interactive(aes(tooltip=paste0(PARTYCODE, ": ", round(proportion*100, 0), "%\n", Polling_initiative, "; ", Date)))+
  theme_ef()+
  theme(
    legend.position = "bottom",
    axis.text.x = element_text()
  ) -> trendline

trendline
gg_a <- girafe(ggobj=trendline, width_svg = 9, height_svg = 6)
gg_a <- girafe_options(gg_a,
                       opts_tooltip(css = tooltip_css, opacity = .75),
                       opts_selection(only_shiny = FALSE, type = "single", css = "stroke:yellow;"),
                       opts_zoom(min = .7, max = 2))
htmlwidgets::saveWidget(gg_a, "D:\\Dropbox\\pollster.ge\\Geo Local 2021\\polls\\interactive\\trendline_ka.html")





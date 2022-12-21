# ----İdxal-----
library(tidyverse)
library(lubridate)
library(inspectdf)

setwd("C:/Users/a.huseynli/Documents/s/Final")
actual_dorecast_tranzit <- readxl::read_xlsx("final_actual_forecast.xlsx", sheet = 2)

actual_dorecast_tranzit <- actual_dorecast_tranzit %>% 
  separate(from_to,into = c("from", "to"), sep = " - ",remove = FALSE) %>% 
  mutate(Date = Date %>% ymd(),
         year = lubridate::year(Date)) %>% 
  select(-Date) %>% 
  select(year, everything())

commodity <- readxl::read_xlsx("C:/Users/a.huseynli/Desktop/Forecast model_28/commodity prices.xlsx", sheet=3); unique(commodity$year) # 192 row
actual_dorecast_tranzit$year <- as.double(actual_dorecast_tranzit$year)
commodity$year <- as.double(commodity$year)
actual_dorecast_tranzit <- left_join(actual_dorecast_tranzit, commodity,by = c("year","yuk_qrupu_tariff"))



macro <- readxl::read_xlsx("C:/Users/a.huseynli/Desktop/Forecast model_28/macro 2010-2026.xlsx"); unique(macro$year) #1,968
olkeler_macro <- readxl::read_xlsx("C:/Users/a.huseynli/Desktop/Forecast model_28/olkeler.xlsx", sheet = 2)

macro <- macro %>% left_join(olkeler_macro, by=c("Location")) %>% select(-Location) %>% 
  rename(Location=Shakir_eng)
macro %>% inspectdf::inspect_na()

macro <- macro %>% select(year,Location,everything())
# from
actual_dorecast_tranzit <- left_join(actual_dorecast_tranzit, macro, by = c("year"="year","from"="Location"))
colnames(actual_dorecast_tranzit)[11:40] <- paste0(colnames(actual_dorecast_tranzit)[11:40],"_from")
# to
actual_dorecast_tranzit <- left_join(actual_dorecast_tranzit, macro, by = c("year"="year","to"="Location"))
colnames(actual_dorecast_tranzit)[41:70] <- paste0(colnames(actual_dorecast_tranzit)[41:70],"_to")

actual_dorecast_tranzit <- actual_dorecast_tranzit %>% arrange(year) %>% arrange(model_id)


# kənd təsərrüfatı 

# Russia - Azerbaijan
actual_dorecast_tranzit$yuk_qrupu_tariff %>% unique()

rus_aze <- actual_dorecast_tranzit %>% filter(from == "Russia" & to == "Azerbaijan" ) %>% 
  mutate(Kend = yuk_qrupu_tariff %>% str_detect(pattern = "fat")) %>% 
  filter(Kend)

corr_rus_aze <- rus_aze %>%
  inspect_cor(method = "spearman") %>%
  filter(col_2 == "tonnaj") %>%
  mutate(corr = round(corr,4)); corr_rus_aze



# meşə, ağac emalı

# Russia - Azerbaijan
actual_dorecast_tranzit$yuk_qrupu_tariff %>% unique()

rus_aze <- actual_dorecast_tranzit %>% filter(from == "Russia" & to == "Azerbaijan" ) %>% 
  mutate(Mese = yuk_qrupu_tariff %>% str_detect(pattern = "Mese")) %>% 
  filter(Mese)

corr_rus_aze <- rus_aze %>%
  inspect_cor(method = "pearson") %>%
  filter(col_2 == "tonnaj") %>%
  mutate(corr = round(corr,4)); corr_rus_aze


# sənaye, metallurgiya

# Russia - Azerbaijan
actual_dorecast_tranzit$yuk_qrupu_tariff %>% unique()

rus_aze_s <- actual_dorecast_tranzit %>% filter(from == "Russia" & to == "Azerbaijan" ) %>% 
  mutate(senaye = yuk_qrupu_tariff %>% str_detect(pattern = "metallurgiya")) %>% 
  filter(senaye)

corr_rus_aze_s <- rus_aze_s %>%
  inspect_cor(method = "pearson") %>%
  filter(col_2 == "tonnaj") %>%
  mutate(corr = round(corr,4)); corr_rus_aze_s

ukr_aze <- actual_dorecast_tranzit %>% filter(from == "Ukraine" & to == "Azerbaijan" ) %>% 
  mutate(senaye = yuk_qrupu_tariff %>% str_detect(pattern = "metallurgiya")) %>% 
  filter(senaye)

corr_ukr_aze <- ukr_aze %>%
  inspect_cor(method = "pearson") %>%
  filter(col_2 == "tonnaj") %>%
  mutate(corr = round(corr,4)); corr_ukr_aze


# qida, ət-süd və balıq məhsulları

# Brazil - Azerbaijan
actual_dorecast_tranzit$yuk_qrupu_tariff %>% unique()

brazil_aze <- actual_dorecast_tranzit %>% filter(from == "Brazil" & to == "Azerbaijan" ) %>% 
  mutate(qida = yuk_qrupu_tariff %>% str_detect(pattern = "Qida")) %>% 
  filter(qida)

corr_brazil_aze <- brazil_aze %>%
  inspect_cor(method = "pearson") %>%
  filter(col_2 == "tonnaj") %>%
  mutate(corr = round(corr,4)); corr_brazil_aze

# Russia - Azerbaijan
actual_dorecast_tranzit$yuk_qrupu_tariff %>% unique()

rus_aze_q <- actual_dorecast_tranzit %>% filter(from == "Russia" & to == "Azerbaijan" ) %>% 
  mutate(qida = yuk_qrupu_tariff %>% str_detect(pattern = "Qida")) %>% 
  filter(qida)

corr_rus_aze_q<- rus_aze_q %>%
  inspect_cor(method = "pearson") %>%
  filter(col_2 == "tonnaj") %>%
  mutate(corr = round(corr,4)); corr_brazil_aze

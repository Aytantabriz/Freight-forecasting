# **** ========================================================================================================================
# LOGICAL REASONING ========================================================================================================================
setwd("C:/Users/a.huseynli/Desktop/Forecast model_28")
library(tidyverse)
library(inspectdf)

idxal <- readxl::read_xlsx("final data/idxal_15.xlsx")
idxal$year <- lubridate::year(idxal$Date)
idxal <- idxal %>% separate(from_to,c('from','to'),sep=' - ',remove=F) 

macro <- readxl::read_xlsx("macro 2010-2026.xlsx"); unique(macro$year)
olkeler_macro <- readxl::read_xlsx("olkeler.xlsx", sheet = 2)
macro <- macro %>% left_join(olkeler_macro, by=c("Location")) %>% select(-Location) %>% 
  rename(Location=Shakir_eng)
remove(olkeler_macro)
idxal <- left_join(idxal, macro, by = c("year","from"="Location"))
colnames(idxal)[11:40] <- paste0(colnames(idxal)[11:40],"_from")
idxal <- left_join(idxal, macro, by = c("year","to"="Location"))
colnames(idxal)[41:70] <- paste0(colnames(idxal)[41:70],"_to")

additional <- readxl::read_xlsx("commodity prices.xlsx", sheet=4) 
idxal <- left_join(idxal, additional,by = c("year","yuk_qrupu_tariff"))

UN <- readxl::read_xlsx("UN.xlsx"); unique(UN$Year)
olkeler_UN <- readxl::read_xlsx("olkeler.xlsx", sheet = 1)
UN <- UN %>% left_join(olkeler_UN, by=c("Gonderen"="Location")) %>% select(-Gonderen) %>% rename(Gonderen=Shakir_eng) %>%
  left_join(olkeler_UN, by=c("Qebul_eden"="Location")) %>% select(-Qebul_eden) %>% rename(Qebul_eden=Shakir_eng)
idxal <- left_join(idxal, UN %>% select(-model_id,-colors),by = c("year"="Year",
                                                                  "yuk_qrupu_tariff"="yuk_qrupu",
                                                                  "from"="Gonderen",
                                                                  "to"="Qebul_eden")) %>%
  rename("UN_ton"="Net_Ton")


# kənd təsərrüfatı 

# Russia - Azerbaijan
idxal$yuk_qrupu_tariff %>% unique()

rus_aze <- idxal %>% filter(from == "Russia" & to == "Azerbaijan" ) %>% 
  mutate(Kend = yuk_qrupu_tariff %>% str_detect(pattern = "nd t")) %>% 
  filter(Kend)

corr_rus_aze <- rus_aze %>%
  inspect_cor(method = "spearman") %>%
  filter(col_2 == "tonnaj") %>%
  mutate(corr = round(corr,4)); corr_rus_aze

# meşə məhsulları 

# Russia - Azerbaijan
idxal$yuk_qrupu_tariff %>% unique()

rus_aze <- idxal %>% filter(from == "Russia" & to == "Azerbaijan" ) %>% 
  mutate(Kend = yuk_qrupu_tariff %>% str_detect(pattern = "ac emal")) %>% 
  filter(Kend)

corr_rus_aze <- rus_aze %>%
  inspect_cor(method = "spearman") %>%
  filter(col_2 == "tonnaj") %>%
  mutate(corr = round(corr,4)); corr_rus_aze


# mineral xammal

# Russia - Azerbaijan
idxal$yuk_qrupu_tariff %>% unique()

rus_aze <- idxal %>% filter(from == "Russia" & to == "Azerbaijan" ) %>% 
  mutate(Kend = yuk_qrupu_tariff %>% str_detect(pattern = "Mineral xammal")) %>% 
  filter(Kend)

corr_rus_aze <- rus_aze %>%
  inspect_cor(method = "spearman") %>%
  filter(col_2 == "tonnaj") %>%
  mutate(corr = round(corr,4)); corr_rus_aze


# kimya sənayesi

# Russia - Azerbaijan
idxal$yuk_qrupu_tariff %>% unique()

rus_aze <- idxal %>% filter(from == "Russia" & to == "Azerbaijan" ) %>% 
  mutate(Kend = yuk_qrupu_tariff %>% str_detect(pattern = "Kimya s")) %>% 
  filter(Kend)

corr_rus_aze <- rus_aze %>%
  inspect_cor(method = "spearman") %>%
  filter(col_2 == "tonnaj") %>%
  mutate(corr = round(corr,4)); corr_rus_aze


# yanacaq məhsulları

# Russia - Azerbaijan
idxal$yuk_qrupu_tariff %>% unique()

rus_aze <- idxal %>% filter(from == "Russia" & to == "Azerbaijan" ) %>% 
  mutate(Kend = yuk_qrupu_tariff %>% str_detect(pattern = "Yanacaq v")) %>% 
  filter(Kend)

corr_rus_aze <- rus_aze %>%
  inspect_cor(method = "spearman") %>%
  filter(col_2 == "tonnaj") %>%
  mutate(corr = round(corr,4)); corr_rus_aze

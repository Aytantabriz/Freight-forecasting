# ============================================ ADY TRANZIT YÜK PROQNOZ MODELİ ============================================
setwd("C:/Users/a.huseynli/Desktop/Forecast model_28")

library(tidyverse)
library(lubridate)
library(timetk)
library(inspectdf)
library(missForest)
library(tidymodels)
library(modeltime)
library(h2o)
library(modeltime.h2o)
library(caTools)
library(highcharter)

# **** ========================================================================================================================
# DATA PREPARATION ========================================================================================================================
# * import data =======================================================================================================
# df2010 <- readxl::read_xlsx("Raw data/2010-2013.xlsx",sheet = 1)
# df2011 <- readxl::read_xlsx("Raw data/2010-2013.xlsx",sheet = 2)
# df2012 <- readxl::read_xlsx("Raw data/2010-2013.xlsx",sheet = 3)
# df2013 <- readxl::read_xlsx("Raw data/2010-2013.xlsx",sheet = 4)
# df_2014 <- readxl::read_xlsx("Raw data/2014.xlsx")
# df_2015_2016 <- readxl::read_xlsx("Raw data/2015-2016.xlsx")
# df_2017_2018 <- readxl::read_xlsx("Raw data/2017-2018.xlsx")
# df_2019_2020 <- readxl::read_xlsx("Raw data/2019-2020.xlsx")
# df_2021 <- readxl::read_xlsx("Raw data/2021.xlsx")
# olkeler <- readxl::read_xlsx("olkeler.xlsx", sheet = 3)
# stansiyalar <- readxl::read_xlsx("stansiyalar.xlsx")
# yuk_qruplari <- readxl::read_xlsx("yuk_qruplari.xlsx", sheet = 2)

# * merging data =======================================================================================================
df_2010_2013 <- rbind(df2010,df2011,df2012,df2013)
df_2014.2021 <- rbind(df_2014,df_2015_2016,df_2017_2018,df_2019_2020,df_2021)

# * colnames =======================================================================================================
colnames(df_2014.2021)[2] <- "hecm_tutumu"
colnames(df_2014.2021)[6] <- "yuk_qrupu"
colnames(df_2014.2021)[12] <- "mesafe"
colnames(df_2014.2021)[13] <- "teyinat_st_kod"
colnames(df_2014.2021)[14] <- "teyinat_st_beyn_kod"
colnames(df_2014.2021)[15] <- "gonderen_st_kod"
colnames(df_2014.2021)[16] <- "gonderen_st_beyn_kod"
colnames(df_2014.2021)[20] <- "gonderen_olke"
colnames(df_2014.2021)[21] <- "teyinat_olke"
colnames(df_2014.2021)[22] <- "teyinat_st"
colnames(df_2014.2021)[23] <- "teyinat_st_beyn"
colnames(df_2014.2021)[24] <- "gonderen_st"
colnames(df_2014.2021)[25] <- "gonderen_st_beyn"
colnames(df_2014.2021)[26] <- "istiqamet"
colnames(df_2014.2021)[29] <- "sifarisci"
colnames(df_2014.2021)[30] <- "sifariscinin adı"

names(df_2010_2013)[3] <- "novaq"
names(df_2010_2013)[4] <- "Vaqon novu"
names(df_2010_2013)[5] <- "gonderen_olke"
names(df_2010_2013)[6] <- "teyinat_olke"
names(df_2010_2013)[7] <- "ETSNQ"
names(df_2010_2013)[8] <- "gonderen_st_kod"
names(df_2010_2013)[9] <- "teyinat_st_kod"
names(df_2010_2013)[10] <- "gonderen_st"
names(df_2010_2013)[11] <- "teyinat_st"
names(df_2010_2013)[12] <- "gonderen_st_beyn_kod"
names(df_2010_2013)[13] <- "teyinat_st_beyn_kod"
names(df_2010_2013)[14] <- "gonderen_st_beyn"
names(df_2010_2013)[15] <- "teyinat_st_beyn"
names(df_2010_2013)[16] <- "Amount"
names(df_2010_2013)[17] <- "Ton"

df_2010_2013$ay <- lubridate::month(df_2010_2013$Date)
df_2010_2013$year <- lubridate::year(df_2010_2013$Date)
df_2010_2013$hecm_tutumu <- NA
df_2010_2013$Qaime <- NA
df_2010_2013$`ETSNQ ad` <- NA
df_2010_2013$`QNQ ad` <- NA
df_2010_2013$yuk_qrupu <- NA
df_2010_2013$mesafe <- NA
df_2010_2013$istiqamet <- NA
df_2010_2013$Ekspeditor <- NA
df_2010_2013$Ekspeditor2 <- NA
df_2010_2013$sifarisci <- NA
df_2010_2013$`sifariscinin adi` <- NA
df_2010_2013 <- df_2010_2013 %>% select(colnames(df_2014.2021))

# * commodity groups =======================================================================================================
df_2010_2013$ETSNQ <- as.numeric(df_2010_2013$ETSNQ)
df_2010_2013 <- df_2010_2013 %>% left_join(yuk_qruplari %>% select(ETSNQ,qnq,yuk_qrupu_tariff), by=c("ETSNQ", "qnq"))
df_2010_2013 %>% inspect_na() %>% View()

df_2010_2013 <- df_2010_2013 %>% left_join(stansiyalar, by=c("gonderen_st"="stansiya")) %>% select(-gonderen_st) %>% rename(gonderen_st=rus)
df_2010_2013 <- df_2010_2013 %>% left_join(stansiyalar, by=c("teyinat_st"="stansiya")) %>% select(-teyinat_st) %>% rename(teyinat_st=rus)

df_2014.2021 <- df_2014.2021 %>% left_join(yuk_qruplari, by=c("ETSNQ", "ETSNQ ad", "qnq", "QNQ ad"))
df_2014.2021 %>% inspect_na() %>% View()
df_2010_2013 <- df_2010_2013 %>% select(colnames(df_2014.2021))
# * merging 2010-2021  =======================================================================================================
df_2010_2021 <- rbind(df_2010_2013, df_2014.2021)

# * countries =======================================================================================================
df_2010_2021 <- left_join(df_2010_2021,olkeler, by=c("gonderen_olke" = "Location")) %>% rename(gonderen_olke_eng=Shakir_eng)
df_2010_2021 <- left_join(df_2010_2021,olkeler, by=c("teyinat_olke"="Location")) %>% rename(teyinat_olke_eng=Shakir_eng)

# * excluding crude oil =======================================================================================================
df_2010_2021$qnq_4 <- substr(df_2010_2021$qnq,1,nchar(df_2010_2021$qnq)-4); unique(df_2010_2021$qnq_4)
df_2010_2021$qnq_4 <- as.double(df_2010_2021$qnq)
df_2010_2021 <- df_2010_2021 %>% filter(ETSNQ != 20100 & qnq_4 !=2709)

# * extracting and grouping useful columns =======================================================================================================
df_2010_2021 <- df_2010_2021 %>% select(year,Rejim,yuk_qrupu_tariff,gonderen_olke_eng,teyinat_olke_eng,Ton,Amount) %>%
  group_by(year,Rejim,yuk_qrupu_tariff,gonderen_olke_eng,teyinat_olke_eng) %>%
  summarise(tonnaj = sum(Ton),
            gelir = sum(Amount)) %>% ungroup() # 12,146 rows; 7 columns

remove(df2010,df2011,df2012,df2013,df_2014,df_2015_2016,df_2017_2018,df_2019_2020,df_2021,
       df_2010_2013,df_2014.2021,stansiyalar)

all <- df_2010_2021
df_2010_2021 %>% writexl::write_xlsx("2010_2021_prep_data.xlsx")

# **** ============================================================================================================================
# ********************************************************************************************************************* ----
all <- readxl::read_xlsx("2010_2021_prep_data.xlsx"); unique(all$year)
all$Rejim %>% unique()
all <- all %>% filter(Rejim=="Tranzit")
all %>% inspect_na()



# * merge commodity price ========================================================================================================================
commodity <- readxl::read_xlsx("commodity prices.xlsx", sheet=3); unique(commodity$year)
all$year <- as.double(all$year)
commodity$year <- as.double(commodity$year)
all <- left_join(all, commodity,by = c("year","yuk_qrupu_tariff"))
all %>% inspect_na()



# * merge UN ========================================================================================================================
UN <- readxl::read_xlsx("UN.xlsx"); unique(UN$Year)
UN %>% inspect_na()

# merge UN to Shakir
all <- left_join(all, UN %>% select(-model_id,-colors),by = c("year"="Year",
                                                                        "yuk_qrupu_tariff"="yuk_qrupu",
                                                                        "gonderen_olke_eng"="Gonderen",
                                                                        "teyinat_olke_eng"="Qebul_eden")) %>% 
  rename("UN_ton"="Net_Ton")
all %>% inspect_na() 
#all %>% filter(is.na(all$UN_ton)) %>% View()



# * merge with macro ========================================================================================================================
macro <- readxl::read_xlsx("macro 2010-2026.xlsx"); unique(macro$year) 
olkeler_macro <- readxl::read_xlsx("olkeler.xlsx", sheet = 2)

macro <- macro %>% left_join(olkeler_macro, by=c("Location")) %>% select(-Location) %>% 
  rename(Location=Shakir_eng)
macro %>% inspect_na()


# from
all <- left_join(all, macro, by = c("year"="year","gonderen_olke_eng"="Location"))
colnames(all)[10:39] <- paste0(colnames(all)[10:39],"_from")
# to
all <- left_join(all, macro, by = c("year"="year","teyinat_olke_eng"="Location"))
colnames(all)[40:69] <- paste0(colnames(all)[40:69],"_to")

inspect_na(all)

remove(olkeler_macro)



# * all final steps ========================================================================================================================
# replacements in colnames
names(all) <- names(all) %>% str_replace_all(" ","_"); glimpse(all)

# Date column
#all$Date <- as.Date(with(all, paste(year, 12, 31, sep = "-")),"%Y-%m-%d")
#all <- all %>% select(Date,everything()); all %>% View()




# * TARIFF ========================================================================================================================
s1 <- all %>% select(year,yuk_qrupu_tariff,gonderen_olke_eng,teyinat_olke_eng,gelir,tonnaj) %>% distinct() %>% 
  group_by(year,yuk_qrupu_tariff,gonderen_olke_eng,teyinat_olke_eng) %>%
  summarise(gelir = sum(gelir),
            tonnaj = sum(tonnaj)) %>%
  mutate(tarif = gelir/tonnaj); glimpse(s1)

s1$tariff <- ifelse(s1$tarif== Inf, 0, s1$tarif)

all <- all %>% left_join(s1 %>% select(year,yuk_qrupu_tariff,gonderen_olke_eng,teyinat_olke_eng,tariff) %>% 
                       distinct(), by=c("year","yuk_qrupu_tariff","gonderen_olke_eng","teyinat_olke_eng"))

all <- all %>% select(-gelir) %>% select(year, Rejim, yuk_qrupu_tariff,gonderen_olke_eng,
                                                           teyinat_olke_eng,tonnaj,tariff, everything())




# * Astara terminalı ========================================================================================================================
corridors <- readxl::read_xlsx("corridors.xlsx")
# from-to column
all <- all %>% mutate(from_to = paste(gonderen_olke_eng,"-",teyinat_olke_eng)); glimpse(all$from_to)

all <- all %>% left_join(corridors %>% select(corridor,from_to) %>% distinct(),by=c("from_to")) %>% 
  select(corridor,everything())

all %>% filter(is.na(corridor)) # NA olmamalıdı
all$corridor %>% unique()

all$Astara_terminali <- ifelse(all$year>=2017 & 
                                          all$corridor %in% c("North-South","South-West","South-East","South"),1,0)

all %>% 
  select(year,corridor,Astara_terminali) %>% 
  filter(Astara_terminali==1) %>% 
  arrange(year) %>% 
  distinct()





# * Covid ========================================================================================================================
all$covid <- ifelse(all$year %in% c(2020,2021,2022.2023,2024),1,0)

all %>% 
  select(year,covid) %>% 
  arrange(year) %>% 
  distinct()


all <- all %>% select(-corridor,-gonderen_olke_eng,-teyinat_olke_eng) %>% 
  select(year,Rejim,yuk_qrupu_tariff,from_to,tonnaj,tariff,commodity_price,
         UN_ton,Astara_terminali,covid,everything())

all %>% inspect_na()



all <- all %>% filter(tonnaj >= 10) # 5619


# **** ========================================================================================================================
# PREPARATION FOR THE MODEL ========================================================================================================================

# * creating unique model id ========================================================================================================================
model_id <- all %>% select(Rejim,yuk_qrupu_tariff,from_to) %>% unique() %>% 
  add_column(model_id = 1:nrow(.))

cleaned <- all %>% merge(model_id, by=c("Rejim","yuk_qrupu_tariff","from_to")) %>% 
  select(-Rejim,-yuk_qrupu_tariff,-from_to) %>% 
  select(model_id, year, tonnaj,commodity_price,UN_ton,everything())




# * remove ids less than 2019 ========================================================================================================================

cleaned %>% select(model_id,year) %>% filter(year > 2019) %>%
  pull(model_id) %>% unique() -> great_2019

df <- cleaned %>% filter(model_id %in% great_2019) # 3801
df$model_id <- as.factor(df$model_id)



# * at least 3 turnover ========================================================================================================================

df %>% group_by(model_id) %>% summarise(count = n()) %>% arrange(count) %>% 
  filter(count >=3) %>% arrange() %>% pull(model_id) %>% unique() -> count
length(count) # 487 #377

df <- df %>% filter(model_id %in% count) # 3146 #2816

df_constant <- df 
df_constant %>% inspect_na() 



# * NA filling ========================================================================================================================
# rec_obj <- recipe(~ ., data = df_constant) %>%
#   step_impute_knn(all_numeric()) %>% prep(stringsAsFactors = FALSE)
# 
# df_constant <- bake(rec_obj, df_constant)
# # df_constant[,-(1:3)] <- df_constant[,-(1:3)] %>% scale() %>% as.data.frame()
# 
# df_constant %>% inspect_na()



# * correlation ========================================================================================================================
# corr <- df_constant %>%
#   select(-model_id) %>%
#   inspect_cor(method = "spearman") %>%
#   filter(col_2 == "tonnaj") %>%
#   mutate(corr = round(corr,4))
# 
# 
# highchart() %>%
#   hc_add_series(
#     corr %>%
#       mutate(corr = round(abs(corr),4)) %>%
#       arrange(desc(corr)),
#     hcaes(y=corr,x=col_1),
#     type="line", color='darkblue', name='corr')
# 


# **** ========================================================================================================================
# H2O MODEL ========================================================================================================================
df_constant_time <- df_constant %>% tk_augment_timeseries_signature()

df_constant_time <- df_constant %>%
  mutate_if(is.ordered, as.character) %>% 
  mutate_if(is.character,as_factor)

df_constant_time %>% glimpse()

# * splitting train and test sets ========================================================================================================================
h2o.init()

train_h2o <- df_constant_time  %>% #filter(year < 2021) %>% 
  as.h2o()
validation_h2o <- df_constant_time %>% filter(year == 2021) %>% as.h2o()
test_h2o <- df_constant_time %>% filter(year == 2021) %>% as.h2o()

y <- "tonnaj"
x <- df_constant_time %>% select(-tonnaj) %>% names()


# * model ========================================================================================================================

model_h2o <- h2o.automl(
  x = x, y = y, 
  training_frame = train_h2o, 
  validation_frame = validation_h2o,
  leaderboard_frame = test_h2o,
  seed = 123, 
  nfolds = 10,
  stopping_metric = "RMSE",
  keep_cross_validation_predictions = TRUE,
  keep_cross_validation_models = TRUE,
  keep_cross_validation_fold_assignment = TRUE,
  verbosity = "debug",
  exclude_algos = "GLM",
  max_runtime_secs = 3000) 


model_h2o@leaderboard %>% as.data.frame() %>% head(10)
h2o_leader <- model_h2o@leader




# * implementing model to test set ========================================================================================================================

pred_h2o <- h2o_leader %>% h2o.predict(test_h2o) 

h2o_leader %>% 
  h2o.rmse(train = T,
           valid = T,
           xval = T)
h2o_leader %>% h2o.r2()

error_tbl <- df_constant_time %>% filter(year==2021) %>% 
  add_column(pred = pred_h2o %>% as_tibble() %>% pull(predict)) %>%
  rename(actual = tonnaj) %>% 
  select(year,actual,pred)

sum(error_tbl$actual)-sum(error_tbl$pred)

library(highcharter)
highchart() %>% 
  hc_xAxis(categories = error_tbl$Date) %>% 
  hc_add_series(data=error_tbl$actual, type='line', color='red', name='Actual') %>% 
  hc_add_series(data=error_tbl$pred, type='line', color='green', name='Predicted') %>% 
  hc_title(text='Predict')


# * save model ========================================================================================================================
model_h2o@leaderboard %>% as_tibble() %>% slice(1) %>% pull(model_id) %>% h2o.getModel() %>% 
  h2o.saveModel(path = "modeller/Tranzit 04.11/")
# "C:\\Users\\e.bakirzada\\Desktop\\Forecast model_28\\modeller\\Tranzit 04.11\\GBM_grid_1_AutoML_1_20220301_165006_model_139"
# "C:\\Users\\e.bakirzada\\Desktop\\Forecast model_28\\modeller\\Tranzit 04.11\\GBM_grid_1_AutoML_1_20220302_101032_model_155"
# "C:\\Users\\a.huseynli\\Desktop\\Forecast model_28\\modeller\\Tranzit 04.11\\GBM_grid__1_AutoML_20220302_133243_model_17"
# "C:\\Users\\a.huseynli\\Desktop\\Forecast model_28\\modeller\\Tranzit 04.11\\GBM_grid__1_AutoML_20220302_150534_model_52"
# **** ----
# FUTURE DATASET ========================================================================================================================
future <- data.frame(model_id = rep((count),each = 5)) %>% 
  add_column(year = rep(c(2022,2023,2024,2025,2026), times = length(count)))



# * merging model_id, commodity prices and UN ========================================================================================================================
future <- future %>%
  merge(model_id %>% select(model_id,yuk_qrupu_tariff,from_to), by = "model_id") %>%
  separate(from_to,c('from','to'),sep=' - ',remove=F) %>%
  left_join(commodity, by=c("year","yuk_qrupu_tariff")) %>%
  left_join(UN %>% select(-c(model_id,colors)),by = c("yuk_qrupu_tariff"="yuk_qrupu","from"="Gonderen","to"="Qebul_eden","year"="Year")) %>%
  rename("UN_ton"="Net_Ton") ; glimpse(future)

# * merging macro ========================================================================================================================
future <- left_join(future, macro, by = c("year","from"="Location"))
colnames(future)[9:37] <- paste0(colnames(future)[9:37],"_from")

future <- left_join(future, macro, by = c("year","to"="Location"))
colnames(future)[38:66] <- paste0(colnames(future)[38:66],"_to")


# * Astara terminali ========================================================================================================================
future <- future %>% left_join(corridors %>% select(corridor,from_to) %>% distinct(),by=c("from_to"="from_to")) %>% select(corridor,everything())
future$Astara_terminali <- ifelse(future$year>=2017 &
                                    future$corridor %in% c("North-South","South-West","South-East","South"),1,0)

future %>%
  select(year,corridor,Astara_terminali) %>%
  filter(Astara_terminali==1) %>%
  arrange(year) %>%
  distinct()



# * Covid ========================================================================================================================
future[future$year %in% c(2020,2021,2022,2023,2024),"covid"] <- 1
future[is.na(future$covid),"covid"] <- 0

future %>%
  select(year,covid) %>%
  arrange(year) %>%
  distinct()



# * Tariff ========================================================================================================================
future <- future %>% left_join(df_constant %>%
                                 filter(year == 2021) %>%
                                 select(model_id,tariff) %>% distinct(),by=c("model_id"="model_id")) %>% 
  left_join(df_constant %>%
              filter(year == 2020) %>%
              select(model_id,tariff) %>% distinct(),by=c("model_id"="model_id")) %>% 
  
  left_join(df_constant %>%
              filter(year == 2019) %>%
              select(model_id,tariff) %>% distinct(),by=c("model_id"="model_id")) %>% 
  mutate(tarif = case_when(
    is.na(tariff.x) & is.na(tariff.y) ~ tariff,
    is.na(tariff.x) ~ tariff.y,
    TRUE ~ tariff.x
  )) %>% 
  select(-tariff.x,-tariff.y,-tariff) %>% 
  rename(tariff = tarif)






#future$Date <- as.Date(with(future, paste(year, 12, 31, sep = "-")),"%Y-%m-%d")
future <- future %>% select(all_of(df_constant %>% select(-tonnaj) %>% names()))




# * NA filling ========================================================================================================================
# inspectdf::inspect_na(future) %>% arrange(desc(pcnt))
# 
# future$model_id <- as.factor(future$model_id)
# rec_obj <- recipe(~ ., data = future) %>%
#   step_impute_knn(all_numeric()) %>% prep(stringsAsFactors = FALSE)
# future <- bake(rec_obj, future)
# future <- future %>% select(colnames(df_constant %>% select(-tonnaj)))
# # future[,-(1:2)] <- future[,-(1:2)] %>% scale() %>% as.data.frame()

future %>% inspect_na() 
# * Time arguments ========================================================================================================================
#future_time <- future %>% tk_augment_timeseries_signature()

future_time <- future %>%
  mutate_if(is.ordered, as.character) %>% 
  mutate_if(is.character,as_factor)

future_time_h2o <- future_time %>% as.h2o()



# **** ========================================================================================================================
# FORECAST ========================================================================================================================


# * new predictions ========================================================================================================================
new_predictions <- h2o_leader %>% 
  h2o.predict(future_time_h2o) %>% 
  as_tibble() %>%
  add_column(year = future_time$year,
             model_id = future_time$model_id) %>% 
  select(year,model_id,predict) %>% 
  rename(tonnaj=predict) %>% 
  mutate(tonnaj = case_when(
    tonnaj < 0 ~ abs(tonnaj),
    TRUE ~ tonnaj
  ))

# * bind actual and predicted ========================================================================================================================
cleaned$model_id <- as.factor(cleaned$model_id)
model_id$model_id <- as.factor(model_id$model_id)

final_actual_forecast <- cleaned %>% select(year,model_id,tonnaj) %>% 
  left_join(model_id, by = c("model_id"))  %>% 
  bind_rows(new_predictions %>% 
              left_join(model_id, by = c("model_id"))) %>% 
  mutate(category = c(rep('Actual', nrow(cleaned)), rep('Predicted',nrow(new_predictions)))) %>% 
  select(year,model_id, tonnaj, Rejim, yuk_qrupu_tariff, from_to, category) %>% 
  arrange(year) %>% 
  arrange(model_id)

final_actual_forecast %>% writexl::write_xlsx("modeller/Tranzit 04.11/final.tranzit,forecast_13.xlsx")

# **** ========================================================================================================================

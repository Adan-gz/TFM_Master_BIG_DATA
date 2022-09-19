

# Detalles ----------------------------------------------------------------

# Script para preparar los datos necesarios para modelizar, así como los datos
# de test del campeonato sin información sobre la VD para llevar a cabo
# la predicción final. Ésta es subida a la web.


# Wrangling ---------------------------------------------------------------

library(dplyr)

## cargamos el dataset
data <- readr::read_csv('data_raw/dengue_features_train.csv') 
labels <- readr::read_csv('data_raw/dengue_labels_train.csv') 
data <- data %>% left_join( labels )

# Ahora ajustamos los de los datos de train para que la información de las features
# sea de la semana t-1, y evitar problemas de lookaheads que afrontaríamos
# en una situación real

data_sj <- data %>% filter( city == 'sj' ) %>% 
  mutate( across( -c(city,year, total_cases), lag ),
          year = lubridate::year( week_start_date ))

data_sj <- data_sj[-1,] ## eliminamos la primera


data_iq <- data %>% filter( city == 'iq' ) %>% 
  mutate( across( -c(city,year, total_cases), lag ),
          year = lubridate::year( week_start_date ))

data_iq <- data_iq[-1,] ## eliminamos la primera


## también aplicamos los lags correspondientesa los datos de test de competicion

test_features <- readr::read_csv('data_raw/dengue_features_test.csv') %>% 
  mutate( total_cases = 1 ) %>% 
  select( all_of( names(data) ) )

test_feat_ori_sj <- test_features %>% filter( city == 'sj' )

test_feat_sj <- test_feat_ori_sj %>% 
  mutate( across( -c(city,year, total_cases), lag ),
          year = lubridate::year( week_start_date ))

test_feat_ori_iq <- test_features %>% filter( city == 'iq' ) 

test_feat_iq <- test_feat_ori_iq %>% 
  mutate( across( -c(city,year, total_cases), lag ),
          year = lubridate::year( week_start_date ))

test_feat_sj[1,] <- data_sj[nrow(data_sj),]
test_feat_iq[1,] <- data_sj[nrow(data_iq),]






# Detalles ----------------------------------------------------------------

# Script necesario para tener la información relativa al punto 6.2, respecto al
# modelo que combina los dos preprocesmaientos y los dos parámetros de tuning
# de xgboost. Concretamente, a la predicción enviada a la DRIVENDATA. 


source('1_get_data.R',encoding = 'UTF-8')


library(tidymodels)


# Preprocesamiento --------------------------------------------------------

## preparar datos de san juan

vars_to_lag <- data %>% select(-c(city,year,week_start_date,weekofyear,
                                  total_cases)) %>% names()

recipe_dif16 <- function(df){
  
  recipe( total_cases ~ ., data = df ) %>% 
    
    step_mutate( 'mes' = lubridate::month(week_start_date, label = TRUE) ) %>% 
    
    step_rm( c(city, week_start_date) ) %>% 
    
    step_nzv(  all_numeric_predictors() ) %>% 
    
    step_impute_bag( all_numeric_predictors() ) %>% 
    
    timetk::step_diff( all_of(vars_to_lag), lag = 16 ) %>% 
    
    step_normalize(all_numeric_predictors()) %>% 
    
    step_dummy( mes, one_hot = TRUE )
}

recipe_dif16_prep <- prep( recipe_dif16( data_sj ) )

train_bake_sj <- bake( recipe_dif16_prep,new_data = NULL )
test_bake_sj <-  bake( recipe_dif16_prep,new_data = test_feat_sj ) %>% 
  select( -total_cases )

## preparar datos de Iquitos

recipe_binning_mes <- function(df){
  
  recipe( total_cases ~ ., data = df ) %>% 
    
    step_mutate( 'mes' = lubridate::month(week_start_date, label = TRUE) ) %>% 
    
    step_rm( c(city, week_start_date) ) %>% 
    
    step_nzv(  all_numeric_predictors() ) %>% 
    
    step_impute_bag( all_numeric_predictors() ) %>% 
    
    step_discretize( all_numeric_predictors(),num_breaks = 4,min_unique = 2 ) %>% 
    
    step_dummy( all_nominal_predictors(), one_hot = TRUE ) %>% 
    
    step_normalize(all_numeric_predictors())
}

recipe_binning_mes_prep <- prep( recipe_binning_mes( data_iq ) )

train_bake_iq <- bake( recipe_binning_mes_prep,new_data = NULL )
test_bake_iq <-  bake( recipe_binning_mes_prep,new_data = test_feat_iq ) %>% 
  select( -total_cases )


# Modelos -----------------------------------------------------------------

source('best_xgboost.R',encoding = 'UTF-8')

fit_dif16_xgboost_sj <- mod_xgboost_best_sj %>% 
  fit( total_cases ~ ., data = train_bake_sj )

fit_binning_xgboost_iq <- mod_xgboost_best_iq %>% 
  fit( total_cases ~ ., data = train_bake_iq )




# Prediccion --------------------------------------------------------------


pred_sj <- predict( fit_dif16_xgboost_sj, new_data = test_bake_sj)$.pred
pred_iq <- predict( fit_binning_xgboost_iq, new_data = test_bake_iq)$.pred

bind_rows( test_feat_ori_sj[,1:3], test_feat_ori_iq[,1:3] ) %>% 
  
  mutate( 'total_cases' = c(pred_sj, pred_iq ),
    'total_cases' = round(total_cases),
    'total_cases' = ifelse(total_cases < 0, 0, total_cases)
  ) %>% 
  readr::write_csv( 'Predicciones/6_fe_xgboost.csv' )





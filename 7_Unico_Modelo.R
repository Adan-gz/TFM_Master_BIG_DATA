
# Detalles ----------------------------------------------------------------

# Código relativo al punto 7 sobre generar un único modelo para las dos ciudades
#• introduceindo la feature ciudad como una variable más.


source('1_get_data.R',encoding = 'UTF-8')
library(tidymodels)


split_data <- initial_time_split(data, prop = .8)


# recipe_base -------------------------------------------------------------

recipe_base <- function(df){
  
 recipe( total_cases ~ ., data = df ) %>% 
  
  step_rm( c(week_start_date) ) %>% 
  
  step_nzv() %>% 
  
  step_impute_bag( all_predictors() ) %>% 
  
  step_normalize( all_numeric_predictors() ) %>% 
  
  step_dummy( city, one_hot = TRUE )

}
mod_xgboost <- boost_tree(mode = 'regression',engine = 'xgboost')

fit_xgboost <- last_fit( object = mod_xgboost, 
                         preprocessor = recipe_base(training(split_data)),
                    split = split_data, metrics = metric_set(mae) )

collect_metrics(fit_xgboost)


# preidicción pra competición

recipe_base_prep <- prep( recipe_base(data) )

fit_full <- mod_xgboost %>% fit( total_cases ~., data = bake(recipe_base_prep,new_data= NULL) )

test_feat <- bind_rows(test_feat_sj,test_feat_iq)
test_feat_bake <- bake(recipe_base_prep,new_data= test_feat) 

test_features[,1:3] %>% 
  mutate( 'total_cases' = predict(fit_full, test_feat_bake)$.pred,
          'total_cases' = round(total_cases),
          'total_cases' = ifelse(total_cases <0,0,total_cases),
          ) %>% 
  readr::write_csv( 'Predicciones/8_1modelo_xgboost.csv' )


# recipe dif16 ------------------------------------------------------------


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


mod_xgboost_16 <- boost_tree(mode = 'regression',engine = 'xgboost')

fit_xgboost_16 <- last_fit( object = mod_xgboost_16, 
                            preprocessor = recipe_dif16(training(split_data)),
                            split = split_data, metrics = metric_set(mae) )

collect_metrics(fit_xgboost_16)

# predicción para competición
recipe_dif16_prep <- prep( recipe_dif16(data) )

fit_full <- mod_xgboost %>% fit( total_cases ~., data = bake(recipe_dif16_prep,new_data= NULL) )

test_feat <- bind_rows(test_feat_sj,test_feat_iq)
test_feat_bake <- bake(recipe_dif16_prep,new_data= test_feat) 

test_features[,1:3] %>% 
  mutate( 'total_cases' = predict(fit_full, test_feat_bake)$.pred,
          'total_cases' = round(total_cases),
          'total_cases' = ifelse(total_cases <0,0,total_cases),
  ) %>% 
  readr::write_csv( 'Predicciones/8_1modelo_xgboost_dif16.csv' )







# Detalles ----------------------------------------------------------------

# Código relativo al Apéndice sobre extraer la importancia de las variables
# en el mejor modelo


source('1_get_data.R',encoding = 'UTF-8')
source('best_xgboost.R')

library(tidymodels)
library(vip)


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

wflow_sj <- workflow( recipe_dif16(data_sj), mod_xgboost_best_sj )
wflow_iq <- workflow( recipe_binning_mes(data_iq), mod_xgboost_best_iq )

fit_sj <- parsnip::fit( wflow_sj, data = data_sj )
fit_iq <- parsnip::fit( wflow_iq, data = data_iq )
 

model_sj <- extract_fit_parsnip( fit_sj )
model_iq <- extract_fit_parsnip( fit_iq )

vi_sj <- vip::vi( model_sj  )
vi_iq <- vip::vi( model_iq  )

p_importance_sj <- vip::vip(model_sj ,num_features = 25, 
         aesthetics = list('fill' = "#f46572",'width'=.8))+
  geom_text( aes( y = Importance +0.01, label = round(Importance,3)) )+
  geom_hline(yintercept =  mean(vi_sj$Importance), linetype = 2 )+
  labs(title = 'San Juan', y = 'Importancia')+
  theme_light()+
  theme( panel.grid.minor.x = element_blank(),
         panel.grid.minor.y = element_blank(),
         panel.grid.major.y = element_blank(),
         plot.title = element_text(face = 'bold')
         )

p_importance_iq <- vip::vip(model_iq ,num_features = 25, 
         aesthetics = list('fill' = "#f46572",'width'=.8))+
  geom_text( aes( y = Importance +0.005, label = round(Importance,3)) )+
  geom_hline(yintercept =  mean(vi_iq$Importance), linetype = 2 )+
  labs(title = 'Iquitos', y = 'Importancia')+
  theme_light()+
  theme( panel.grid.minor.x = element_blank(),
         panel.grid.minor.y = element_blank(),
         panel.grid.major.y = element_blank(),
         plot.title = element_text(face = 'bold')
  )


ggsave(plot = p_importance_sj,
       filename = 'plots/Apendice/p_importance_sj.png',
       type = 'cairo',
       width  = 4000,
       height = 2250,
       dpi    = 320,
       units = 'px'
)


ggsave(plot = p_importance_iq,
       filename = 'plots/Apendice/p_importance_iq.png',
       type = 'cairo',
       width  = 4000,
       height = 2250,
       dpi    = 320,
       units = 'px'
)









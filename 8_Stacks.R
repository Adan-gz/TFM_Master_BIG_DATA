

# Detalles ----------------------------------------------------------------

# Código relativo al punto 7 sobre generar un stacks de modelos



source('1_get_data.R',encoding = 'UTF-8')
library(tidymodels);library(stacks)

split_data <- initial_time_split(data, prop = .8)

set.seed(123)
split_rolling <- rolling_origin( data, 1150, 250, skip = 4*5 )


# preprocesamiento --------------------------------------------------------

recipe_base <- function(df){
  
  recipe( total_cases ~ ., data = df ) %>% 
    
    step_mutate( 'mes' = lubridate::month(week_start_date, label = TRUE) ) %>% 
    
    step_rm( c(week_start_date) ) %>% 
    
    step_nzv() %>% 
    
    step_impute_bag( all_predictors() ) %>% 
    
    step_normalize( all_numeric_predictors() ) %>% 
    
    step_dummy( c(city,mes), one_hot = TRUE )
  
}


vars_to_lag <- data %>% select(-c(city,year,week_start_date,weekofyear,
                                  total_cases)) %>% names()

recipe_dif16 <- function(df){
  
  recipe( total_cases ~ ., data = df ) %>% 
    
    step_mutate( 'mes' = lubridate::month(week_start_date, label = TRUE) ) %>% 
    
    step_rm( c(week_start_date) ) %>% 
    
    step_nzv(  all_numeric_predictors() ) %>% 
    
    step_impute_bag( all_numeric_predictors() ) %>% 
    
    timetk::step_diff( all_of(vars_to_lag), lag = 16 ) %>% 
    
    step_normalize(all_numeric_predictors()) %>% 
    
    step_dummy( c(city,mes), one_hot = TRUE )
}

recipe_binning_mes <- function(df){
  
  recipe( total_cases ~ ., data = df ) %>% 
    
    step_mutate( 'mes' = lubridate::month(week_start_date, label = TRUE) ) %>% 
    
    step_rm( c( week_start_date) ) %>% 
    
    step_nzv(  all_numeric_predictors() ) %>% 
    
    step_impute_bag( all_numeric_predictors() ) %>% 
    
    step_discretize( all_numeric_predictors(),num_breaks = 4,min_unique = 2 ) %>% 
    
    step_dummy( all_nominal_predictors(), one_hot = TRUE ) 
    
}


# parsnip -----------------------------------------------------------------


mod_xgboost_tune <- boost_tree(mode = 'regression',engine = 'xgboost') %>% 
  set_args(tree_depth = tune(),trees = tune(),learn_rate = tune(),
           mtry = tune() , min_n = tune(), loss_reduction = tune())

set.seed(123)
grid_xgboost <- dials::grid_max_entropy( 
  tree_depth(),trees(),learn_rate(),mtry(range = c(2,10) ) , min_n(),
  loss_reduction(), size = 30 )


ctrl_grid <- control_stack_grid()
# ctrl_res <- control_stack_resamples()

# xgboost ----------------------------------------------------------------

wflow_xgboost <- workflow() %>% 
  add_model(mod_xgboost_tune) %>%
  add_recipe( recipe_base( training(split_data) ) )

set.seed(2020)
res_xgboost <- 
  tune_grid(
    wflow_xgboost,
    resamples = split_rolling,
    metrics = metric_set(mae),
    grid = grid_xgboost,
    control = ctrl_grid
  )


# xgboost + dif16 -------------------------------------------------------

wflow_xgboost_dif16 <- workflow() %>% 
  add_model(mod_xgboost_tune) %>%
  add_recipe( recipe_dif16( training(split_data) ) )

set.seed(2020)
res_xgboost_dif16 <- 
  tune_grid(
    wflow_xgboost_dif16,
    resamples = split_rolling,
    metrics = metric_set(mae),
    grid = grid_xgboost,
    control = ctrl_grid
  )

# xgboost + binning -------------------------------------------------------

wflow_xgboost_binning <- workflow() %>% 
  add_model(mod_xgboost_tune) %>%
  add_recipe( recipe_binning_mes( training(split_data) ) )

set.seed(2020)
res_xgboost_binning <- 
  tune_grid(
    wflow_xgboost_binning,
    resamples = split_rolling,
    metrics = metric_set(mae),
    grid = grid_xgboost,
    control = ctrl_grid
  )

collect_metrics(res_xgboost_binning, summarize = T) %>% 
  arrange(mean)

select_best(res_xgboost_binning)

# STACKS ------------------------------------------------------------------


xgboost_stacks_data <- stacks() %>%
  add_candidates(res_xgboost, name = 'XGBOOST') %>%
  add_candidates(res_xgboost_dif16, name = 'XGBOOST DIF16') %>%
  add_candidates(res_xgboost_binning, name = 'XGBOOST BINNING')

xgboost_stacks_model <- xgboost_stacks_data %>%
  blend_predictions()

xgboost_stacks_model_st <-  xgboost_stacks_model %>%
  fit_members()

autoplot(xgboost_stacks_model_st, type = "weights")

xgboost_stacks_model_st

library(ggplot2);library(dplyr)
p_stacks_weight <- tribble(
  ~ 'member',           ~ 'weight',
  'XGBOOST.BINNING_1_24', 1.18e+8,
  'XGBOOST_1_24'        , 1.16e+7,
  'XGBOOST_1_09'        , 4.30e+6,
  'XGBOOST.BINNING_1_02', 2.78e+2,
  'XGBOOST.DIF16_1_29'  , 1.34e-1,
  'XGBOOST_1_04'        , 3.92e-2 ) %>%

  ggplot( aes( weight,forcats::fct_reorder(member,weight), fill = member ) )+

  geom_col(show.legend = F, width = .8)+

  scale_fill_manual( values = ksnet::color_pallette_alpha( "#f46572", n = 6,rev = TRUE) )+

  labs(y=NULL)+

  theme_light()+

  theme( axis.title.x = element_text( size = 14 ),
         axis.title.y = element_text( size = 14 ),
         axis.text.x = element_text( size = 13 ),
         axis.text.y = element_text( size = 13 ),
         plot.caption = element_text(hjust = 0)
  )

ggsave(plot = p_stacks_weight,
       filename = 'plots/9_stacks/p_stacks_weight.png',
       type = 'cairo',
       width  = 4000,
       height = 2250,
       dpi    = 320,
       units = 'px'
)







autoplot(xgboost_stacks_model_st)



yardstick::mae_vec(
  truth = testing(split_data)$total_cases,
  estimate = predict( xgboost_stacks_model_st, new_data = testing(split_data) )$.pred )


# Competicion -------------------------------------------------------------

test_feat <- bind_rows(test_feat_sj,test_feat_iq)

test_features[,1:3] %>%
  mutate( 'total_cases' = predict(xgboost_stacks_model_st, test_feat)$.pred,
          'total_cases' = round(total_cases),
          'total_cases' = ifelse(total_cases <0,0,total_cases),
  ) %>%
  readr::write_csv( 'Predicciones/8_stacks.csv' )







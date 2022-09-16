

# Modelos resultantes del script 5_Primeros_Modelos.RMD

best_xgboost <- readRDS('Perfomance/5_best_xgboost.RDS')

mod_xgboost_best_sj <- parsnip::boost_tree(mode = 'regression',engine = 'xgboost') %>% 
  parsnip::set_args(tree_depth     = best_xgboost$tree_depth[1],
           trees          = best_xgboost$trees[1],
           learn_rate     = best_xgboost$learn_rate[1],
           mtry           = best_xgboost$mtry[1],
           min_n          = best_xgboost$min_n[1], 
           loss_reduction = best_xgboost$loss_reduction[1])

mod_xgboost_best_iq <- parsnip::boost_tree(mode = 'regression',engine = 'xgboost') %>% 
  parsnip::set_args(tree_depth     = best_xgboost$tree_depth[2],
           trees          = best_xgboost$trees[2],
           learn_rate     = best_xgboost$learn_rate[2],
           mtry           = best_xgboost$mtry[2],
           min_n          = best_xgboost$min_n[2], 
           loss_reduction = best_xgboost$loss_reduction[2])
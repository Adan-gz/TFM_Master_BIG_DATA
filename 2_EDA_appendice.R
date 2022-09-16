

# Detalles ----------------------------------------------------------------

# Breve análisis (fundamentalmente visual) llevado a cabo que acabo ubicándose
# en el Apéndice.

# Data --------------------------------------------------------------------

source('1_get_data.R',encoding = 'UTF-8')

library(ggplot2);library(flextable)

vars_order <- data %>% select( -week_start_date,-city ) %>% names()

data <- data %>% 
  
  mutate( city = factor(city, c('sj','iq'),c('San Juan','Iquitos') ) )


# Estadisticos univariados ------------------------------------------------

data_summary <- data %>% 
  
  select( -week_start_date ) %>% 
  
  tidyr::pivot_longer( -city, names_to = 'Variable' ) %>% 
  
  group_by( city, Variable ) %>% 
  
  summarise(
    'missings' = sum(is.na(value),na.rm = TRUE),
    'Min' = min(value, na.rm = T),
    'p25' = quantile(value,.25,na.rm = T),
    'Media' = mean(value, na.rm = T),
    'p50' = median(value, na.rm = T),
    'p75' = quantile(value,.75,na.rm = T),
    'Max' = max(value, na.rm = T), 
    'SD' = sd(value, na.rm = T),
    .groups = 'drop'
  ) %>% 
  
  mutate( across( where(is.numeric), round,2 ),
          city = as.character(city),
          'Variable' = factor(Variable, vars_order)) %>% 
  
  arrange(city, Variable)
 
data_summary$city[2:23] <- ''
data_summary$city[25:46] <- ''

data_summary %>% 
  flextable() %>% 
  theme_vanilla() %>% 
  fontsize( j = 1:ncol(data_summary), size = 9 ) %>% 
  save_as_docx( path = 'tablas/Apendice/estadisticos_univ.docx' )


# plots univariados -------------------------------------------------------


list_plots_univ_san_juan <- DataExplorer::plot_histogram( 
  data %>% filter(city == 'San Juan'), 
  ggtheme = theme_light(),title = 'San Juan',
  theme_config = theme(axis.title.x = element_blank(),
                       plot.title = element_text(face='bold')),
  geom_histogram_args = list('fill'= '#f46572',color = 'white' ) )


list_plots_univ_iquitos <- DataExplorer::plot_histogram( 
  data %>% filter(city == 'Iquitos'), 
  ggtheme = theme_light(),title = 'Iquitos',
  theme_config = theme(axis.title.x = element_blank(),
                       plot.title = element_text(face='bold')),
  geom_histogram_args = list('fill'= '#f46572',color = 'white' ) )



# plots bivariados --------------------------------------------------------

scatter_plot <- function(vi){
  data %>% 
    select(total_cases, x = all_of(vi), city ) %>% 
    
    ggplot( aes( x, total_cases, color = city, # fill = city
    ) )+
    
    geom_point( alpha = .2, shape = 21 )+
    
    geom_smooth( data =. %>% filter(city == 'San Juan'),
                 method = 'loess', se = F, color = c("#056F6A"))+
    
    geom_smooth( data =. %>% filter(city == 'Iquitos'),
                 method = 'loess', se = F, color = c('#934641'))+
    
    scale_color_manual( values =  c('Iquitos'='#f46572', 'San Juan' = "#00b2a9") )+
    
    scale_x_continuous(breaks = scales::breaks_pretty(7))+
    scale_y_continuous(breaks = scales::breaks_pretty(7))+
    labs(y=NULL,color=NULL,fill=NULL, x = vi)+
    theme_light()+
    theme(panel.grid.minor.x = element_blank(),
          panel.grid.minor.y = element_blank(),
          panel.grid.major.x = element_blank(),
          panel.grid.major.y = element_blank(),
          legend.position = 'none'
    )
}

list_plots_biv <- purrr::map( vars_order[-c(1,2,23)], scatter_plot )

plots_biv <- patchwork::wrap_plots(list_plots_biv)
plots_biv

ggsave(plot = plots_biv,
       filename = 'plots/Apendice/p_biv.png',
       type = 'cairo',
       width  = 4000,
       height = 2250,
       dpi    = 320,
       units = 'px'
)

# year & weekofyear -------------------------------------------------------


plot_year <- data %>% 
  ggplot( aes( factor(year), total_cases ) )+
  geom_boxplot()+
  labs(x=NULL)+
  theme_light()+
  facet_wrap(~city, scales='free')

ggsave(plot = plot_year,
       filename = 'plots/Apendice/p_biv_year.png',
       type = 'cairo',
       width  = 4000,
       height = 2250,
       dpi    = 320,
       units = 'px'
)

weekofyear_label <- as.character(seq(1,55,1))
weekofyear_label[ seq(2,54,2)] <- ''

plot_weekofyear <- data %>% 
  ggplot( aes( factor(weekofyear), total_cases ) )+
  geom_boxplot()+
  scale_x_discrete( labels = weekofyear_label )+
  labs(x=NULL)+
  theme_light()+
  facet_wrap(~city, scales='free_y')


ggsave(plot = plot_weekofyear,
       filename = 'plots/Apendice/p_biv_weekofyear.png',
       type = 'cairo',
       width  = 4000,
       height = 2250,
       dpi    = 320,
       units = 'px'
)

# biv temporal ------------------------------------------------------------

range_sj <- range(data$total_cases[ data$city == 'San Juan' ])
range_iq <- range(data$total_cases[ data$city == 'Iquitos' ])

biv_temporal <- function( vi, city_label, range_city, year_break ){
  data %>% 
    filter( city == city_label ) %>% 
    
    select(total_cases, x = all_of(vi), week_start_date ) %>% 
    
    mutate( x = scales::rescale(x,range_city ) ) %>% 
    
    ggplot( aes( week_start_date ) )+
    
    geom_line( aes(y = total_cases), color='#00b2a9', alpha = .5 )+
    geom_line( aes(y = x), color='#f46572', alpha = .5 )+
    
    scale_x_date(breaks = scales::breaks_width( year_break),
                 labels = scales::label_date_short())+
    
    scale_y_continuous(breaks = scales::breaks_pretty(7))+
    
    labs(y=NULL,color=NULL,fill=NULL, x = NULL, subtitle = vi)+
    theme_light()+
    theme(panel.grid.minor.x = element_blank(),
          panel.grid.minor.y = element_blank(),
          panel.grid.major.x = element_blank(),
          panel.grid.major.y = element_blank(),
          legend.position = 'none'
    )
  
}

list_plots_biv_temp_sj <- purrr::map( 
  vars_order[-c(1,2,23)],
  ~ biv_temporal(.x, city_label = 'San Juan', range_city = range_sj, '5 years') )

plots_biv_temp_sj <- patchwork::wrap_plots(list_plots_biv_temp_sj)

ggsave(plot = plots_biv_temp_sj,
       filename = 'plots/Apendice/p_biv_temp_sj.png',
       type = 'cairo',
       width  = 4000,
       height = 2250,
       dpi    = 320,
       units = 'px'
)


list_plots_biv_temp_iq <- purrr::map( 
  vars_order[-c(1,2,23)],
  ~ biv_temporal(.x, city_label = 'Iquitos', range_city = range_iq, '3 years') )

plots_biv_temp_iq <- patchwork::wrap_plots(list_plots_biv_temp_iq)

ggsave(plot = plots_biv_temp_iq,
       filename = 'plots/Apendice/p_biv_temp_iq.png',
       type = 'cairo',
       width  = 4000,
       height = 2250,
       dpi    = 320,
       units = 'px'
)


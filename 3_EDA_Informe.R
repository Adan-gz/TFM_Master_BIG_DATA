

# Detalles ----------------------------------------------------------------

# Script para llevar a cabo las tablas y gráficas presentadas en el apartado
# de análisis de datos exploratorio del informe.

# Data --------------------------------------------------------------------


library(dplyr);library(ggplot2);library(flextable)
library(lubridate);library(patchwork)


source('1_get_data.R',encoding = 'UTF-8')

data <- data %>% 
  mutate( city_label = factor(city, c('sj','iq'),c('San Juan','Iquitos') ) )

data_sj <- data_sj %>% 
  mutate( city_label = factor(city, c('sj','iq'),c('San Juan','Iquitos') ) )

data_iq <- data_iq %>% 
  mutate( city_label = factor(city, c('sj','iq'),c('San Juan','Iquitos') ) )

pallette_alpha_color <- function(color, n, min_alpha = .1){
  
  col_rgb <-  as.numeric(col2rgb(color))
  seq_alpha <- seq( from = min_alpha, to = 1, length.out = n )
  
 purrr::map_chr(
   seq_alpha,
    ~ rgb( col_rgb[1]/255,col_rgb[2]/255,col_rgb[3]/255, alpha = .x )
  )
 
}


# Plot_time_series_base ---------------------------------------------------

p_time_series_base <- timetk::plot_time_series(.data = data, 
                         .date_var = week_start_date,
                         .value = total_cases, 
                         .facet_vars = city_label,
                         .color_var = city_label, 
                         .smooth_color = 'gray30',
                         .title = NULL,
                         .legend_show = F, 
                         .interactive = FALSE)

ggsave(plot = p_time_series_base,
       filename = 'plots/2_EDA/p_time_series_base.png',
       type = 'cairo',
       width  = 4000,
       height = 2250,
       dpi    = 320,
       units = 'px'
)

# tabla series temporales -------------------------------------------------

data %>% 
  
  group_by( city_label ) %>% 
  
  summarise(
    N = n(),
    'Min' = min(total_cases),
    'p25' = quantile(total_cases,.25),
    'Media' = mean(total_cases),
    'p50' = median(total_cases),
    'p75' = quantile(total_cases,.75),
    'Max' = max(total_cases), 
    'SD' = sd(total_cases)
  ) %>% 
  
  flextable() %>% 
  theme_vanilla() %>% 
  fontsize( j = 1:9, size = 9 ) %>% 
  save_as_docx( path = 'tablas/2_EDA/resumen_series.docx' )


# tendencias mensuales ----------------------------------------------------

data_mes <- data %>% 

  mutate( mes = month(week_start_date, label = TRUE),
          mes = factor(mes, levels(mes),  stringr::str_to_title(levels(mes)) ),
          year = as.factor(year) ) %>% 
  
  group_by( city_label, year, mes) %>% 
  
  summarise( total_cases = sum(total_cases),
             .groups = 'drop' )  


meses <- levels(data_mes$year)
length(meses)


col_meses <- c( rev( pallette_alpha_color('#b1a734', 10,min_alpha = .1)),
   'gray70',
   pallette_alpha_color('#872222', 10,min_alpha = .1) )

names(col_meses) <- as.character(meses)

plot_mes_sj <- data_mes %>% 
  
  filter( city_label == 'San Juan' ) %>% 
  
  ggplot( aes( mes, total_cases, color = year, group = year ) )+
  
  geom_line( size = 1.2 )+
  
  scale_color_manual( values = col_meses[ as.character(unique(data_sj$year)) ] )+
  
  labs( x='' )+
  
  theme_bw()+
  
  theme( legend.position = 'bottom' )+
  
  facet_wrap( ~ city_label, nrow = 2)

plot_mes_iq <- data_mes %>% 
  
  filter( city_label == 'Iquitos' ) %>% 
  
  ggplot( aes( mes, total_cases, color = year, group = year ) )+
  
  geom_line( size = 1.2 )+
  
  scale_color_manual( values = col_meses[ as.character(unique(data_iq$year)) ] )+
  
  labs( x='', color='' )+
  
  theme_bw()+
  
  theme( legend.position = 'none' )+
  
  facet_wrap( ~ city_label, nrow = 2)


ggsave(plot = plot_mes_sj/plot_mes_iq,
       filename = 'plots/2_EDA/p_mes_ts.png',
       type = 'cairo',
       width  = 4000,
       height = 2250,
       dpi    = 320,
       units = 'px'
)

## acf

acf_sj <- acf(data_sj$total_cases)
acf_iq <- acf(data_iq$total_cases)
par(mfrow = c(1,2))
plot(acf_sj,main='San Juan')
plot(acf_iq,main='Iquitos')

par(mfrow = c(1,1))



# Valores perdidos --------------------------------------------------------

top_perdidos_sj <- DataExplorer::profile_missing(data_sj) %>% 
  arrange(-pct_missing) %>% 
  filter( pct_missing > 0.01 )

top_perdidos_sj_word <- top_perdidos_sj %>% 
  mutate( pct_missing = scales::percent( round(pct_missing,3) ) ) %>% 

  setNames( c('Variable','N perdidos','%') ) %>% 
  flextable() %>% 
  theme_vanilla() %>% 
  add_header_lines(values = 'Ciudad: San Juan') %>% 
  fontsize( j = 1:3, size = 8 ) %>% 
  save_as_docx( path = 'tablas/2_EDA/valores_perdidos_sj.docx' )

top_perdidos_iq <- DataExplorer::profile_missing(data_iq) %>% 
  arrange(-pct_missing) %>% 
  filter( pct_missing > 0.01 )

top_perdidos_iq_word <- top_perdidos_iq %>% 
  mutate( pct_missing = scales::percent( round(pct_missing,3) ) ) %>% 
  
  setNames( c('Variable','N perdidos','%') ) %>% 
  flextable() %>% 
  theme_vanilla() %>%
  add_header_lines(values = 'Ciudad: Iquito') %>% 
  fontsize( j = 1:3, size = 8 ) %>% 
  save_as_docx( path = 'tablas/2_EDA/valores_perdidos_iq.docx' )

ggmice::plot_pattern(
 data_sj %>% select( all_of(top_perdidos_sj$feature) )
) 

ggmice::plot_pattern(
  data_iq %>% select( all_of(top_perdidos_iq$feature) )
) 


# correlación entre variables ---------------------------------------------


corrplot::corrplot(
  cor( data_sj  %>% select(-c(city,city_label, year,week_start_date)), 
       use = 'complete.obs' ), type = 'lower', order = 'FPC', tl.col = 'black'
)

corrplot::corrplot(
  cor( data_iq  %>% select(-c(city,city_label, year,week_start_date)), 
       use = 'complete.obs' ), type = 'lower', order = 'FPC', tl.col = 'black'
)


# correlacion features-VD -------------------------------------------------

features <- data_iq  %>% select(-c(city,city_label, year,week_start_date, 
                                   total_cases)) %>% names()

ksnet::get_association(
  df = data_sj,
  pairs_to_check = list('total_cases' = features), return_df = TRUE
  
) %>% 
  select( var2, estimate ) %>% 
  arrange( desc( abs(estimate) ) ) %>% 
  mutate( estimate = round(estimate,2) ) %>% 
  flextable() %>% 
  theme_vanilla() %>% 
  save_as_docx( path = 'tablas/2_EDA/correlacion_feat_vd.docx' )


ksnet::get_association(
  df = data_iq,
  pairs_to_check = list('total_cases' = features), return_df = TRUE
  
) %>% 
  select( var2, estimate ) %>% 
  arrange( desc( abs(estimate) ) ) %>% 
  mutate( estimate = round(estimate,2) ) %>% 
  flextable() %>% 
  theme_vanilla() 


# relacion con reanalysis_specific_humidity_g_per_kg  ---------------------

humedadSpec_dengue_point <- data %>% 
  
  ggplot( aes( reanalysis_specific_humidity_g_per_kg, total_cases,
               color = city_label ) )+
  
  geom_point(alpha = .2)+
  
  geom_smooth( aes(fill = city_label), method = 'loess', show.legend = F)+
  

  labs(color = 'Ciudad', x = 'Humedad específica\n(reanalysis_specific_humidity_g_per_kg)',
       y = 'Nº de casos de dengue')+
  
  theme_minimal()+
  
  theme( legend.position = c(0.25,0.85),
         legend.text = element_text( size = 13 ),
         legend.title = element_text( size = 13 ),
         axis.title.x = element_text( size = 14 ),
         axis.title.y = element_text( size = 14 ),
         axis.text.x = element_text( size = 13 ),
         axis.text.y = element_text( size = 13 )
         )

ggsave(plot = humedadSpec_dengue_point,
       filename = 'plots/2_EDA/humedadSpec_dengue_point.png',
       type = 'cairo',
       width  = 4000,
       height = 2250,
       dpi    = 320,
       units = 'px'
)


humedadSpec_dengue_sjLine <-data_sj %>% 
  
  mutate( rsh = scales::rescale(
    reanalysis_specific_humidity_g_per_kg, to = c(0,400) ) ) %>% 
  
  ggplot( aes( week_start_date ) )+
  
  geom_line( aes( y = total_cases, color = 'Casos de Dengue' ) )+
  
  geom_line( aes( y = rsh, color = 'Humedad Específica' ) )+
  
  scale_x_date( breaks = scales::breaks_width('2 year'),
                labels = scales::label_date_short())+
  
  
  labs(x=NULL, colour = NULL,
       caption = 'Nota: los valores de reanalysis_specific_humidity_g_per_kg se reescalan al rango 0-400 del número de casos.')+
  
  theme_minimal()+
  
  theme( legend.position = c(0.15,0.85),
         plot.caption = element_text(hjust = 0, size = 8),
         legend.text = element_text( size = 13 ),
         legend.title = element_text( size = 13 ),
         axis.title.x = element_text( size = 14 ),
         axis.title.y = element_text( size = 14 ),
         axis.text.x = element_text( size = 11 ),
         axis.text.y = element_text( size = 13 ))

ggsave(plot = humedadSpec_dengue_sjLine,
       filename = 'plots/2_EDA/humedadSpec_dengue_sjLine.png',
       type = 'cairo',
       width  = 4000,
       height = 2250,
       dpi    = 320,
       units = 'px'
)

  
data_sj %>% 
  
  mutate( rsh = scales::rescale(
    lag( reanalysis_specific_humidity_g_per_kg ), to = c(0,400) ) ) %>% 
  
  ggplot( aes( week_start_date ) )+
  
  geom_line( aes( y = total_cases, color = 'Casos de Dengue' ) )+
  
  geom_line( aes( y = rsh, color = 'Humedad Específica' ) )+
  
  scale_x_date( breaks = scales::breaks_width('2 year'),
                labels = scales::label_date_short())+
  
  
  labs(x=NULL, colour = NULL,
       caption = 'Nota: los valores de reanalysis_specific_humidity_g_per_kg se reescalan al rango 0-400 del número de casos.')+
  
  theme_minimal()+
  
  theme( legend.position = c(0.15,0.85),
         plot.caption = element_text(hjust = 0, size = 8),
         legend.text = element_text( size = 13 ),
         legend.title = element_text( size = 13 ),
         axis.title.x = element_text( size = 14 ),
         axis.title.y = element_text( size = 14 ),
         axis.text.x = element_text( size = 11 ),
         axis.text.y = element_text( size = 13 ))



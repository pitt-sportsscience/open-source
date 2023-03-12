library(tidyverse)
library(plotly)
library(corrplot)
library(janitor)
library(irr)

df <- readr::read_csv('all_cross_sport_cmj.csv')

df_clean <- df %>%
  filter(organization != "Football", # remove football
         organization != "Olympic Sports") %>% # remove staff
  mutate(gender = case_when(organization == "Lacrosse" ~ "W",
                            TRUE ~ gender),
         organization = case_when(organization == "Men's Basketball Past" ~ "Men's Basketball",
                                  organization == "Women's Soccer Past" ~ "Women's Soccer",
                                  organization == "Swimming & Diving Past" ~ "Swimming & Diving",
                                  TRUE ~ organization),
         date = as.Date(date, format = "%m/%d/%y")) %>%
  clean_names()

test_retest <- df_clean %>%
  dplyr::group_by(date, first_name, last_name) %>%
  slice(1:2) %>%
  filter(!grepl("CMJ", segment),
         organization != "Swimming & Diving",
         organization != "Lacrosse")

jump1 <- test_retest %>%
  filter(segment == "Countermovement Jump:1")

jump2 <- test_retest %>%
  filter(segment == "Countermovement Jump:2")

df_test_retest <- inner_join(jump1, jump2, by = c("last_name", "date"),
                             suffix = c(".1", ".2"))

variables <- c("jump_height_m", "m_rsi", "rsi",
               "peak_relative_propulsive_power_w_kg", "braking_rfd_n_s", 
               "time_to_takeoff_s", "flight_time_s", "countermovement_depth_m", "system_weight_n")

df_output <- data.frame(matrix(nrow = 0, ncol = 8))
colnames(df_output) <- c("Variable", "ICC", "P-Value", "Within CV", "Between CV", "SEM" , "MDC", "N")

reliability <- function(x){
  
  df_output <- data.frame()
    
    for (i in seq_along(variables)){
      
      data = x %>%
        select(paste0(variables[i], '.1'), paste0(variables[i], '.2')) %>% 
        ungroup() %>%
        as.matrix()
    
    baseline_sd = sd(data[,1], na.rm = TRUE)
    
    baseline_mean = mean(data[,1], na.rm = TRUE)
    
    between_cv = (baseline_sd / baseline_mean) * 100
    
    within_diff = data[,3] - data[,4]
    
    within_mean = (mean(data[,3]) + mean(data[,4])) / 2
    
    within_cv = within_diff / within_mean
    
    icc_df = icc(data, model = c("twoway"),
                 type = c("agreement"),
                 unit = c("average"), r0 = 0, conf.level = 0.95)
    
    sem = baseline_sd * (sqrt(1-icc_df$value))
    
    mdc = sem*1.96*sqrt(2)
    
    output = cbind(icc_df$value, icc_df$p.value, between_cv, sem, mdc, nrow(data))
    
    
    
    colnames(output) <- c("ICC", "P-Value", "Within CV", "Between CV", "SEM" , "MDC", "N")
    return(output) 
  }}

reliability(df_test_retest)

mrsi_icc <- df_test_retest %>%
  ungroup() %>%
  select(flight_time_s.1, flight_time_s.2)

baseline_sd = sd(mrsi_icc$flight_time_s.1, na.rm = TRUE)

baseline_mean = mean(mrsi_icc$flight_time_s.1, na.rm = TRUE)

between_cv = (baseline_sd / baseline_mean) * 100

within_diff = mrsi_icc$flight_time_s.1 - mrsi_icc$flight_time_s.2


within_mean = mean((mrsi_icc$flight_time_s.1 + mrsi_icc$flight_time_s.2) /2)

within_cv = (within_diff / within_mean) * 100

icc_df = icc(mrsi_icc, model = c("twoway"),
             type = c("agreement"),
             unit = c("average"), r0 = 0, conf.level = 0.95)

sem = baseline_sd * (sqrt(1-icc_df$value))

mdc = sem*1.96*sqrt(2)

output <- cbind(icc_df$value, icc_df$p.value, within_cv, between_cv, sem, mdc)
colnames(output) <- c("ICC", "P-Value", "Within CV", "Between CV", "SEM" , "MDC")

output
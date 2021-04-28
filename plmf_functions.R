plot_res_plm <- function (plm_residuals){
  
  df_res <- as.data.frame(plm_residuals)
  
  df_res %<>%   rownames_to_column( var = "rowname")   %>%
    separate(rowname,  c("id", "year"))    %>% 
    group_by(id, year)
  
  names(df_res)  <- c("id", "year" , "residuals")
  
  df_res %<>%  transform(id = as.numeric(id)) %>% 
    transform(year = as.numeric(year)) %>% 
    transform(residuals = as.numeric(residuals)) %>% 
    group_by(id, year) %>% 
    mutate(dta_wide, cntry = case_when((id == 1) ~ "AUT",
                                       (id ==   2) ~ "BEL",
                                       (id == 3) ~ "CYP",
                                       (id == 4) ~ "FIN",
                                       (id == 5) ~ "FRA",
                                       (id == 6) ~ "DEU",
                                       (id == 7) ~ "GRC",
                                       (id == 8) ~ "IRL",
                                       (id == 9) ~ "ITA",
                                       (id == 10) ~ "ESP",
                                       (id == 11) ~ "NLD",
                                       (id == 12) ~ "PRT"))
  
  
 plt_res <-  ggplot(data = df_res, aes(year, residuals)) +
    geom_line(color = "#41cbb8", size = 1) +
    geom_hline(yintercept=0, linetype="dashed", color = "red")+
    labs(title = "Regression Residuals",
         subtitle = 'EZ Selected countries',
         y = "Percentaje of GDP", x = "") + 
    facet_wrap(~ cntry,  scales="free") 

  l_to_return <- list(plot(plt_res), df_res)
 
 
  return(l_to_return)
  
}

plot_pred_plm <- function (dependent, model_object) {
  
  PDEB <- as.data.frame(dependent)
  names(PDEB) <- "model_PDEB"
  
  df_pred <-  as.data.frame(model_object$model[ , 1] - model_object$residuals)
  
  df_pred$year <- df_res$year
  df_pred$id <- df_res$id
  df_pred$PDEB <- PDEB$model_PDEB
  
  names(df_pred)  <- c("predicted", "year" , "id", "model_PDEB")
  
  df_pred %<>%  transform(id = as.numeric(id)) %>% 
    transform(year = as.numeric(year)) %>% 
    transform(predicted = as.numeric(predicted)) %>% 
    group_by(id, year) %>% 
    mutate(dta_wide, cntry = case_when((id == 1) ~ "AUT",
                                       (id ==   2) ~ "BEL",
                                       (id == 3) ~ "CYP",
                                       (id == 4) ~ "FIN",
                                       (id == 5) ~ "FRA",
                                       (id == 6) ~ "DEU",
                                       (id == 7) ~ "GRC",
                                       (id == 8) ~ "IRL",
                                       (id == 9) ~ "ITA",
                                       (id == 10) ~ "ESP",
                                       (id == 11) ~ "NLD",
                                       (id == 12) ~ "PRT"))
  
  
  ggplot(data = df_pred, aes(x=year)) +
    geom_line(aes(y = predicted), color = "#cb4154", size = 1 ) + 
    geom_line(aes(y = model_PDEB), color="#4199cb", size = 1 ) +
    geom_hline(yintercept=60, linetype="dashed", color = "red")+
    labs(title = "Regression Predicted values (red) and Observed (Blue)",
         subtitle = 'EZ Selected countries',
         y = "Percentaje of GDP", x = "") + 
    theme(legend.position="bottom")  +
    facet_wrap(~ cntry,  scales="free") 
  
}

plot_fitted_by_hand <- function (dependent, fitted_by_hand ) {
  
  df_fitted_by_hand <- as.data.frame(fitted_by_hand) 
  
  
  df_fitted_by_hand %<>%   rownames_to_column( var = "rowname")   %>%
    separate(rowname,  c("id", "year"))    %>% 
    group_by(id, year)
  
  # PDEB <- as.data.frame(dependent)
  # names(PDEB) <- "model_PDEB"  
  # df_fitted_by_hand$PDEB <- PDEB$model_PDEB
  # 
  dta_wide %<>% arrange(id,year)
  df_fitted_by_hand$PDEB <- dta_wide$PDEB
  
  
  names(df_fitted_by_hand)  <- c("id", "year" , "fitted_by_hand", "model_PDEB")
  
  
  df_fitted_by_hand %<>%  transform(id = as.numeric(id)) %>% 
    transform(year = as.numeric(year)) %>% 
    transform(fitted_by_hand = as.numeric(fitted_by_hand)) %>% 
    group_by(id, year) %>% 
    mutate(dta_wide, cntry = case_when((id == 1) ~ "AUT",
                                       (id == 2) ~ "BEL",
                                       (id == 3) ~ "CYP",
                                       (id == 4) ~ "FIN",
                                       (id == 5) ~ "FRA",
                                       (id == 6) ~ "DEU",
                                       (id == 7) ~ "GRC",
                                       (id == 8) ~ "IRL",
                                       (id == 9) ~ "ITA",
                                       (id == 10) ~ "ESP",
                                       (id == 11) ~ "NLD",
                                       (id == 12) ~ "PRT"))
  
  ggplot(data = df_fitted_by_hand, aes(year)) +
    geom_line(aes(y = fitted_by_hand), color = "#cb4154", size = 1) +  
    geom_line(aes(y = model_PDEB), color="#4199cb", size = 1 ) +
    geom_hline(yintercept=60, linetype="dashed", color = "red")+
    labs(title = "Fitted by hand (red) and observed values(blue)",
         subtitle = 'EZ Selected countries',
         y = "Percentaje of GDP", x = "") + 
    facet_wrap(~ cntry, scales="free") 
  
}
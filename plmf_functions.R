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
         y = "Percentage of GDP", x = "") + 
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
    labs(title = "Regression predicted debt level (red) and Observed debt level(blue)",
         subtitle = 'EZ Selected countries',
         y = "Percentage of GDP", x = "") + 
    theme(legend.position="bottom")  +
    facet_wrap(~ cntry,  scales="free") 
  
}

plot_fitted_by_hand <- function (structuraldebt) {
    
  df_fitted_by_hand_old <- as.data.frame(structuraldebt) 
  # df_fitted_by_hand_new <- as.data.frame(structuraldebt_new) 
  
  dta_wide %<>% arrange(id,year)
  
  
  df_fitted_by_hand_old["id"] <- dta_wide$id                 # Add new column to data
  df_fitted_by_hand_old["year"] <- dta_wide$year                 # Add new column to data
  
  
  # df_fitted_by_hand_new["id"] <- dta_wide$id                 # Add new column to data
  # df_fitted_by_hand_new["year"] <- dta_wide$year  
  
  # PDEB <- as.data.frame(dependent)
  # names(PDEB) <- "model_PDEB"  
  # df_fitted_by_hand$PDEB <- PDEB$model_PDEB
  # 
  df_fitted_by_hand_old$PDEB <- dta_wide$PDEB
  # df_fitted_by_hand_new$PDEB <- dta_wide$PDEB
  
  # df_fitted_by_hand_old$structuraldebt_new <- df_fitted_by_hand_new$structuraldebt_new
  
  df_fitted_by_hand <- df_fitted_by_hand_old
  
  names(df_fitted_by_hand)  <- c("structural_old" , "id", "year"  , "model_PDEB")
  
  
  df_fitted_by_hand %<>%  transform(id = as.numeric(id)) %>% 
    transform(year = as.numeric(year)) %>% 
    transform(fitted_by_hand = as.numeric(structuraldebt)) %>% 
    group_by(id, year) %>% 
    mutate(., cntry = case_when((id == 1) ~ "AUT",
                                (id == 2) ~ "BEL",
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
    geom_line(aes(y = structural_old), color = "#cb4154", size = 1) +
    # geom_line(aes(y = structural_new), color = "orange", size = 1) +  
    geom_line(aes(y = model_PDEB), color="#4199cb", size = 1 ) +
    geom_hline(yintercept=60, linetype="dashed", color = "red")+
    labs(title = "Structural debt level  (red) and Observed debt level(blue)",
         subtitle = 'EZ Selected countries',
         y = "Percentage of GDP", x = "") + 
    facet_wrap(~ cntry, scales="free")

}

fitted_to_df_var <- function (fitted_by_hand){

  df_fitted_by_hand <- as.data.frame(fitted_by_hand) 


  df_fitted_by_hand %<>%   rownames_to_column( var = "rowname")   %>%
  separate(rowname,  c("id", "year"))    %>% 
  group_by(id, year)

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

  df_fitted_by_hand_VAR <- merge(df_fitted_by_hand, dta_wide , by=c("id", "year"), all=T)

  return(df_fitted_by_hand_VAR)

}


construct_IRFs_CI <- function(x, scales = "free_y", lowerq = 0.16, upperq = 0.84, percentile = 'standard', selection = NULL,
                                   cumulative = NULL, ..., base)
  {
  
  
  # Auxiliary functions
  cutt <- function(x, selind){
    x$irf <- x$irf[selind]  ## account for the first column being "V1" the horizon counter
    return(x)
  }
  
  agg <- function(x, aggind){
    x$irf[,aggind] <- cumsum(x$irf[aggind])
    return(x)
  }
  
  # To select shocks and variables to plot
  kk <- ncol(x$true$irf)
  k <- sqrt(kk - 1)
  
  ### Calculate cumulated irfs if necessary
  if(!is.null(cumulative)){
    aggind <- list()
    temp <- (k * cumulative) + 1
    for(i in 1:length(temp)){
      aggind[[i]] <- temp[i] - (k - c(1:k))
    }
    aggind <- unlist(aggind)
    
    x$true      <- agg(x$true, aggind)
    x$bootstrap <- lapply(x$bootstrap, agg, aggind)
  }
  
  # Select specific shocks or series
  if(!is.null(selection)){
    selind <- list()
    temp <- (k * selection[[1]]) + 1 ## for the column V1
    for(i in 1:length(temp)){
      selind[[i]] <- temp[i] - (k - selection[[2]])
    }
    selind <- c(1, unlist(selind))
    
    x$true      <- cutt(x$true, selind)
    x$bootstrap <- lapply(x$bootstrap, cutt, selind)
    
    kk <- ncol(x$true$irf)
  }
  
  n.ahead <- nrow(x$true$irf)
  
  bootstrap <- x$bootstrap
  nboot <- length(bootstrap)
  rest <- x$rest_mat
  
  n.probs <- length(lowerq)
  if(length(lowerq) != length(upperq)){
    stop("Vectors 'lowerq' and 'upperq' must be of same length!")
  }
  
  intervals <- array(0, c(n.ahead, kk, nboot))
  for(i in 1:nboot){
    intervals[,,i] <- as.matrix(bootstrap[[i]]$irf)
  }
  
  # find quantiles for lower and upper bounds
  lower <- array(0, dim = c(n.ahead, kk, n.probs))
  upper <- array(0, dim = c(n.ahead, kk, n.probs))
  if(percentile == 'standard' | percentile == 'hall'){
    for(i in 1:n.ahead){
      for(j in 1:kk){
        lower[i,j, ] <- quantile(intervals[i,j, ], probs = lowerq)
        upper[i,j, ] <- quantile(intervals[i,j, ], probs = upperq)
      }
    }
    
    if(percentile == 'hall'){
      for(p in 1:n.probs){
        lower[ ,-1, p] <- as.matrix(2*x$true$irf)[ ,-1] - lower[ ,-1, p]
        upper[ ,-1, p] <- as.matrix(2*x$true$irf)[ ,-1] - upper[ ,-1, p]
      }
    }
    
  }else if(percentile == 'bonferroni'){
    rest <- matrix(t(rest), nrow = 1)
    rest[is.na(rest)] <- 1
    rest <- c(1, rest)
    for(i in 1:n.ahead){
      for(j in 1:kk){
        if(rest[j] == 0){
          lower[i,j, ] <- quantile(intervals[i,j, ], probs = (lowerq/n.ahead))
          upper[i,j, ] <- quantile(intervals[i,j, ], probs = 1 + (((upperq - 1)/n.ahead)))
        }else{
          lower[i,j, ] <- quantile(intervals[i,j, ], probs = (lowerq/(n.ahead + 1)))
          upper[i,j, ] <- quantile(intervals[i,j, ], probs = 1 + (((upperq - 1)/(n.ahead + 1))))
        }
      }
    }
  }else{
    stop("Invalid choice of percentile; choose between standard, hall and bonferroni")
  }
  
  # plot IRF with confidence bands
  alp <- 0.7 * (1+log(n.probs, 10))/n.probs
  irf <- melt(x$true$irf, id = 'V1')
  cbs <- data.frame(V1 = rep(irf$V1, times=n.probs),
                    variable = rep(irf$variable, times=n.probs),
                    probs = rep(1:n.probs, each=(kk-1)*n.ahead),
                    lower = c(lower[,-1,]),
                    upper = c(upper[,-1,]))
  # ggplot() +
  #   geom_ribbon(data=cbs, aes(x=V1, ymin=lower, ymax=upper, group=probs), alpha=alp, fill='darkgrey') +
  #   geom_line(data=irf, aes(x=V1, y=value)) +
  #   geom_hline(yintercept = 0, color = 'red') +
  #   facet_wrap(~variable, scales = scales, labeller = label_parsed) +
  #   xlab("Horizon") + ylab("Response") +
  #   theme_bw()
  # 
  return(list(cbs,irf))
  
}

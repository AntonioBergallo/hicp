
#' Calculate Contribution UK Data
#'
#' This function calculates the contribution of COICOP codes to a target in UK data.
#'
#' @param df A data frame containing the data to be analyzed.
#' @param target The target COICOP code for which the contribution is calculated.
#' @param value The value column in the data frame.
#' @param coicops A vector of COICOP codes to be included in the calculation.
#' @return A data frame with the calculated contributions.
#' @examples
#' df <- data.frame(date = as.Date('2020-01-01') + 0:365, value = rnorm(366, 100, 10), coicop = rep(letters[1:10], each = 37))
#' contrib_uk(df, "target", value = "value", coicops = letters[1:5])
contrib_uk <- function(df, target, value = value, coicops, type = "yoy") {
  
  data <- df %>% 
    select(date, value, w, coicop, name) %>% 
    filter(coicop %in% coicops) %>% 
    bind_rows(db_cpi_agg_w %>% filter(coicop == target) %>% select(date, value,w, coicop)) 
  
  if(type == "yoy") {
    
    unchained <- data %>% 
      mutate(unchained = 100*unchain(value, date, by =1),
             .by = coicop) %>% 
      mutate(unchained = case_when(month(date) == 1 ~ 100*value/lag(value),
                                   TRUE ~unchained),
             .by = coicop) %>% 
      mutate(year = year(date),
             month = month(date)) %>% 
      mutate(unchained_all = unchained[coicop == target],
             weight_all = w[coicop == target],
             .by = "date") %>% 
      filter(coicop != target)
    
    data_final <- unchained %>% 
      mutate(across(c("w", "unchained", "unchained_all", "weight_all"), function(x) x [date == last(date[year(date) == year])],
                    .names = "{.col}_dec_lag"),
             .by = c(coicop, year)) %>% 
      mutate(across(c("w", "unchained", "unchained_all", "weight_all"), function(x) x [date == first(date[year(date) == year])],
                    .names = "{.col}_jan"),
             .by = c(coicop, year)) %>% 
      mutate(across(contains("dec_lag"), function(x) lag(x)),
             .by = c(coicop,month)) %>% 
      mutate(Contribution =  case_when(month != 1 ~ lag(w,12)/lag(weight_all,12) * (unchained_dec_lag - lag(unchained,12))/lag(unchained_all,12) * 100 +
                                         w_jan/weight_all_jan * (unchained_jan - 100)/lag(unchained_all,12) *unchained_all_dec_lag +
                                         w/weight_all * (unchained-100)/lag(unchained_all,12) * unchained_all_jan/100 * unchained_all_dec_lag,
                                       TRUE ~ lag(w)/lag(weight_all) * (unchained_dec_lag - 100) +
                                         w/weight_all * (unchained-100) * unchained_all_dec_lag/100),
             .by = coicop) %>% 
      select(date, name, coicop, Contribution) %>% 
      na.omit %>% 
      full_join(db_cpi_agg_w %>% filter(coicop == target) %>% select(date, value_yoy) %>% rename_with(~c("date", target)))
    
  } else if(type == "mom") {
    
    unchained <- data %>% 
      mutate(unchained = 100*unchain(value, date, by =1),
             .by = coicop) %>% 
      mutate(unchained = case_when(month(date) == 1 ~ 100*value/lag(value),
                                   TRUE ~unchained),
             .by = coicop)
    
    data_final <- unchained %>% 
      mutate(unchained_all = unchained[coicop == target],
             weight_all = w[coicop == target],
             value_all = value[coicop == target],
             .by = "date") %>% 
      mutate(Contribution = case_when(!month(date) %in% c(1,2) ~ 100*(unchained/lag(unchained)-1)*w*lag(unchained)/(lag(unchained_all)*weight_all),
                                      T ~ 100*(unchained/100 - 1)*w/weight_all),
             mom = 100*(value_all/lag(value_all)-1),
             .by = "coicop") %>% 
      na.omit %>% 
      select(date, name, coicop, Contribution) %>% 
      na.omit %>% 
      full_join(db_cpi_agg_w %>% filter(coicop == target) %>% select(date, value_mom) %>% rename_with(~c("date", target)))
    
  }
  
  
  return(data_final)
  
}

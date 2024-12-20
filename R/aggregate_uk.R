
#' Aggregate UK Data
#'
#' This function aggregates UK data based on a specified level and COICOP codes.
#'
#' @param df A data frame containing the data to be aggregated.
#' @param level A string indicating the aggregation level.
#' @param agg_name The name of the aggregate.
#' @param coicops A vector of COICOP codes to be included in the aggregation. Defaults to all COICOP codes in the data frame.
#' @return A data frame with the aggregated data.
#' @examples
#' df <- data.frame(date = as.Date('2020-01-01') + 0:365, value = rnorm(366, 100, 10), coicop = rep(letters[1:10], each = 37), m_abb = rep(month.abb, 31))
#' aggregate_uk(df, "level", "agg_name")
aggregate_uk <- function(df, level, agg_name, coicops = df$coicop){
  
  db_cpi_item_raw_x_jan <- 
    df %>% 
    tibble %>% 
    filter(!!sym(level)== TRUE) %>%
    filter(coicop %in% coicops) %>% 
    mutate(jan_ratio=unchain(x=value, t=date, by = 1),
           .by="coicop") %>% 
    filter(m_abb != "Jan")
  
  db_cpi_item_raw_jan <- 
    df %>% 
    tibble %>% 
    filter(!!sym(level)== TRUE) %>%
    filter(coicop %in% coicops) %>% 
    mutate(jan_ratio=unchain(x=value, t=date, by = 1),
           .by="coicop") %>%
    filter(month(date) %in% c(1,12)) %>% 
    group_by(coicop) %>% 
    mutate(dec_ratio = case_when(m_abb == "Jan" ~ jan_ratio/lag(jan_ratio),
                                 TRUE ~ NA)) %>% 
    filter(m_abb == "Jan") %>% 
    ungroup %>% 
    mutate(weighted = dec_ratio*w) %>% 
    arrange(date) %>% 
    reframe(laspey = (sum(weighted)/sum(w)),
            w0 = sum(w),
            grp = agg_name,
            .by = date) 
  
  
  
  db_agg <- data.table(db_cpi_item_raw_x_jan)[,aggregate(x=jan_ratio, w0=w, grp=coicop, index=laspey),
                                              by="date"] %>% 
    tibble %>% 
    filter(grp == "00") %>% 
    mutate(grp = agg_name) %>% 
    bind_rows(db_cpi_item_raw_jan) %>% 
    arrange(date) %>% 
    mutate(laspey = laspey*100) 
  
  db_final <- chain_uk(db_agg, 2018) %>% 
    mutate(value_yoy = 100*(chained/lag(chained,12)-1),
           value_mom = 100*(chained/lag(chained,1)-1)) %>% 
    mutate(name = agg_name)
  
  return(db_final)
  
}
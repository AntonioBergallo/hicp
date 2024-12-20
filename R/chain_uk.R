#' Chain UK Data
#'
#' This function chains UK data to a base year.
#'
#' @param df A data frame containing the data to be chained.
#' @param year The base year for chaining.
#' @return A data frame with the chained data.
#' @examples
#' df <- data.frame(date = as.Date('2020-01-01') + 0:365, laspey = rnorm(366, 100, 10))
#' chain_uk(df, 2020)
chain_uk <- function(df, year) {
  
  data <- df %>% 
    select(date, laspey)
  
  base_year_chain <- data %>% 
    filter(year(date) == year) %>% 
    mutate(rechain = case_when(month(date) != 1 ~ laspey*laspey[month(date) == 1]/100,
                               TRUE ~ laspey)) %>% 
    mutate(average = mean(rechain)) %>% 
    reframe(chained = rechain/average*100,
            .by = date)
  
  data_2 <- full_join(data, base_year_chain) %>% 
    mutate(chained = case_when(year(date) == year-1 & month(date) == 12 ~ 100/lead(laspey)*lead(chained),
                               year(date) == year+1 & month(date) == 1 ~ laspey*lag(chained)/100,
                               TRUE ~ chained)) %>% 
    mutate(chained = case_when(year(date) == year-1 & month(date) != 12 ~ laspey/laspey[year(date) == year-1 & month(date) == 12]*chained[year(date) == year-1 & month(date) == 12],
                               TRUE ~ chained)) %>% 
    na.trim("left")
  
  for (i in (1:nrow(data_2))) {
    
    if(!is.na(data_2$chained[[i]])) {
      
    } else if(month(data_2$date[[i]]) == 1) {
      
      data_2$chained[[i]] <- data_2$laspey[[i]]*data_2$chained[[i-1]]/100
      
    } else {
      
      year <- year(data_2$date[[i]])
      value <- data_2 %>% 
        filter(year(date) == year & month(date) == 1) %>% 
        pull(chained)
      
      data_2$chained[[i]] <- data_2$laspey[[i]] * value/100
      
    }
    
  }
  
  return(data_2)
  
}
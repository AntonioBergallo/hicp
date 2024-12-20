
#' Calculate Monthly Contribution to a Target Group
#'
#' This function computes the monthly contribution of specified groups (COICOP categories) 
#' to a target group's value, based on unchained values and weights.
#'
#' @param df A data frame containing the input data. Must include columns: `date`, `value`, `weight`, and `coicop`.
#' @param target A string specifying the COICOP category (target group) for which the contributions are calculated.
#' @param value The column in the data frame representing the values of interest (default: `value`).
#' @param coicops A vector of COICOP categories to include in the contribution calculation.
#'
#' @return A data frame with calculated contributions and additional columns:
#' \describe{
#'   \item{\code{unchained}}{Unchained values for each COICOP category.}
#'   \item{\code{unchained_all}}{Unchained value for the target COICOP category.}
#'   \item{\code{weight_all}}{Weight for the target COICOP category.}
#'   \item{\code{Contribution}}{Calculated contribution to the target's change.}
#'   \item{\code{mom}}{Month-on-month change for the target COICOP category.}
#' }
#'
#' @examples
#' # Example usage:
#' df <- data.frame(
#'   date = seq.Date(from = as.Date("2023-01-01"), to = as.Date("2023-12-01"), by = "month"),
#'   value = rnorm(12, 100, 10),
#'   weight = runif(12, 0.1, 1),
#'   coicop = sample(c("A", "B", "C"), 12, replace = TRUE)
#' )
#' contrib_mom(df, target = "A", value = "value", coicops = c("A", "B"))
#'
#' @export
contrib_mom <- function(df, target, value = value, coicops) {
  
  data <- df %>% 
    select(date, value, weight, coicop) %>% 
    filter(coicop %in% c(coicops, target)) 
  
  unchained <- data %>% 
    mutate(unchained = 100*unchain(value, date, by=12),
           .by = coicop)
  
  data_final <- unchained %>% 
    mutate(unchained_all = unchained[coicop == target],
           weight_all = weight[coicop == target],
           value_all = value[coicop == target],
           .by = "date") %>% 
    filter(coicop != target) %>% 
    na.omit %>% 
    mutate(Contribution = case_when(!month(date) %in% 1 ~ 100*(unchained/lag(unchained)-1)*weight*lag(unchained)/(lag(unchained_all)*weight_all),
                                    T ~ 100*(unchained/100 - 1)*weight/weight_all),
           mom = 100*(value_all/lag(value_all)-1),
           .by = "coicop") %>% 
    na.omit %>% 
    select(date, coicop, Contribution) 
  
  return(data_final)
  
}

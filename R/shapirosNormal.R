#' shapirosNormal
#'
#' Compute the normality of a subset of data. The function split the data into
#' groups (defined by \code{factor_vars}) and compute the normality of each
#' group using a shapiro test
#'
#' @param df A \code{dataframe} object with a response variable and one or more
#' independent variables
#'
#' @param resp_var is the name of the response variable
#'
#' @param factor_vars a vector with the names of the variables used as factor
#' variables
#'
#' @return A \code{data frame} with several variables:
#' \itemize{
#' \item \code{statistic} is the value of the Shapiro-Wilk statistic
#' \item \code{p_value} an approximate p-value for the test. See
#' \code{\link[stats]{shapiro.test}}
#' }

shapirosNormal <- function(df, resp_var, factor_vars) {
  rv <- enquo(resp_var)
  out <- df %>%
    group_by_(.dots=factor_vars) %>%
    summarise(statistic = round(shapiro.test(!!rv)$statistic,5),
              p_value = round(shapiro.test(!!rv)$p.value,5)) %>%
    data.frame()

  return(out)
}

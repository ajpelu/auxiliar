#' homogetest
#'
#' Check homogeneity of variances of different groups using several tests
#'
#' The function computes the Levene's test for homogeneity of variance across
#' groups (\code{\link[car]{leveneTest}}) and Fligner-Killeen Test of
#' Homogeneity of Variances (\code{\link[stats]{fligner.test}} for each of
#' the variables included in \code{factores} vector.
#'
#' @param df A \code{dataframe} object with a response variable and one or more
#' independent variables
#'
#' @param resp_var is the name of the response variable
#'
#' @param factores a vector with the names of the variables used as factor
#'
#' @return A \code{data frame} with several variables:
#' \itemize{
#' \item \code{fk_stat} value of the Fligner-Killeen statistic
#' \item \code{fk_pvalue} the p-value for the test
#' \item \code{lev_stat} value of the Levene statistic
#' \item \code{lev_pvalue} the p-value for the test}
#'
#' @example
#' \dontrun{
#' factores <- c('disturb_year', 'site', 'interaction(disturb_year, site)')
#' homogetest(resp_var = 'rs', factores = factores, df = evires)
#' }

homogetest <- function(resp_var, factores, df){

  out_factores <- c()

  for (f in factores){
    hv <- c()
    myformula <- as.formula(paste0(resp_var, "~", f))
    #tests
    fk <- fligner.test(myformula, data = df)
    lv <- leveneTest(myformula, data = df)
    # out
    hv$fk_stat <- fk$statistic
    hv$fk_pvalue <- fk$p.value
    hv$lev_stat <- lv$`F value`[1]
    hv$lev_pvalue <- lv$`Pr(>F)`[1]
    hv$factor <- f
    hv <- as.data.frame(hv)
    row.names(hv) <- NULL

    out_factores <- rbind(out_factores, hv)}
  return(out_factores)

}

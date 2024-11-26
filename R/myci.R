#' myci
#'
#' @param x numeric vector
#'
#' @return 95% confidence interval for mu from a single sample x
#' @export
#'
#' @examples
#' myci(1:10)

myci <- function(x){


  t=qt(0.975,length(x)-1)
  ci=c()
  ci[1]=mean(x)-t*sd(x)/sqrt(length(x))
  ci[2]=mean(x)+t*sd(x)/sqrt(length(x))
  ci

}

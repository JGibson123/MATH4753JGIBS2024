#' myncurve function
#'
#' @param mu mean
#' @param sigma standard deviation
#' @param a probability (X <= a)
#'
#' @return chart of curve with region shaded with probability.
#' @export
#'
#' @examples
#' myncurve(1, 2, 4)
myncurve = function(mu, sigma, a){
  curve(dnorm(x,mean=mu,sd=sigma), xlim = c(mu-3*sigma, mu + 3*sigma))
  xcurve=seq(a-1000,a,length=10000)
  ycurve=dnorm(xcurve, mean=mu, sd=sigma)
  polygon(c(-100000,xcurve,a),c(0,ycurve,0),col="Red")

  pnorm(a, mean=mu, sd=sigma)
}

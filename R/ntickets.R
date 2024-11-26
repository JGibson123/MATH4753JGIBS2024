#' ntickets function
#'
#' @param N Number of seats on the flight
#' @param gamma gamma
#' @param p probability
#'
#' @return Discrete and continuous plots of Objective vs N tickets sold.
#' @export
#'
#' @examples
#' ntickets(400, 0.01, 0.95)
ntickets <- function(N=200, gamma=0.02, p=0.95){





  nd <- seq(N, floor(N+N/10), by=1)
  nc <- seq(N, floor(N+N/10), length = 10000)

  tmpd <- 1-gamma-pbinom(q=(N+0.5), size=nd, prob=p)
  tmpc <- 1-gamma-pnorm(q=(N+0.5), mean=nc*p, sd=sqrt(nc*p*(1-p)))


  indd <- which.min(abs(tmpd))
  indc <- which.min(abs(tmpc))

  lay <- layout(matrix(c(1,2)))

  plot(nd, tmpd, type="b", ylim=c(0,1), main=paste0("Objective vs. N tickets sold (Discrete)\nN=",nd[indd]), xlab="N", ylab="Objective")
  abline(h=tmpd[indd], v=nd[indd], col="red")

  plot(nc, tmpc, type="l", lwd=2, ylim=c(0,1), main=paste0("Objective vs. N tickets sold (Continuous)\nN=",nc[indc]), xlab="N", ylab="Objective")
  abline(h=tmpc[indc], v=nc[indc], col="blue")

  list(nd=nd[indd], nc=nc[indc], N=N, gamma=gamma, prob=p)
}


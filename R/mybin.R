#' mybin
#'
#' @param iter number of iterations
#' @param n size of sample
#' @param p probability
#' @param main plot title
#'
#' @return barplot and table of the proportions
#' @export
#'
#' @examples
#' mybin(iter=100, n=10, p=0.5)
mybin=function(iter=100,n=10, p=0.5, main="Binomial simulation"){
  # make a matrix to hold the samples
  #initially filled with NA's
  sam.mat=matrix(NA,nr=n,nc=iter, byrow=TRUE)
  #Make a vector to hold the number of successes in each trial
  succ=c()
  for( i in 1:iter){
    #Fill each column with a new sample
    sam.mat[,i]=sample(c(1,0),n,replace=TRUE, prob=c(p,1-p))
    #Calculate a statistic from the sample (this case it is the sum)
    succ[i]=sum(sam.mat[,i])
  }
  #Make a table of successes
  succ.tab=table(factor(succ,levels=0:n))

  #Make a barplot of the proportions
  barplot(succ.tab/(iter), col=rainbow(n+1), main=main, xlab="Number of successes")
  succ.tab/iter
}

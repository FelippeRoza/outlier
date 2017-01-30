#' Outlier searching
#'
#' Returns a boolean data frame with the same dimensions of input dataframe
#' where the TRUES points to the input outliers
#'
#' @param input data frame
#' @param superior if TRUE, returns only the superior outliers
#'
#' @return Data frame
#'
#' @examples
#' input=data.frame(a=c(10,11,rnorm(20),14,-20),b=c(5,6,rnorm(20),7,-5))
#' output <- outlier(input)
#'
#' @export
outlier <- function(input, superior = TRUE){
  boxp <- boxplot(input, plot=FALSE)

  output = NULL
  for (g in unique(boxp$group)){
    column_name = boxp$name[g]
    outliers = boxp$out[boxp$group == g]
    if(superior){
      outliers = outliers[outliers > boxp$stats[3,g]] #represents the median of the g-th column of input dataframe
      #only the superior outliers will be ouputed
    }
    output = cbind(output,input[,column_name] %in% outliers)
  }
  output = data.frame(output)
  colnames(output) = boxp$names
  return(output)
}

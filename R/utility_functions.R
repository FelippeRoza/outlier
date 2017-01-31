#' Relative Change
#'
#' Relative Change calculation, as stated in \link{https://en.wikipedia.org/wiki/Relative_change_and_difference}
#'
#' @param x a numeric or complex vector or array
#' @param xref ref that x is compared to, must have same size of x
#'
#' @return data of same size of x and xref
#'
#' @examples
#' output <- rel_change(1,0.4)
#' print(output)
#'
#' @export

rel_change <- function(x, xref){
    return (x - xref)/abs(xref)
}

#' Outlier searching
#'
#' Returns a boolean data frame with the same dimensions of input dataframe
#' where the TRUES points to the input outliers
#'
#' @param input data frame
#' @param superior if TRUE, returns only the superior outliers
#' @param out_type 'logical', 'absolute' or 'percentage'
#'
#' @return Data frame, type defined by out_type parameter
#'
#' @examples
#' input=data.frame(a=c(10,11,rnorm(20),14,-20),b=c(5,6,rnorm(20),7,-5))
#' output <- outlier(input)
#'
#' @export

outlier <- function(input, superior = TRUE, out_type = 'logical'){
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
    colnames(output) = boxp$names[unique(boxp$group)]

    if(out_type == 'absolute' || out_type == 'percentage'){
        output <- sapply(output, as.numeric)
        for (g in unique(boxp$group)){
            col_name = boxp$name[g]
            output[output[,col_name] == 1, col_name] = rel_change(input[output[,col_name] == 1, col_name],boxp$stats[3,g])
        }
        if(out_type == 'percentage'){
            output = output * 100
        }
    }

    return(output)
}

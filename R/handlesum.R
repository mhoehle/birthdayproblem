#' Make syntax for nested for loop function in R
#' @param n The number of people in the "class"
#' @return Returns a function entitled \code{compute_tList}
make_tListFunc_syntax <- function(n) {
  str <- NULL
  str <- rbind(str, "compute_tList <- function() {")
  str <- rbind(str, paste0("n <- ",n))
  str <- rbind(str, "tList <- NULL")
  for (i in n:1) {
    str <- rbind(str,paste0(paste0(rep(" ",n-i),collapse=""),"for (t",i," in 0:floor(n/",i,")) {"))
  }
  str <- rbind(str,paste0(paste0(rep(" ",n),collapse=""),paste0("t <- c(",paste0("t",1:n,collapse=","),")")))
  str <- rbind(str,paste0(paste0(rep(" ",n),collapse=""),"if (sum( (1:n)*t) == n) tList <- rbind(tList, t)"))
  for (i in n:1) {
    ##Sum too large, then stop summing
    str <- rbind(str,paste0(paste0(rep(" ",i),collapse=""),"if (sum( (n:(n-",i,"+1)*t[n:(n-",i,"+1)])) > n) break;"))

    ##Done
    str <- rbind(str, paste0(paste0(rep(" ",i-1),collapse=""),"}"))
  }
  str <- rbind(str, "return(tList)")
  str <- rbind(str, "}")
  return(str)
}

#' Make syntax for nested for loop function in Rcpp
#' @import Rcpp
#' @param n The number of people in the "class"
#' @return Returns a function entitled \code{compute_tList}
#'
make_tListFunc_syntax_rcpp <- function(n) {
  str <- NULL
  str <- rbind(str, "#include <Rcpp.h>")
  str <- rbind(str, "// [[Rcpp::export]]")
  str <- rbind(str, 'void make_tList_rcpp() {')
  str <- rbind(str, paste0('int n = ',n,";"))
  str <- rbind(str, "std::cout << std::endl;")
  for (i in n:1) {
    str <- rbind(str,paste0(paste0(rep(" ",n-i+1),collapse=""),"for (int t",i,"=0; t",i," <= floor(n/",i,"); t",i,"++) {"))
  }

  ##Correct sum
  theSum <- paste0("(",paste0(paste0(1:n,"*t",1:n, collapse="+"),")"))
  str <- rbind(str,paste0(paste0(rep(" ",n+1),collapse=""),paste0("if (",theSum," == n) {")))
  output <- paste0("t",1:n, collapse=" << \",\" << ")
  str <- rbind(str,paste0(paste0(rep(" ",n+2),collapse=""),paste0("Rcpp::Rcout << ",output," << std::endl;")))
  str <- rbind(str,paste0(paste0(rep(" ",n+1),collapse=""),paste0("}")))

  ##Close the loops
  for (i in n:1) {
    ##Sum too large - stop
    theSum <- paste0("(",paste0(paste0(n:(n-i+1),"*t",n:(n-i+1), collapse="+"),")"))
    str <- rbind(str,paste0(paste0(rep(" ",i+1),collapse=""),paste0("if (",theSum," > n) { break; }")))

    ##Close parenthesis
    str <- rbind(str, paste0(paste0(rep(" ",i),collapse=""),"}"))
  }
  str <- rbind(str, "}")
  return(str)
}

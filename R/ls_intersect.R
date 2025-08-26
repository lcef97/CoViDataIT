#' @keywords internal
#'
ls_intersect <- function(LS){
  aa <- names(LS[[1]])
  if(length(LS) > 1){
    N <- length(LS)
    for (i in c(2:N)){
      aa <- intersect(aa, names(LS[[i]]))
    }
  }
  res <- lapply(LS, function(x){
    return(x[aa])
  })
  return(res)
}

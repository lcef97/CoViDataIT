#' Download regional CoViD cases data from the Italian Superior Institute of Health
#'
#' @description  This function downloads the region-level CoViD cases for a user-specified time period
#'
#'
#' @param date_from Character. The starting date. Important: necessary to use the "yyyy-mm-dd" format.
#' @param date_to Character. The ending date. Important: necessary to use the "yyyy-mm-dd" format.
#'
#'
#' @param verbose Logical. If \code{TRUE}, the user keeps track of the main underlying operations. \code{TRUE} by default.
#' @param autoAbort Logical. Whether to automatically abort the operation and return NULL in case of missing internet connection or server response errors. \code{FALSE} by default.
#'
#'
#' @source  \href{https://github.com/pcm-dpc/COVID-19/tree/master/dati-regioni}{GitHub archive}
#'
#' @details Covid cases data are provided by the Italian Superior Institute of Health and hosted on GitHub.
#' The current package version of this function only downloads the data between two dates.
#'
#'
#' @return An object of class \code{tbl_df}, \code{tbl} and \code{data.frame}.
#'
#' @examples
#'
#' \donttest{
#'   Get_CoViData(date_from = "2022-01-01", date_to = "2022-01-08")
#'
#' }
#'
#'
#' @export





Get_CoViData <- function(date_from = "2022-01-01", date_to = "2022-01-15",
                         verbose = TRUE, autoAbort = FALSE){

  if(!Check_connection(autoAbort)) return(NULL)

  string0 <- "https://raw.githubusercontent.com/pcm-dpc/COVID-19/master/dati-regioni/dpc-covid19-ita-regioni-"
  date_from <- as.Date(date_from)
  date_to <- as.Date(date_to)

  while(!date_to > date_from){
    message("There is a problem with dates: starting date: ",
            as.character(date_from), " is later than end date: ",
            as.character(date_to), ".\n",
            "Please, select two other dates (first: starting date, second: end date) ",
            "using this specific format: `yyyy-mm-dd;yyyy-mm-dd`.\n",
            "Do not use quotes in the prompt.")
    newdates <- readline(prompt = "  > ")
    newdates <- unlist(strsplit(newdates, split = ";"))
    date_from <- as.Date(newdates[1])
    date_to <- as.Date(newdates[2])
  }

  dates <- seq(from = date_from, to = date_to, by = 1)

  starttime <- Sys.time()
  DB <- list()
  for(t in c(1:length(dates))){
    str <- paste0(string0, gsub("-", "", as.character(dates[t])), ".csv")
    dd <- tryCatch({
      read.csv(str)
    }, error = function(e){
      message("No data found for ", as.character(dates[t]), "\n")
      return(NULL)
    })
    if(!is.null(dd) && verbose){
      cat("Data downloaded for", as.character(dates[t]), "\n")
    }
    DB[[t]] <- dd
  }
  res <- do.call(rbind, DB)
  res$data <- substr(res$data, 1, 10)
  endtime <- Sys.time()

  if(verbose){
    cat(round(difftime(endtime, starttime, units = "secs"), 2),
        "seconds needed to retrieve CoViD data from", date_from, "to", date_to, "\n")
  }
  return(res)

}


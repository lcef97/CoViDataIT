#' Download regional CoViD cases data in Italy
#'
#' @description  This function downloads the region-level CoViD cases for a user-specified time period,
#' elaborated by the Italian Ministry of Health and shared by the Civil Protection.
#'
#'
#' @param date_from Character. The starting date. Important: necessary to use the "yyyy-mm-dd" format.
#'                  "2022-01-01" by default.
#' @param date_to Character. The ending date. Important: necessary to use the "yyyy-mm-dd" format.
#'                  "2022-01-31" by default.
#'
#'
#' @param verbose Logical. If \code{TRUE}, the user keeps track of the main underlying operations. \code{TRUE} by default.
#' @param autoAbort Logical. Whether to automatically abort the operation and return NULL in case of missing internet connection or server response errors. \code{FALSE} by default.
#'
#'
#' @source  \href{https://github.com/pcm-dpc/COVID-19/tree/master/dati-regioni}{GitHub archive}
#'
#' @details Covid cases data are shared by the Italian Civil Protection and hosted on GitHub.
#'          For the time being, data sharing was interrupted on January 10th 2025.
#'
#'
#'
#'
#' @return An object of class \code{data.frame} with 21 rows per day (corresponding to Italian regions) and 30 columns.
#'         Variables are:
#'         \itemize{
#'
#'         \item \code{Date}, Date.
#'         \item \code{State}, character. Same for all observations.
#'         \item \code{Region_code}, integer. The ISTAT regional code
#'         \item \code{Region_name}, character.
#'         \item \code{Latitude}, numeric. Is not read by the system as a coordinate
#'         \item \code{Longitude}, numeric. Same as latitude.
#'         \item \code{Hospitalised_with_symptoms}, integer. The total amount of patiens hospitalised but not in intensive care.
#'         \item \code{Critical_care}, integer. The total amount of patients with severe symptoms hospitalised in intensive care.
#'         \item \code{Tot_hospitalised}, integer. Total amount of hospitalised patients.
#'         \item \code{Home_isolation}, integer. Patients not hospitalised, which are required to undergo home isolation.
#'         \item \code{Tot_positives}, integer. The total of positive patients, both hospitalised and isolated at home.
#'         \item \code{Positives_variation}. integer The variation in the total amount of positives, not to be confused with the new cases.
#'         \item \code{New_cases}, integer. Daily infection cases.
#'         \item \code{Discharged}, integer. Daily healed patients.
#'         \item \code{Deaths}, integer. Total amount of _confirmed_ deaths starting from the outbreak. Notice this is only
#'                        an official count, and is likely underestimated.
#'         \item \code{Diagnostic_suspect_tested}, integer. Cases tested after diagnostic suspect.
#'         \item \code{Screening_tested}, integer. Cases tested after screening.
#'         \item \code{Tot_cases}, integer. The whole total of confirmed infections starting from the outbreak.
#'                          Notice this is only the count of confirmed infections and is obviously underestimated.
#'         \item \code{Tot_tests}, integer. Total number of tests taken since the outbreak.
#'         \item \code{Tested_cases}, integer.
#'         \item \code{Notes}, character
#'         \item \code{Intensive_care_new}, integer. Daily admissions in intensive care.
#'         \item \code{New_tests}, integer. Daily total tests
#'         \item \code{Positive_rate}, numeric. Proportion of positive tests, given by: daily infections/daily tests
#'}
#' @examples
#'
#' \donttest{
#'   dd <- Get_CoViData(date_from = "2022-01-01", date_to = "2022-01-08")
#'   head(dd)
#' }
#'
#'
#' @export





Get_CoViData <- function(date_from = "2022-01-01", date_to = "2022-01-31",
                         verbose = TRUE, autoAbort = FALSE){

  starttime <- Sys.time()

  if(!Check_connection(autoAbort)) return(NULL)

  string0 <- "https://raw.githubusercontent.com/pcm-dpc/COVID-19/master/dati-regioni/dpc-covid19-ita-regioni-"
  date_from <- as.Date(date_from)
  date_to <- as.Date(date_to)

  while(!date_to >= date_from){
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

  if(date_to >= as.Date("2025-01-10")){
    message("No data available after January 10th 2025. We apologise for the inconvenience.")
  }

  dates <- seq(from = date_from, to = date_to, by = 1)

  DB <- list()
  for(t in c(1:length(dates))){
    str <- paste0(string0, gsub("-", "", as.character(dates[t])), ".csv")
    dd <- tryCatch({
      utils::read.csv(str)
    }, error = function(e){
      message("No data found for ", as.character(dates[t]), "\n")
      return(NULL)
    })
    if(!is.null(dd) && verbose){
      cat("Data downloaded for", as.character(dates[t]), "\n")
    }
    DB[[t]] <- dd
  }
  df <- do.call(rbind, DB)
  df$data <- as.Date(substr(df$data, 1, 10))

  names(df) <- c("Date", "State", "Region_code", "Region_name", "Latitude" ,"Longitude" ,
                 "Hospitalised_with_symptoms", "Critical_care", "Tot_hospitalised",
                 "Home_isolation", "Tot_positives",  "Positives_variation",
                 "New_cases", "Discharged", "Deaths", "Diagnostic_suspect_tested",
                 "Screening_tested", "Tot_cases", "Tot_tests", "Tested_cases",
                 "Notes", "Intensive_care_new", "Notes_about_tests", "Notes_about_cases",
                 "Tot_cases_molecular_tests", "Tot_cases_quick_test", "Molecular_tests",
                 "Quick_tests", "NUTS1_code", "NUTS2_code" )
  if(length(dates)>1){

    reg_names <- unique(df$Region_name)
    New_tests_M <- matrix(NA, nrow = length(dates), ncol = 21)

    for(t in c(2:length(dates))){
      for(s in c(1:length(reg_names))){
        New_tests_M[t, s] <- df$Tot_tests[which(df$Date == dates[t] & df$Region_name == reg_names[s]) ] -
          df$Tot_tests[which(df$Date == dates[t-1] & df$Region_name == reg_names[s]) ]
      }
    }
    New_tests <- c(t(New_tests_M))

    #New_tests_V <- rep(NA, length(dates) * 21)

    #for(t in c(2:length(dates))){
     # for(s in c(1:length(reg_names))){
      #  New_tests_V[(21*(t-1))+s] <- df$Tot_tests[which(df$Date == dates[t] & df$Region_name == reg_names[s]) ] -
       #   df$Tot_tests[which(df$Date == dates[t-1] & df$Region_name == reg_names[s]) ]
      #}
    #}

    df$New_tests <- New_tests
  } else df$New_tests <- NA
  df$positive_rate <- df$New_cases/df$New_tests


  endtime <- Sys.time()


  if(verbose){
    cat(round(difftime(endtime, starttime, units = "secs"), 2),
        "seconds needed to retrieve CoViD data from", date_from, "to", date_to, "\n")
  }
  return(df)

}


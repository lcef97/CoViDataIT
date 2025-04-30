#' @keywords internal
#'
tabrename <- function(){
   names_ITA <- c("data", "stato", "codice_regione", "denominazione_regione" ,
                  "lat", "long", "ricoverati_con_sintomi", "terapia_intensiva",
                  "totale_ospedalizzati", "isolamento_domiciliare",
                  "totale_positivi", "variazione_totale_positivi",
                  "nuovi_positivi", "dimessi_guariti",
                  "deceduti", "casi_da_sospetto_diagnostico" ,
                  "casi_da_screening", "totale_casi",
                  "tamponi", "casi_testati" , "note", "ingressi_terapia_intensiva",
                  "note_test", "note_casi" , "totale_positivi_test_molecolare",
                  "totale_positivi_test_antigenico_rapido",
                  "tamponi_test_molecolare", "tamponi_test_antigenico_rapido",
                  "codice_nuts_1", "codice_nuts_2",
                  "codice_provincia", "denominazione_provincia", "sigla_provincia", "codice_nuts_3")
   names_ENG <- c("Date", "State", "Region_code", "Region_name", "Latitude" ,"Longitude" ,
                 "Hospitalised_with_symptoms", "Critical_care", "Tot_hospitalised",
                 "Home_isolation", "Tot_positives",  "Positives_variation",
                 "New_cases", "Discharged", "Deaths", "Diagnostic_suspect_tested",
                 "Screening_tested", "Tot_cases", "Tot_tests", "Tested_cases",
                 "Notes", "Intensive_care_new", "Notes_about_tests", "Notes_about_cases",
                 "Tot_cases_molecular_tests", "Tot_cases_quick_test", "Molecular_tests",
                 "Quick_tests", "NUTS1_code", "NUTS2_code",
                 "Province_code", "Province_name", "Province_abbreviation", "NUTS3_code")
  return(data.frame(names_ITA = names_ITA, names_ENG = names_ENG))

}

#' @title Load MoNA MS/MS Tibble
#' @description
#' Load MassBank of North America.
#'
#' @param standard If TRUE, it will return a tibble with standard column names, which include accession, inchikey,
#' formula, name, exactmass, smiles, inchi, precursorMz, adduct, polarity, collision_energy, instrument_type, instrument, predicted,
#' mz, intensity.
#'
#' @return A tibble.
#' @export
#'
#' @examples
#' monaMsTb <- load_monaMsTb()
load_monaMsTb <- function(standard = TRUE){
  monaMsTb_path <- system.file("extdata", "monaMsTb.rds", package = "pubmsR")
  message("Load monaMsTb...")
  if(file.exists(monaMsTb_path)) monaMsTb <- tibble::tibble(readRDS(monaMsTb_path))
  else stop("Can not find monaMsTb, please redownload!")
  if(standard){
    monaMsTb$polarity <- unname(sapply(monaMsTb$polarity, function(x) {
      if(is.na(x)) return(NA)
      else if(x == 0) return("neg")
      else if(x == 1) return("pos")
      else if(x == -1) return("pos")
      else return(NA)
    }))
    monaMsTb$predicted <- TRUE
    monaMsTb <- monaMsTb %>%
      dplyr::select(accession, inchikey, formula, name, exactmass, precursorMz, adduct, polarity, collision_energy, instrument_type, instrument, predicted, mz, intensity)
  }
  return(monaMsTb)
}

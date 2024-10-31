#' @title Load LipidBlast MS/MS Tibble
#' @description
#' Load LipidBlast MS/MS Spectra.
#'
#' @param standard If TRUE, it will return a tibble with standard column names, which include accession, inchikey,
#' formula, name, exactmass, smiles, inchi, precursorMz, adduct, polarity, collision_energy, instrument_type, instrument, predicted,
#' mz, intensity.
#'
#' @return A tibble.
#' @export
#'
#' @examples
#' lipidblastMsTb <- load_lipidblastMsTb()
load_lipidblastMsTb <- function(standard = TRUE){
  lipidblastMsTb_path <- system.file("extdata", "lipidblastMsTb.rds", package = "pubmsR")
  message("Load lipidblastMsTb...")
  if(file.exists(lipidblastMsTb_path)) lipidblastMsTb <- tibble::tibble(readRDS(lipidblastMsTb_path))
  else stop("Can not find lipidblastMsTb, please redownload!")
  if(standard){
    lipidblastMsTb$polarity <- unname(sapply(lipidblastMsTb$polarity, function(x) {
      if(is.na(x)) return(NA)
      else if(x == 0) return("neg")
      else if(x == 1) return("pos")
      else if(x == -1) return("pos")
      else return(NA)
    }))
    lipidblastMsTb$predicted <- FALSE
    lipidblastMsTb <- lipidblastMsTb %>%
      dplyr::select(accession, inchikey, formula, name, exactmass, precursorMz, adduct, polarity, collision_energy, instrument_type, instrument, predicted, mz, intensity)
  }
  return(lipidblastMsTb)
}

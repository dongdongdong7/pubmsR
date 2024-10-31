#' @title .read_hmdb_xml
#' @description
#' Read a HMDB spectra xml file.
#'
#' @param xml_path One xml file path.
#' @param nonStop Whether to stop when an error occurs.
#'
#' @return A data.frame.
#' @author Barry Song
#' @examples
#' xml_path <- system.file("demo", "HMDB0000014_ms_ms_spectrum_2248108_experimental.xml", package = "pubmsR")
#' .read_hmdb_xml(xml_path)
.read_hmdb_xml <- function(xml_path, nonStop = TRUE){
  doc <- xml2::read_xml(xml_path)
  # id
  id <- xml2::xml_text(xml2::xml_find_first(doc, xpath = "id"))
  # notes
  notes <- xml2::xml_text(xml2::xml_find_first(doc, xpath = "notes"))
  # sample-concentration
  sample_concentration <- xml2::xml_text(xml2::xml_find_first(doc, xpath = "sample-concentration"))
  # solvent
  solvent <- xml2::xml_text(xml2::xml_find_first(doc, xpath = "solvent"))
  # sample-mass
  sample_mass <- xml2::xml_text(xml2::xml_find_first(doc, xpath = "sample-mass"))
  # sample-assessment
  sample_assessment <- xml2::xml_text(xml2::xml_find_first(doc, xpath = "sample-assessment"))
  # spectra--assessment
  spectra_assessment <- xml2::xml_text(xml2::xml_find_first(doc, xpath = "spectra-assessment"))
  # sample-source
  sample_source <- xml2::xml_text(xml2::xml_find_first(doc, xpath = "sample-source"))
  # collection-date
  collection_date <- xml2::xml_text(xml2::xml_find_first(doc, xpath = "collection-date"))
  # instrument-type
  instrument_type <- xml2::xml_text(xml2::xml_find_first(doc, xpath = "instrument-type"))
  # peak-counter
  peak_counter <- xml2::xml_text(xml2::xml_find_first(doc, xpath = "peak-counter"))
  # created-at
  created_at <- xml2::xml_text(xml2::xml_find_first(doc, xpath = "created-at"))
  # updated-at
  updated_at <- xml2::xml_text(xml2::xml_find_first(doc, xpath = "updated-at"))
  # mono-mass
  mono_mass <- xml2::xml_text(xml2::xml_find_first(doc, xpath = "mono-mass"))
  # energy-field
  energy_field <- xml2::xml_text(xml2::xml_find_first(doc, xpath = "energy-field"))
  # collision-energy-level
  collision_energy_level <- xml2::xml_text(xml2::xml_find_first(doc, xpath = "collision-energy-level"))
  # collision-energy-voltage
  collision_energy_voltage <- xml2::xml_text(xml2::xml_find_first(doc, xpath = "collision-energy-voltage"))
  # ionization-mode
  ionization_mode <- xml2::xml_text(xml2::xml_find_first(doc, xpath = "ionization-mode"))
  # sample-concentration-units
  sample_concentration_units <- xml2::xml_text(xml2::xml_find_first(doc, xpath = "sample-concentration-units"))
  # sample-mass-units
  sample_mass_units <- xml2::xml_text(xml2::xml_find_first(doc, xpath = "sample-mass-units"))
  # predicted
  predicted <- xml2::xml_text(xml2::xml_find_first(doc, xpath = "predicted"))
  # structure-id
  structure_id <- xml2::xml_text(xml2::xml_find_first(doc, xpath = "structure-id"))
  # splash-key
  splash_key <- xml2::xml_text(xml2::xml_find_first(doc, xpath = "splash-key"))
  # chromatography-type
  chromatography_type <- xml2::xml_text(xml2::xml_find_first(doc, xpath = "chromatography-type"))
  # analyzer-type
  analyzer_type <- xml2::xml_text(xml2::xml_find_first(doc, xpath = "analyzer-type"))
  # ionization-type
  ionization_type <- xml2::xml_text(xml2::xml_find_first(doc, xpath = "ionization-type"))
  # charge-type
  charge_type <- xml2::xml_text(xml2::xml_find_first(doc, xpath = "charge-type"))
  # data-source
  data_source <- xml2::xml_text(xml2::xml_find_first(doc, xpath = "data-source"))
  # data-source-id
  data_source_id <- xml2::xml_text(xml2::xml_find_first(doc, xpath = "data-source-id"))
  # adduct
  adduct <- xml2::xml_text(xml2::xml_find_first(doc, xpath = "adduct"))
  # adduct-type
  adduct_type <- xml2::xml_text(xml2::xml_find_first(doc, xpath = "adduct-type"))
  # adduct-mass
  adduct_mass <- xml2::xml_text(xml2::xml_find_first(doc, xpath = "adduct-mass"))
  # database-id
  database_id <- xml2::xml_text(xml2::xml_find_first(doc, xpath = "database-id"))

  res <- data.frame(id = id, notes = notes, sample_concentration = sample_concentration, solvent = solvent,
                    sample_mass = sample_mass, sample_assessment = sample_assessment, spectra_assessment = spectra_assessment,
                    sample_source = sample_source, collection_date = collection_date, instrument_type = instrument_type,
                    peak_counter = peak_counter, created_at = created_at, updated_at = updated_at, mono_mass = mono_mass,
                    energy_field = energy_field, collision_energy_level = collision_energy_level, collision_energy_voltage = collision_energy_voltage,
                    ionization_mode = ionization_mode, sample_concentration_units = sample_concentration_units, sample_mass_units = sample_mass_units,
                    predicted = predicted, structure_id = structure_id, splash_key = splash_key, chromatography_type = chromatography_type,
                    analyzer_type = analyzer_type, ionization_type = ionization_type, charge_type = charge_type,
                    data_source = data_source, data_source_id = data_source_id, adduct = adduct, adduct_type = adduct_type,
                    adduct_mass = adduct_mass, database_id = database_id)

  # spectra
  mz <- xml2::xml_double(xml2::xml_find_all(doc, "ms-ms-peaks/ms-ms-peak/mass-charge"))
  int <- xml2::xml_double(xml2::xml_find_all(doc, "ms-ms-peaks/ms-ms-peak/intensity"))
  if (!length(mz) || !length(int) || length(mz) != length(int)) {
    msg <- paste0("No mz and intensity values found in file ",
                  basename(xml_path))
    if (nonStop) {
      warning(msg)
      res$mz <- list(NA)
      res$int <- list(NA)
      return(res)
    }
    else stop(msg)
  }
  res$mz <- list(mz)
  res$int <- list(int)
  return(res)
}

#' @title load_hmdbMsTb
#' @description
#' Load Human Metabolome Spectra Database (Version: 5.0)
#' The data is stored in a tibble with 65029 experimental and 65535 predicted spectra (18273 unique molecules),
#' which was generated form the xml file in \url{https://hmdb.ca/downloads}.
#'
#' @param standard If TRUE, it will return a tibble with standard column names, which include accession, inchikey,
#' formula, name, exactmass, smiles, inchi, precursorMz, adduct, polarity, collision_energy, instrument_type, instrument, predicted,
#' mz, intensity.
#'
#' @return A hmdbMsTb tibble.
#' @author Barry Song
#' @export
#'
#' @examples
#' hmdbMsTb <- load_hmdbMsTb()
load_hmdbMsTb <- function(standard = TRUE){
  hmdbMsTb_path <- system.file("extdata", "hmdbMsTb.rds", package = "pubmsR")
  message("Load hmdbMsTb...")
  if(file.exists(hmdbMsTb_path)) hmdbMsTb <- tibble::tibble(readRDS(hmdbMsTb_path))
  else stop("Can not find hmdbMsTb, please redownload!")
  if(standard){
    pos_char <- c("Positive", "positive", "+")
    neg_char <- c("Negative", "negative", "-")
    polarity_vec <- unname(sapply(1:nrow(hmdbMsTb), function(i) {
      if(hmdbMsTb$ionization_mode[i] %in% pos_char | hmdbMsTb$adduct_type[i] %in% pos_char | hmdbMsTb$charge_type[i] %in% pos_char) return("pos")
      else if(hmdbMsTb$ionization_mode[i] %in% neg_char | hmdbMsTb$adduct_type[i] %in% neg_char | hmdbMsTb$charge_type[i] %in% neg_char) return("neg")
      else return(NA)
    }))
    adduct_new <- unname(sapply(1:nrow(hmdbMsTb), function(i) {
      adduct_orign <- hmdbMsTb$adduct[i]
      if(is.na(adduct_orign)){
        adduct_notes <- stringr::str_extract(hmdbMsTb$notes[i], "(?<=adduct_type ).*?(?= original_collision_energy)")
        if(is.na(adduct_notes)) return(NA)
        else if(adduct_notes == "NA") return(NA)
        else{
          has_M <- stringr::str_detect(adduct_notes, "M")
          has_square_brackets <- grepl("\\[.*?\\]", string)
          if(has_M & has_square_brackets) return(adduct_notes)
          else return(NA)
        }
      }
      else if(adduct_orign == "M-H") return("[M-H]-")
      else if(adduct_orign == "M+H") return("[M+H]+")
      else return(NA)
    }))
    instrument_vec <- unname(sapply(1:nrow(hmdbMsTb), function(i) {
      stringr::str_extract(hmdbMsTb$notes[i], "(?<=instrument=).*")
    }))
    hmdbMsTb$polarity <- polarity_vec
    hmdbMsTb$adduct_new <- adduct_new
    hmdbMsTb$instrument <- instrument_vec
    hmdbCmpTb <- pubcmpR::load_hmdbCmpTb()
    hmdbMsTb <- hmdbMsTb %>%
      dplyr::left_join(hmdbCmpTb, by = c("database_id" = "accession")) %>%
      dplyr::select(database_id, inchikey, chemical_formula, name, monisotopic_molecular_weight, smiles, inchi,
                    adduct_mass, adduct_new, polarity, collision_energy_voltage, instrument_type, instrument,predicted, mz, int)
    colnames(hmdbMsTb) <- c("accession", "inchikey", "formula", "name", "exactmass", "smiles", "inchi", "precursorMz",
                            "adduct", "polarity", "collision_energy", "instrument_type", "instrument", "predicted", "mz", "intensity")
  }
  return(hmdbMsTb)
}

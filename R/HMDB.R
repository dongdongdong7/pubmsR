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
                  basename(x))
    if (nonStop) {
      warning(msg)
      res$mz <- NULL
      res$int <- NULL
      return(res)
    }
    else stop(msg)
  }
  res$mz <- list(mz)
  res$int <- list(int)
  return(res)
}

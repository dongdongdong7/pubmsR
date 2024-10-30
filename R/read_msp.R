#' @title read_msp
#' @description
#' Read msp file. There are several styles of msp format, they are MoNA, NIST and RIKEN etc.
#'
#' @param file_path msp file path.
#' @param format MoNA, NIST, RIKE and CUSTOM. When using CUSTOM, you should input your own mapping.
#' @param mapping If mapping is NA and format is CUSTOM, it will use MsBackendMsp default msp style.
#' @param thread Thread number.
#'
#' @return A data.frame.
#' @export
#'
#' @examples
#' demo_MoNA_path <- system.file("demo", "demo_MoNA.msp", package = "pubmsR")
#'demo_NIST_path <- system.file("demo", "demo_NIST.msp", package = "pubmsR")
#'demo_RIKEN_path <- system.file("demo", "demo_RIKEN.msp", package = "pubmsR")
#'read_msp(demo_MoNA_path, format = "MoNA", thread = 1)
#'read_msp(demo_NIST_path, format = "NIST", thread = 1)
#'read_msp(demo_RIKEN_path, format = "RIKEN", thread = 1)
read_msp <- function(file_path, format = c("MoNA", "NIST", "RIKEN", "CUSTOM")[1], mapping = NA, thread = 1){
  if(format == "MoNA"){
    mapping <- c(name = "Name", accession = "DB#", formula = "Formula", inchikey = "InChiKey",
                adduct = "Precursor_type", exactmass = "ExactMass", precursorMz = "PrecursorMZ",
                polarity = "Ion_mode", collision_energy = "Collision_energy",
                instrument_type = "Instrument_type", instrument = "Instrument",
                comments = "Comments", num_peaks = "Num Peaks", synonym = "Synon", mw = "MW")
  }else if(format == "NIST"){
    mapping <- c(name = "Name", synonym = "Synon", accession = "DB#", inchikey = "InChiKey",
                 inchi = "InChi", smiles = "SMILES", adduct = "Precursor_type", spectrum_type = "Spectrum_type",
                 precursorMz = "PrecursorMZ", instrument_type = "Instrument_type", instrument = "Instrument",
                 polarity = "Ion_mode", collision_energy = "Collision_energy", formula = "Formula",
                 mw = "MW", exactmass = "ExactMass", comments = "Comments", splash = "Splash", num_peaks = "Num Peaks")
  }else if(format == "RIKEN"){
    mapping <- c(name = "NAME ", precursorMz = "PRECURSORMZ", adduct = "PRECURSORTYPE", formula = "FORMULA",
                 inchikey = "INCHIKEY", inchi = "INCHI", smiles = "SMILES", rtime = "RETENTIONTIME",
                 instrument_type = "INSTRUMENTTYPE", instrument = "INSTRUMENT", polarity = "IONMODE",
                 links = "LINKS", comments = "Comment", splash = "Splash", num_peaks = "Num Peaks")
  }else if(format == "CUSTOM"){
    if(is.na(mapping)) mapping <- MsBackendMsp::spectraVariableMapping(MsBackendMsp::MsBackendMsp())
    else mapping <- mapping
  }else stop("format is wrong!")
  if(thread == 1){
    BPPARAM = BiocParallel::SerialParam()
  }else if(thread > 1){
    if(thread > BiocParallel::snowWorkers()){
      warning("The number of threads is greater than the number of cpu's you have!")
      thread <- BiocParallel::snowWorkers()
    }
    BPPARAM = BiocParallel::SnowParam(workers = thread)
  }else{
    stop("Thread number is wrong!")
  }
  MsBackendMsp::readMsp(file_path, msLevel = 2L, mapping = mapping, BPPARAM = BPPARAM)
}

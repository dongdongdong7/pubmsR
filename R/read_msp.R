#' @title Reading MSP files
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
#' demo_NIST_path <- system.file("demo", "demo_NIST.msp", package = "pubmsR")
#' demo_RIKEN_path <- system.file("demo", "demo_RIKEN.msp", package = "pubmsR")
#' read_msp(demo_MoNA_path, format = "MoNA", thread = 1)
#' read_msp(demo_NIST_path, format = "NIST", thread = 1)
#' read_msp(demo_RIKEN_path, format = "RIKEN", thread = 1)
read_msp <- function(file_path, format = c("MoNA", "NIST", "RIKEN", "CUSTOM")[1], mapping = NA, thread = 1){
  if(format == "MoNA"){
    mapping <- c(name = "Name", accession = "DB#", formula = "Formula", inchikey = "InChiKey",
                adduct = "Precursor_type", spectrum_type = "Spectrum_type", exactmass = "ExactMass", precursorMz = "PrecursorMZ",
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
    BPPARAM = BiocParallel::SerialParam(stop.on.error = FALSE, progressbar = TRUE)
  }else if(thread > 1){
    if(thread > BiocParallel::snowWorkers()){
      warning("The number of threads is greater than the number of cpu's you have!")
      thread <- BiocParallel::snowWorkers()
    }
    BPPARAM = BiocParallel::SnowParam(workers = thread, stop.on.error = FALSE, progressbar = TRUE)
  }else{
    stop("Thread number is wrong!")
  }
  .readMsp(file_path, msLevel = 2L, mapping = mapping, BPPARAM = BPPARAM)
}

# This function is copy from MsBackendMsp
.readMsp <- function (f, msLevel = 2L, mapping = MsBackendMsp::spectraVariableMapping(MsBackendMsp::MsBackendMsp()),
                      BPPARAM = BiocParallel::SerialParam(), ...)
{
  if (length(f) != 1L)
    stop("Please provide a single msp file.")
  msp <- scan(file = f, what = "", sep = "\n", quote = "",
              allowEscapes = FALSE, quiet = TRUE, blank.lines.skip = FALSE,
              strip.white = TRUE)
  cmts <- grep("^[#]", msp)
  if (length(cmts))
    msp <- msp[-cmts]
  wsp <- grep("^[[:space:]]|(^$)", msp)
  begin <- c(1, wsp + 1L)
  end <- c(wsp - 1L, length(msp))
  keep <- begin < end
  begin <- begin[keep]
  end <- end[keep]
  df_tmp <- data.frame(a = begin, b = end)
  loop <- function(i){
    a <- df_tmp[i, "a"];b <- df_tmp[i, "b"]
    tryCatch(MsBackendMsp:::.extract_msp_spectrum(msp[a:b], mapping = mapping),
             error = function(e){
               print(e)
               NULL
             })
  }
  sp <- BiocParallel::bplapply(1:nrow(df_tmp), function(i){loop(i)}, BPPARAM = BPPARAM)
  sp <- sp[!sapply(sp, is.null)]
  res <- S4Vectors::DataFrame(MsCoreUtils::rbindFill(sp))
  spv <- Spectra::coreSpectraVariables()
  spv <- spv[!names(spv) %in% c("mz", "intensity")]
  for (i in seq_along(res)) {
    if (all(lengths(res[[i]]) == 1))
      res[[i]] <- unlist(res[[i]])
    if (any(col <- names(spv) == colnames(res)[i]))
      res[[i]] <- suppressWarnings(as(res[[i]], spv[col][1]))
  }
  res$mz <- IRanges::NumericList(res$mz, compress = FALSE)
  res$intensity <- IRanges::NumericList(res$intensity, compress = FALSE)
  res$dataOrigin <- f
  if (!any(colnames(res) == "msLevel"))
    res$msLevel <- as.integer(msLevel)
  res
}


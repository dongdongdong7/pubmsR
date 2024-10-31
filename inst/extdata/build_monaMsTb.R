# This is script to build monaMsTb from MoNA
# https://mona.fiehnlab.ucdavis.edu/downloads (LC-MS/MS Spectra MSP File)
# Barry Song

# library(magrittr)
# setwd("G:/spd/Projects/2024/pubmsR/Progress/experiments/build_monaMsTb/")
mona_msp_file_path <- "G:/spd/Projects/2024/pubmsR/Data/MoNA/MoNA-export-LC-MS-MS_Spectra.msp"
mona_DF <- pubmsR::read_msp(file_path = mona_msp_file_path, format = "MoNA", thread = 16)
mona_DF <- mona_DF[, -which(colnames(mona_DF) %in% c("dataOrigin", "msLevel"))]
monaMsTb <- tibble::as_tibble(mona_DF) %>%
  dplyr::select(accession, adduct, collision_energy, exactmass, formula, inchikey, instrument, instrument_type, polarity, precursorMz, mz, intensity, name)
smiles_vec <- unname(sapply(monaMsTb$comments, function(x) {
  stringr::str_extract(x, pattern = "(?<=SMILES=).*?(?=\")")
}))
inchi_vec <- unname(sapply(monaMsTb$comments, function(x) {
  stringr::str_extract(x, pattern = "(?<=InChI=).*?(?=\")")
}))
cas_vec <- unname(sapply(monaMsTb$comments, function(x) {
  stringr::str_extract(x, pattern = "(?<=cas=).*?(?=\")")
}))
monaMsTb$smiles <- smiles_vec
monaMsTb$inchi <- inchi_vec
monaMsTb$cas <- cas_vec
monaMsTb$exactmass <- as.numeric(monaMsTb$exactmass)
saveRDS(monaMsTb, file = "monaMsTb.rds")

unique(mona_DF$collision_energy)
unique(mona_DF$instrument_type)
unique(mona_DF$instrument)

# This is script to build lipidblastMsTb from LipidBlast
# https://fiehnlab.ucdavis.edu/projects/lipidblast
# Barry Song

# library(magrittr)
# setwd("G:/spd/Projects/2024/pubmsR/Progress/experiments/build_lipidblastMsTb/")
lipidblast_msp_file_path <- "G:/spd/Projects/2024/pubmsR/Data/LipidBlast/MoNA-export-LipidBlast.msp"
lipidblast_DF <- pubmsR::read_msp(file_path = lipidblast_msp_file_path, format = "LipidBlast", thread = 16)
lipidblast_DF <- lipidblast_DF[, -which(colnames(lipidblast_DF) %in% c("dataOrigin", "msLevel"))]
smiles_vec <- unname(sapply(lipidblast_DF$comments, function(x) {
  stringr::str_extract(x, pattern = "(?<=SMILES=).*?(?=\")")
}))
inchi_vec <- unname(sapply(lipidblast_DF$comments, function(x) {
  stringr::str_extract(x, pattern = "(?<=InChI=).*?(?=\")")
}))
lipidblast_DF$smiles <- smiles_vec
lipidblast_DF$inchi <- inchi_vec
lipidblast_DF$exactmass <- as.numeric(lipidblast_DF$exactmass)
lipidblastMsTb <- lipidblast_DF %>%
  dplyr::as_tibble() %>%
  dplyr::select(accession, adduct, collision_energy, exactmass, formula, inchikey, smiles, inchi, instrument, instrument_type, polarity, name, precursorMz, mz, intensity)
saveRDS(lipidblastMsTb, file = "lipidblastMsTb.rds")

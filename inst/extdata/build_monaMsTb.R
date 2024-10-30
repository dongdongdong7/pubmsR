# This is script to build monaMsTb from MoNA
# https://mona.fiehnlab.ucdavis.edu/downloads (LC-MS/MS Spectra MSP File)
# Barry Song

# library(magrittr)
# setwd("G:/spd/Projects/2024/pubmsR/Progress/experiments/build_monaMsTb/")
mona_msp_file_path <- "G:/spd/Projects/2024/pubmsR/Data/MoNA/MoNA-export-LC-MS-MS_Spectra.msp"
mona_DF <- pubmsR::read_msp(file_path = mona_msp_file_path, format = "MoNA", thread = 16)
mona_DF <- mona_DF[, -which(colnames(mona_DF) %in% c("dataOrigin", "msLevel"))]
monaMsTb <- tibble::as_tibble(mona_DF)
saveRDS(monaMsTb, file = "monaMsTb.rds")

unique(mona_DF$collision_energy)
unique(mona_DF$instrument_type)
unique(mona_DF$instrument)


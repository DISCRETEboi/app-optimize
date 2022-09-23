# This script defines a function that creates the rds version of each
# sas data file in the specified study directory.
# The new rds study directories will be subdirectories of '/RBM_JMPCLINICAL/JMP_entimICE_rds/'
# Example execution: "toRds('r20500a')" where 'r20500a' is a study directory of sas data files.
# The user only calls the function toRds()

library(haven)
library(stringr)

# path to directory that houses the study directories where the rds versions of the sas7bdat files will be stored.
prt_directory <- "/RBM_JMPCLINICAL/JMP_entimICE_rds/"
# path to directory that houses the study directories where the sas7bdat are collected.
org_directory <- "/RBM_JMPCLINICAL/JMP_entimICE/"

# prt_directory_me <- "RBM_JMPCLINICAL/JMP_entimICE_rds/"

# checks if the rds directory already exist. If false, creates the directory.
if (!dir.exists(prt_directory)) {
  dir.create(prt_directory)
}

# primary function which is called on the interested study.
# creates the rds versions of the sas7bdat files by calling the createRDS function.
# the study directories are stored as subdirectories of '/RBM_JMPCLINICAL/JMP_entimICE_rds/'.
toRds <- function(study) {
  files <- list.files(str_c(org_directory, study), pattern = '\\.sas7bdat$')
  filenames <- sapply(strsplit(files, '\\.'), function(rg){rg[1]})
  if (!dir.exists(str_c(prt_directory, study))) {
    dir.create(str_c(prt_directory, study))
  }
  invisible(lapply(filenames, createRDS, study))
}

# creates the rds data file of the sas data filename passed to it.
createRDS <- function(filename, study) {
  dt <- read_sas(str_c(org_directory, study, '/', filename, '.sas7bdat'))
  saveRDS(dt, str_c(prt_directory ,study, '/', filename, '.rds'))
  # rm(dt)
  # cat(filename, ' => success :)', '\n', sep = '')
}

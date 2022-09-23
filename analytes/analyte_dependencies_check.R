# This script defines a function which takes in the path to a package & an analyte specification
# then returns the names of the scripts which contain the function dependencies

library(stringr)
library(magrittr)

returnDependencies <- function(pkg_path, analytes) {
  pkg_scripts <- list.files(paste0(pkg_path, "/R/"), pattern = "\\.R$")
  file_dependencies_all <- character(0)
  
  check_dependency <- function(function_name) {
    pkg_scripts_stripped_names <- sub("\\.R$", "", pkg_scripts)
    file_dependency_exp <- paste0(function_name, ".R")
    is_file_present <- function_name %in% pkg_scripts_stripped_names
    if (is_file_present) {
      return(file_dependency_exp)
    } else {
      return(NA)
    }
  }
  
  for (i in analytes) {
    analyte_exp_script <- paste0(i, ".R")
    if (analyte_exp_script %in% pkg_scripts) {
      script_lines_text <- parse(paste0(pkg_path, "/R/", analyte_exp_script)) %>% as.character() %>%
        strsplit("\n") %>% unlist()
      function_calls <- str_extract(script_lines_text, "[^ ]+\\(") %>% gsub(".*::|\\(", "", .) %>% na.omit()
      file_dependencies <- sapply(function_calls, check_dependency) %>% na.omit()
      file_dependencies_all <- c(file_dependencies_all, file_dependencies)
    } else {
      message("The analyte '", i, "' does not have the file '", analyte_exp_script, "' present!")
    }
  }
  
  files_analytes <- gsub("\\.R$", "", file_dependencies_all)
  if (length(file_dependencies_all) > 0) {
    file_dependencies_all_recurse <- unique(c(file_dependencies_all, returnDependencies(pkg_path, files_analytes)))
    return(file_dependencies_all_recurse)
  } else {
    return(unique(file_dependencies_all))
  }
}

#pkg_path <- "CDM/d5496c00005"; analytes <- c("sspec_subject_ip", "eligi_pi")
#cat(returnDependencies(pkg_path, analytes))

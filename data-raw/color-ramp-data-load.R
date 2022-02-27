#Monochrome palette data
nn_mono_pal_data = readr::read_rds("inst/extdata/parsed_mono_pal_data.rds")
usethis::use_data(nn_mono_pal_data,internal = TRUE,overwrite = TRUE)

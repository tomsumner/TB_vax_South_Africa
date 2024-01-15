
suppressPackageStartupMessages({
  rm(list=ls())
  library(here)
  library(data.table)
  library(xml2)
})


BCG_paths <- c("BCG_NCI_POI_45_10yr_med_2025.xml",
               "BCG_NCI_POI_70_10yr_med_2025.xml", 
               "BCG_NCI_POI_45_5yr_med_2025.xml",
               "BCG_NCI_POI_45_15yr_med_2025.xml",
               "BCG_NCI_POI_45_20yr_med_2025.xml",
               "BCG_AI_POI_45_10yr_med_2025.xml",
               "BCG_NCI_POID_45_10yr_med_2025.xml",
               "BCG_NCI_POI_45_10yr_med_2029.xml",
               "BCG_NCI_POI_45_10yr_med_2025_diffages.xml",
               "BCG_NCI_POI_45_10yr_low_2025.xml",
               "BCG_NCI_POI_45_10yr_high_2025.xml",
               "BCG_NCI_POI_45_10yr_med_2025_artsame.xml",
               "BCG_NCI_POI_45_10yr_med_2025_artdiff.xml",
               "BCG_NCI_POI_45_10yr_med_2025_vsupsame.xml",
               "BCG_NCI_POI_45_10yr_med_2025_vsupdiff.xml"
               #"BCG_NCI_POI_45_10yr_med_2025_endTB.xml"
               )

M72_paths <- c("M72_AI_POD_50_10yr_med_2030.xml", 
          "M72_AI_POD_60_10yr_med_2030.xml",
          "M72_AI_POD_70_10yr_med_2030.xml",
          "M72_AI_POD_50_5yr_med_2030.xml",
          "M72_AI_POD_50_15yr_med_2030.xml",
          "M72_AI_POD_50_20yr_med_2030.xml",
          "M72_CI_POD_50_10yr_med_2030.xml",
          "M72_AI_POID_50_10yr_med_2030.xml",
          "M72_AI_POD_50_10yr_med_2034.xml",
          "M72_AI_POD_50_10yr_med_2030_diffages.xml",
          "M72_AI_POD_50_10yr_med_2030_young.xml",
          "M72_AI_POD_50_10yr_low_2030.xml",
          "M72_AI_POD_50_10yr_high_2030.xml",
          "M72_AI_POD_50_10yr_med_2030_hivsame.xml",
          "M72_AI_POD_50_10yr_med_2030_hivpess.xml"
               )


for (k in 1:length(BCG_paths)){
  
  input_vx <- read_xml(paste0("./countries/ZAF/parameters/BCGepi/XMLinput_", BCG_paths[k]))
  params_output  <- xml_find_all(input_vx, xpath='//output')
  
  ## Econ output in single age groups
  xml_set_attr(x=xml_find_all(params_output, 'detailed.output'),
               attr = "age.group.lower.limits", value = "c(0:80,90)")

  write_xml(input_vx, file = paste0("countries/ZAF/parameters/BCGecon/XMLinput_", BCG_paths[k])) 
  
  }


for (k in 1:length(M72_paths)){
  
  input_vx <- read_xml(paste0("./countries/ZAF/parameters/M72epi/XMLinput_", M72_paths[k]))
  params_output  <- xml_find_all(input_vx, xpath='//output')
  
  ## Econ output in single age groups
  xml_set_attr(x=xml_find_all(params_output, 'detailed.output'),
               attr = "age.group.lower.limits", value = "c(0:80,90)")
  
  write_xml(input_vx, file = paste0("countries/ZAF/parameters/M72econ/XMLinput_", M72_paths[k])) 
  
}





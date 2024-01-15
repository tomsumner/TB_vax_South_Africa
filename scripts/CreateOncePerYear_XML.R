# --------------------------------------
# Create XMLs based on "continous vx" XMLs
# Rebecca Clark
# 13 October 2022
# --------------------------------------

m72_xml <- list.files("./countries/IND/parameters/M72epi/")

for (k in 1:length(m72_xml)){
  
  input_vx <- read_xml(paste0("./countries/IND/parameters/M72epi/", m72_xml[k]))
  
  params_vx <- xml_find_all(input_vx, xpath='//VXa/VXa.incidence')
  
  xml_set_attr(x=xml_find_all(x = params_vx, xpath='incidence.data'),
               attr = "once.per.year", value = "true")
  
  write_xml(input_vx, file = paste0("countries/IND/parameters/M72epi/", m72_xml[k])) 
}


bcg_xml <- list.files("./countries/IND/parameters/BCGepi/")

for (k in 1:length(bcg_xml)){
  
  input_vx <- read_xml(paste0("./countries/IND/parameters/BCGepi/", bcg_xml[k]))
  
  params_vx <- xml_find_all(input_vx, xpath='//VXa/VXa.incidence')
  
  xml_set_attr(x=xml_find_all(x = params_vx, xpath='incidence.data'),
               attr = "once.per.year", value = "true")
  
  write_xml(input_vx, file = paste0("countries/IND/parameters/BCGepi/", bcg_xml[k])) 
}



m72_xml <- list.files("./countries/IND/parameters/M72econ/")

for (k in 1:length(m72_xml)){
  
  input_vx <- read_xml(paste0("./countries/IND/parameters/M72econ/", m72_xml[k]))
  
  params_vx <- xml_find_all(input_vx, xpath='//VXa/VXa.incidence')
  
  xml_set_attr(x=xml_find_all(x = params_vx, xpath='incidence.data'),
               attr = "once.per.year", value = "true")
  
  write_xml(input_vx, file = paste0("countries/IND/parameters/M72econ/", m72_xml[k])) 
}


bcg_xml <- list.files("./countries/IND/parameters/BCGecon/")

for (k in 1:length(bcg_xml)){
  
  input_vx <- read_xml(paste0("./countries/IND/parameters/BCGecon/", bcg_xml[k]))
  
  params_vx <- xml_find_all(input_vx, xpath='//VXa/VXa.incidence')
  
  xml_set_attr(x=xml_find_all(x = params_vx, xpath='incidence.data'),
               attr = "once.per.year", value = "true")
  
  write_xml(input_vx, file = paste0("countries/IND/parameters/BCGecon/", bcg_xml[k])) 
}
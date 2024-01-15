# Jan 20 2022
#-----------------------
rm(list=ls())
# Set-up
library(data.table)
library(xml2)
library(here)


##BCG

vx_char <- fread("scripts/BCG_scenariosb.csv")
#lmic <- lmic[1:82,]

# Load in the template BCG

for(i in 1:nrow(vx_char)){
input    <- read_xml("scripts/XMLinputVx_poi_template.xml")
tb_trans  <- xml_find_all(input, xpath='//TB/TB.transmission')
vx_prog   <- xml_find_all(input, xpath='//VXa/VXa.progression')
vx_inc <- xml_find_all(input, xpath='//VXa/VXa.incidence')



efficacy    <- vx_char$Efficacy[i]
dop <- vx_char$wane[i]
vx_inc_file1 <- paste0("data/",vx_char$data_file_name1[i])
vx_inc_file2 <- paste0("data/",vx_char$data_file_name2[i])



# replace the XMLinput values with the country specific values

xml_set_attr(x=xml_find_all(x = tb_trans, xpath='TB.parameter[@VXa.stage="vac"]'),
             attr = "value", value = efficacy)
xml_set_attr(x=xml_find_all(x = vx_prog, xpath='VXa.parameter[@TB.stage = "Un"]'),
             attr = "value", value = dop)
xml_set_attr(x=xml_find_all(x = vx_prog, xpath='VXa.parameter[@TB.stage = "Uc"]'),
             attr = "value", value = dop)

xml_set_attr(x=xml_find_all(x = vx_prog, xpath='VXa.parameter[@TB.stage = "Lf"]'),
             attr = "value", value = dop)
xml_set_attr(x=xml_find_all(x = vx_prog, xpath='VXa.parameter[@TB.stage = "Ls"]'),
             attr = "value", value = dop)
xml_set_attr(x=xml_find_all(x = vx_prog, xpath='VXa.parameter[@TB.stage = "R"]'),
             attr = "value", value = dop)

#poi

xml_set_attr(x=xml_find_all(x = vx_inc, xpath='incidence.data[@file = "data/name_to_replace.txt"]'),
             attr = "file", value = vx_inc_file1)
xml_set_attr(x=xml_find_all(x = vx_inc, xpath='incidence.data[@file = "data/name_to_replace_prev.txt"]'),
             attr = "file", value = vx_inc_file2)

write_xml(input, file = paste0("countries/ZAF/parameters/",vx_char$xml_file_name_epi[i])) 


}


#####xmlinput_epi to xmlinput_econ BCG

vx_char <- fread("scripts/BCG_scenariosb.csv")

for(i in 1:nrow(vx_char)){
input    <- read_xml(paste0("countries/ZAF/parameters/",vx_char$xml_file_name_epi[i]))
op  <- xml_find_all(input, xpath='//output')

xml_set_attr(x=xml_find_all(x = op, xpath='detailed.output[@age.group.lower.limits = "c(0,15)"]'),
             attr = "age.group.lower.limits", value = "c(0:80,90)")

xml_set_attr(x=xml_find_all(x = op, xpath='detailed.output[@years = "c(2026:2020)+0.5"]'),
             attr = "years", value = "c(2023:2050)+0.5")

write_xml(input, file = paste0("countries/ZAF/parameters/",vx_char$xml_file_name_econ[i])) 
}


##########




####M72

vx_char <- fread("scripts/M72_scenarios.csv")


for(i in 1:nrow(vx_char)){
# Load in the template XMLs M72
input    <- read_xml("scripts/XMLinputVx_pod_template_diffeff.xml")
tb_prog  <- xml_find_all(input, xpath='//TB/TB.progression')
vx_prog   <- xml_find_all(input, xpath='//VXa/VXa.progression')
vx_inc <- xml_find_all(input, xpath='//VXa/VXa.incidence')


efficacy    <- vx_char$Efficacy[i]
dop <- vx_char$wane[i]
vx_inc_file1 <- paste0("data/",vx_char$data_file_name1[i])
vx_inc_file2 <- paste0("data/",vx_char$data_file_name2[i])

 
  # replace the XMLinput values with the country specific values
  
  xml_set_attr(x=xml_find_all(x = tb_prog, xpath='TB.parameter[@VXa.stage="vac"]'),
               attr = "value", value = efficacy)
  xml_set_attr(x=xml_find_all(x = vx_prog, xpath='VXa.parameter[@TB.stage = "Un"]'),
               attr = "value", value = dop)
  xml_set_attr(x=xml_find_all(x = vx_prog, xpath='VXa.parameter[@TB.stage = "Uc"]'),
               attr = "value", value = dop)
  
  xml_set_attr(x=xml_find_all(x = vx_prog, xpath='VXa.parameter[@TB.stage = "Lf"]'),
               attr = "value", value = dop)
  xml_set_attr(x=xml_find_all(x = vx_prog, xpath='VXa.parameter[@TB.stage = "Ls"]'),
               attr = "value", value = dop)
  xml_set_attr(x=xml_find_all(x = vx_prog, xpath='VXa.parameter[@TB.stage = "R"]'),
               attr = "value", value = dop)
  
  #pod
  xml_set_attr(x=xml_find_all(x = vx_inc, xpath='incidence.data[@file = "data/name_to_replace_vac.txt"]'),
               attr = "file", value = vx_inc_file1)
  
  xml_set_attr(x=xml_find_all(x = vx_inc, xpath='incidence.data[@file = "data/name_to_replace_prev.txt"]'),
               attr = "file", value = vx_inc_file2)
  
   
 
  write_xml(input, file = paste0("countries/ZAF/parameters/",vx_char$xml_file_name_epi[i])) 
  
}


##xml epi to econ output for M72

vx_char <- fread("scripts/M72_scenarios.csv")

for(i in 1:nrow(vx_char)){
  input    <- read_xml(paste0("countries/ZAF/parameters/",vx_char$xml_file_name_epi[i]))
  op  <- xml_find_all(input, xpath='//output')
  
  xml_set_attr(x=xml_find_all(x = op, xpath='detailed.output[@age.group.lower.limits = "c(0,15)"]'),
               attr = "age.group.lower.limits", value = "c(0:80,90)")
  
  write_xml(input, file = paste0("countries/ZAF/parameters/",vx_char$xml_file_name_econ[i])) 
}
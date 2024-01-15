# Vaccine Scenario Labelling

#--------------------
# M72-like Vaccines
#--------------------

epi_M72$Runtype <- 
  ordered(epi_M72$Runtype,
          levels = c("baseline", "M72_AI_POD_50_10yr_med_2030",
                     "M72_AI_POD_60_10yr_med_2030", "M72_AI_POD_70_10yr_med_2030",
                     "M72_AI_POD_50_5yr_med_2030",
                     "M72_AI_POD_50_15yr_med_2030", "M72_AI_POD_50_20yr_med_2030",
                     "M72_AI_POID_50_10yr_med_2030", "M72_CI_POD_50_10yr_med_2030",
                     "M72_AI_POD_50_10yr_low_2030", "M72_AI_POD_50_10yr_high_2030",
                     "M72_AI_POD_50_10yr_med_2034", 
                     "M72_AI_POD_50_10yr_med_2030_diffages", 
                     "M72_AI_POD_50_10yr_med_2030_young",
                     "M72_AI_POD_50_10yr_med_2030_hivsame",
                     "M72_AI_POD_50_10yr_med_2030_hivpess"
                     ),
          labels = c("No-New-Vaccine", "Basecase",
                     "60% efficacy", "70% efficacy", 
                     "5 years duration of protection",
                     "15 years duration of protection", "20 years duration of protection",
                     "Prevention of infection & disease", "Efficacy with current infection at vaccination", 
                     "Low coverage", "High coverage",
                     "2034 introduction",
                     "Older ages (campaign for ages 18-55)", "Younger ages (campaign for ages 11-34, routine age 10)",
                     "Same efficacy in PLHIV", "Low efficacy in PLHIV"))


#--------------------
# BCG-like Vaccines
#--------------------

epi_BCG$Runtype <- 
  ordered(epi_BCG$Runtype,
          levels = c("baseline", "BCG_NCI_POI_45_10yr_med_2025", 
                     "BCG_NCI_POI_70_10yr_med_2025", 
                     "BCG_NCI_POI_45_5yr_med_2025",
                     "BCG_NCI_POI_45_15yr_med_2025", "BCG_NCI_POI_45_20yr_med_2025",
                     "BCG_NCI_POID_45_10yr_med_2025",
                     "BCG_AI_POI_45_10yr_med_2025", 
                     "BCG_NCI_POI_45_10yr_low_2025", "BCG_NCI_POI_45_10yr_high_2025",
                     "BCG_NCI_POI_45_10yr_med_2029", "BCG_NCI_POI_45_10yr_med_2025_diffages",
                     "BCG_NCI_POI_45_10yr_med_2025_artsame", 
                     "BCG_NCI_POI_45_10yr_med_2025_all_artsame",
                     "BCG_NCI_POI_45_10yr_med_2025_all_artdiff",
                     "BCG_NCI_POI_45_10yr_med_2025_all_vsupsame",
                     "BCG_NCI_POI_45_10yr_med_2025_all_vsupdiff"
                     ),
          labels = c("No-New-Vaccine", "Basecase",
                     "70% efficacy", "5 years duration of protection",
                     "15 years duration of protection", "20 years duration of protection",
                     "Prevention of infection & disease",
                     "Efficacy with any infection at vaccination",
                     "Low coverage", "High coverage",
                     "2029 introduction", "Older ages (campaign for ages 16-34, routine age 15)",
                     "Vx PLHIV on ART, same efficacy (campaign for ages 11-18)",
                     "Vx PLHIV on ART, same efficacy (campaign for ages 10+)",
                     "Vx PLHIV on ART, lower efficacy (campaign for ages 10+)",
                     "Vx PLHIV on ART + virally suppressed, same efficacy (campaign for ages 10+)",
                     "Vx PLHIV on ART + virally suppressed, lower efficacy (campaign for ages 10+)"
          ))


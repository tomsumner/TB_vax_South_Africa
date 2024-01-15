# Vaccine Scenario Labelling

#--------------------
# M72-like Vaccines
#--------------------


#--------------------
# BCG-like Vaccines
#--------------------

# epi_BCG$Runtype <- 
#   ordered(epi_BCG$Runtype,
#           levels = c("baseline", "BCG_NCI_POI_45_10yr_med_2025", 
#                      "BCG_NCI_POI_45_10yr_med_2025_artsame", 
#                      "BCG_NCI_POI_45_10yr_med_2025_diffageswart",
#                      "BCG_NCI_POI_45_10yr_med_2025_nohiv_age10",
#                      "BCG_NCI_POI_45_10yr_med_2025_nohiv_age20",
#                      "BCG_NCI_POI_45_10yr_med_2025_nohiv_age30",
#                      "BCG_NCI_POI_45_10yr_med_2025_nohiv_age40",
#                      "BCG_NCI_POI_45_10yr_med_2025_nohiv_age50",
#                      "BCG_NCI_POI_45_10yr_med_2025_artsame_age10",
#                      "BCG_NCI_POI_45_10yr_med_2025_artsame_age20",
#                      "BCG_NCI_POI_45_10yr_med_2025_artsame_age30",
#                      "BCG_NCI_POI_45_10yr_med_2025_artsame_age40",
#                      "BCG_NCI_POI_45_10yr_med_2025_artsame_age50"
#                      
#                      ),
#           labels = c("No-New-Vaccine", "Basecase",
#                      "Same efficacy in PLHIV on ART",
#                      "Same efficacy in PLHIV on ART (campaign 11-34 & routine 10)",
#                      "HIV negative only campaign for 10-19",
#                      "HIV negative only campaign for 20-29",
#                      "HIV negative only campaign for 30-39",
#                      "HIV negative only campaign for 40-49",
#                      "HIV negative only campaign for 50-59",
#                      "On ART only with campaign for 10-19",
#                      "On ART only with campaign for 20-29",
#                      "On ART only with campaign for 30-39",
#                      "On ART only with campaign for 40-49",
#                      "On ART only with campaign for 50-59"
#                      ))
# 

##M72
epi_BCG$Runtype <- 
  ordered(epi_BCG$Runtype,
          levels = c("baseline",
                     "BCG_NCI_POI_45_10yr_med_2025_artsame", 
                     "BCG_NCI_POI_45_10yr_med_2025_all_artsame",
                     "BCG_NCI_POI_45_10yr_med_2025_all_artdiff",
                     "BCG_NCI_POI_45_10yr_med_2025_all_vsupsame",
                     "BCG_NCI_POI_45_10yr_med_2025_all_vsupdiff"
          ),
          labels = c("No-New-Vaccine",
                     "Same efficacy in PLHIV on ART",
                     "Campaign for PLHIV on ART, age 10+ (same efficacy)",
                     "Campaign PLHIV on ART, age 10+ (different efficacy)",
                     "Campaign for PLHIV virally suppressed on ART, age 10+ (same efficacy)",
                     "Campaign for PLHIV virally suppressed on ART, age 10+ (different efficacy)"
          ))



# Groups for plotting 
coverage <- c("No-New-Vaccine", "Low coverage", "Basecase", "High coverage")
efficacy <- c("No-New-Vaccine", "Basecase", "60% efficacy", "70% efficacy")
duration <- c("No-New-Vaccine", "Basecase", "5 years duration of protection",
              "15 years duration of protection", "20 years duration of protection")
ages <- c("No-New-Vaccine", "Basecase", "Different Ages", "Elderly targeting")
mecheffect <- c("No-New-Vaccine", "Basecase", "POID")
hoststatus <- c("No-New-Vaccine", "Basecase", "AI", "CI")
year <- c("No-New-Vaccine", "Basecase", "2029 introduction", "2034 introduction")
hiv <- c("No-New-Vaccine", "Basecace", "Same efficacy in PLHIV on ART",
         "Different efficacy in PLHIV on ART",
         "Same efficacy in PLHIV on ART + virally suppressed",
         "DIfferent efficacy in PLHIV on ART + virally suppressed","Same efficacy in PLHIV", "Low efficacy in PLHIV")

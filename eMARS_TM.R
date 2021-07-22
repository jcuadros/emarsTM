options(install.packages.check.source = "no", warn = -1)

pckgs<-c("jsonlite", "tidyverse","textclean", "quanteda","udpipe",
         "wordcloud","readxl","NLP","tm","openNLP","openNLPmodels.en","skmeans",
         "rJava", "UpSetR","writexl","usefun")
pckgs2Install<-pckgs[!(pckgs %in% library()$results[,1])]
pckgs2Load<-pckgs[!(pckgs %in% (.packages()))]
for(pckg in pckgs2Install) {install.packages(pckg,repos="https://cloud.r-project.org/",
                                             quiet=TRUE, type="binary")}
for(pckg in pckgs2Load) {library(pckg,character.only = TRUE)}


#DATA####
    ### ... import ####

route_report <- read.csv("C:/Users/polja/Desktop/IQS/MEQ/TFM/eMarsRoute.csv", header = FALSE)

nreports <- nrow(route_report)

eMARS_import_df <- data.frame("AccidentID" = vector(length=nreports), "Title" = vector(length=nreports), "Date" = vector(length=nreports), 
                         "LastUpdate" = vector(length=nreports), "Legislation" = vector(length=nreports), "SevesoIIStatus" = vector(length=nreports),
                         "EventType" = vector(length=nreports), "IndustryType" = vector(length=nreports), "AccidentDescription" = vector(length=nreports),
                         "SiteDescription" = vector(length=nreports), #"Substances" = vector(length=nreports), #"CASNumber" = vector(length=nreports),
                         "InstallationDescription" = vector(length=nreports), "SubstanceInvolvedDescription" = vector(length=nreports),
                         "CausesDescription" = vector(length=nreports), "ConsequenceDescription" = vector(length=nreports), 
                         "EmergencyResponse" = vector(length=nreports), "LessonsLearned" = vector(length=nreports))


for(i in 1:nreports) {
  reportJSON <- fromJSON(route_report[i,1])
  eMARS_import_df[i,"AccidentID"]=reportJSON$Data$Accident$AccidentId
  eMARS_import_df[i,"Title"]=reportJSON$Data$AccidentVersion$Translation$Title
  eMARS_import_df[i,"Date"]=reportJSON$Data$AccidentVersion$EndYear
  eMARS_import_df[i,"LastUpdate"]=reportJSON$Data$AccidentVersion$LastUpdateDate
  eMARS_import_df[i,"Legislation"]=reportJSON$Data$AccidentVersion$Legislation
  eMARS_import_df[i,"SevesoIIStatus"]=reportJSON$Data$AccidentVersion$SevesoIIStatus
  eMARS_import_df[i,"AccidentType"]=reportJSON$Data$AccidentVersion$EventType
  eMARS_import_df[i,"IndustryType"]=reportJSON$Data$AccidentVersion$IndustryType
  eMARS_import_df[i,"AccidentDescription"]=reportJSON$Data$AccidentDescription_Tab$AccidentDescription$Translation$AccidentDescription
  eMARS_import_df[i,"SiteDescription"]=reportJSON$Data$SiteAndInstallation_Tab$AccidentSiteAndInstallation$Translation$Site_Description
  eMARS_import_df[i,"InstallationDescription"]=reportJSON$Data$SiteAndInstallation_Tab$AccidentSiteAndInstallation$Translation$Installation_Description
  eMARS_import_df[i,"SubstanceInvolvedDescription"]=reportJSON$Data$SubstancesInvolved_Tab$SubstancesDescription$Translation$Description
  #eMARS_import_df[i,"Substances"]=paste(unlist(reportJSON$Data$SubstancesInvolved_Tab$SubstancesInvolved$Substance), collapse =";")
  #eMARS_import_df[i,"CASNumber"]=paste(unlist(reportJSON$Data$SubstancesInvolved_Tab$SubstancesInvolved$CASNumber), collapse =";")
  eMARS_import_df[i,"CausesDescription"]=reportJSON$Data$Causes_Tab$CausesDescription$Translation$Causes_Description
  eMARS_import_df[i,"ConsequenceDescription"]=reportJSON$Data$Consequence_Tab$AccidentConsequence$Consequence_Description
  eMARS_import_df[i,"EmergencyResponse"]=reportJSON$Data$AccidentVersion$Translation$EmergencyResponce_Descr
  eMARS_import_df[i,"LessonsLearned"]=reportJSON$Data$AccidentVersion$Translation$LessonsLearned
}
  

    ### ... clean ####
   

eMARS_df <- data.frame("AccidentID" = vector(length=nreports), "Title" = vector(length=nreports), "Date" = vector(length=nreports), 
                              "LastUpdate" = vector(length=nreports), "Legislation" = vector(length=nreports), "SevesoIIStatus" = vector(length=nreports),
                              "EventType" = vector(length=nreports), "IndustryType" = vector(length=nreports), "AccidentDescription" = vector(length=nreports),
                              "SiteDescription" = vector(length=nreports), #"Substances" = vector(length=nreports), #"CASNumber" = vector(length=nreports),
                              "InstallationDescription" = vector(length=nreports), "SubstanceInvolvedDescription" = vector(length=nreports),
                              "CausesDescription" = vector(length=nreports), "ConsequenceDescription" = vector(length=nreports), 
                              "EmergencyResponse" = vector(length=nreports), "LessonsLearned" = vector(length=nreports))

for(i in 1:nreports) {
  eMARS_df[i,"AccidentID"]=clean_data_f(eMARS_import_df$AccidentID[i])
  eMARS_df[i,"Title"]=clean_data_f(eMARS_import_df$Title[i])
  eMARS_df[i,"Date"]=clean_data_f(eMARS_import_df$Date[i])
  eMARS_df[i,"LastUpdate"]=clean_data_f(eMARS_import_df$LastUpdate[i])
  eMARS_df[i,"Legislation"]=clean_data_f(eMARS_import_df$Legislation[i])
  eMARS_df[i,"SevesoIIStatus"]=clean_data_f(eMARS_import_df$SevesoIIStatus[i])
  eMARS_df[i,"AccidentType"]=clean_data_f(eMARS_import_df$AccidentType[i])
  eMARS_df[i,"IndustryType"]=clean_data_f(eMARS_import_df$IndustryType[i])
  eMARS_df[i,"AccidentDescription"]=clean_data_f(eMARS_import_df$AccidentDescription[i])
  eMARS_df[i,"SiteDescription"]=clean_data_f(eMARS_import_df$SiteDescription[i])
  eMARS_df[i,"InstallationDescription"]=clean_data_f(eMARS_import_df$InstallationDescription[i])
  eMARS_df[i,"SubstanceInvolvedDescription"]=clean_data_f(eMARS_import_df$SubstanceInvolvedDescription[i])
  #eMARS_df[i,"Substances"]=clean_data_f(eMARS_import_df$Substances[i])
  #eMARS_df[i,"CASNumber"]=clean_data_f(eMARS_import_df$CASNumber[i])
  eMARS_df[i,"CausesDescription"]=clean_data_f(eMARS_import_df$CausesDescription[i])
  eMARS_df[i,"ConsequenceDescription"]=clean_data_f(eMARS_import_df$ConsequenceDescription[i])
  eMARS_df[i,"EmergencyResponse"]=clean_data_f(eMARS_import_df$EmergencyResponse[i])
  eMARS_df[i,"LessonsLearned"]=clean_data_f(eMARS_import_df$LessonsLearned[i])
}

    ### ... fire_bigram ####

    fire_stopwords <- c("fire brigade", "fire service", "fire fighting", "fire alarm", "fire brigades", "fire fighters",
                        "fire fighter", "fire services", "fire department", "fire engines", "fire protection", "fire team",
                        "fire extinguishers", "automatic fire", "fire -fighters", "fire monitors", "fire detection",
                        "fire safety", "fire vehicles", "fire extinguisher", "fire detectors", "fire extinguishing")

    fire_sub <- c("fire_brigade", "fire_service", "fire_fighter", "fire_alarm", "fire_brigade", "fire_fighter",
                  "fire_fighter", "fire_service", "fire_department", "fire_engine", "fire_protection", "fire_team",
                  "fire_extinguisher", "automatic_fire", "fire_fighter", "fire_monitor", "fire_detection",
                  "fire_safety", "fire_vehicle", "fire_extinguish", "fire_detector", "fire_extinguish")



    eMARS_firebigram_df <- data.frame("AccidentID" = vector(length=nreports), "Title" = vector(length=nreports), "Date" = vector(length=nreports), 
                                     "LastUpdate" = vector(length=nreports), "Legislation" = vector(length=nreports), "SevesoIIStatus" = vector(length=nreports),
                                     "EventType" = vector(length=nreports), "IndustryType" = vector(length=nreports), "AccidentDescription" = vector(length=nreports),
                                     "SiteDescription" = vector(length=nreports), #"Substances" = vector(length=nreports), #"CASNumber" = vector(length=nreports),
                                     "InstallationDescription" = vector(length=nreports), "SubstanceInvolvedDescription" = vector(length=nreports),
                                     "CausesDescription" = vector(length=nreports), "ConsequenceDescription" = vector(length=nreports), 
                                     "EmergencyResponse" = vector(length=nreports), "LessonsLearned" = vector(length=nreports))


    for(i in 1:nreports) {
      
      eMARS_firebigram_df[i,"AccidentID"]=eMARS_df$AccidentID[i]
      eMARS_firebigram_df[i,"Title"]=eMARS_df$Title[i]
      eMARS_firebigram_df[i,"Date"]=eMARS_df$Date[i]
      eMARS_firebigram_df[i,"LastUpdate"]=eMARS_df$LastUpdate[i]
      eMARS_firebigram_df[i,"Legislation"]=eMARS_df$Legislation[i]
      eMARS_firebigram_df[i,"SevesoIIStatus"]=eMARS_df$SevesoIIStatus[i]
      eMARS_firebigram_df[i,"AccidentType"]=eMARS_df$AccidentType[i]
      eMARS_firebigram_df[i,"IndustryType"]=eMARS_df$IndustryType[i]
      eMARS_firebigram_df[i,"AccidentDescription"]=tolower(eMARS_df$AccidentDescription[i])
      eMARS_firebigram_df[i,"SiteDescription"]=tolower(eMARS_df$SiteDescription[i])
      eMARS_firebigram_df[i,"InstallationDescription"]=tolower(eMARS_df$InstallationDescription[i])
      eMARS_firebigram_df[i,"SubstanceInvolvedDescription"]=tolower(eMARS_df$SubstanceInvolvedDescription[i])
      #eMARS_firebigram_df[i,"Substances"]=eMARS_df$Substances[i]
      #eMARS_firebigram_df[i,"CASNumber"]=eMARS_df$CASNumber[i]
      eMARS_firebigram_df[i,"CausesDescription"]=tolower(eMARS_df$CausesDescription[i])
      eMARS_firebigram_df[i,"ConsequenceDescription"]=tolower(eMARS_df$ConsequenceDescription[i])
      eMARS_firebigram_df[i,"EmergencyResponse"]=tolower(eMARS_df$EmergencyResponse[i])
      eMARS_firebigram_df[i,"LessonsLearned"]=tolower(eMARS_df$LessonsLearned[i])
      
      
      
      for (j in 1:length(fire_stopwords)){
        
      eMARS_firebigram_df[i,"AccidentDescription"]=gsub(fire_stopwords[j],fire_sub[j],tolower(eMARS_firebigram_df$AccidentDescription[i]))
      eMARS_firebigram_df[i,"SiteDescription"]=gsub(fire_stopwords[j],fire_sub[j],tolower(eMARS_firebigram_df$SiteDescription[i]))
      eMARS_firebigram_df[i,"InstallationDescription"]=gsub(fire_stopwords[j],fire_sub[j],tolower(eMARS_firebigram_df$InstallationDescription[i]))
      eMARS_firebigram_df[i,"SubstanceInvolvedDescription"]=gsub(fire_stopwords[j],fire_sub[j],tolower(eMARS_firebigram_df$SubstanceInvolvedDescription[i]))

      eMARS_firebigram_df[i,"CausesDescription"]=gsub(fire_stopwords[j],fire_sub[j],tolower(eMARS_firebigram_df$CausesDescription[i]))
      eMARS_firebigram_df[i,"ConsequenceDescription"]=gsub(fire_stopwords[j],fire_sub[j],tolower(eMARS_firebigram_df$ConsequenceDescription[i]))
      eMARS_firebigram_df[i,"EmergencyResponse"]=gsub(fire_stopwords[j],fire_sub[j],tolower(eMARS_firebigram_df$EmergencyResponse[i]))
      eMARS_firebigram_df[i,"LessonsLearned"]=gsub(fire_stopwords[j],fire_sub[j],tolower(eMARS_firebigram_df$LessonsLearned[i]))
      
      }
      
    }



    
    counts <- table(eMARS_df$Date)
    barplot(counts , main= "Data Timeline",
            xlab ="Year", ylab = "Accidents",
            font.lab = 2, cex.axis = 1.5, cex.lab = 2,
            cex.names = 1.5 , cex.main = 2) 
  
    
    
    
#CHECKBOXES####

eMARS_checkbox_df <- data.frame()

for(i in 1:nreports) {
  reportJSON <- fromJSON(route_report[i,1])
  
  eMARS_checkbox_df[i,"acdesc_release_majorocc_ground"]=reportJSON$Data$AccidentDescription_Tab$ADReleaseMajors$IsChecked[1]
  eMARS_checkbox_df[i,"acdesc_release_majorocc_water"]=reportJSON$Data$AccidentDescription_Tab$ADReleaseMajors$IsChecked[2]
  eMARS_checkbox_df[i,"acdesc_release_majorocc_fluidair"]=reportJSON$Data$AccidentDescription_Tab$ADReleaseMajors$IsChecked[3]
  eMARS_checkbox_df[i,"acdesc_release_majorocc_solidair"]=reportJSON$Data$AccidentDescription_Tab$ADReleaseMajors$IsChecked[4]
  eMARS_checkbox_df[i,"acdesc_release_majorocc_solidground"]=reportJSON$Data$AccidentDescription_Tab$ADReleaseMajors$IsChecked[5]
  eMARS_checkbox_df[i,"acdesc_release_majorocc_solidwater"]=reportJSON$Data$AccidentDescription_Tab$ADReleaseMajors$IsChecked[6]
  eMARS_checkbox_df[i,"acdesc_release_majorocc_notknown"]=reportJSON$Data$AccidentDescription_Tab$ADReleaseMajors$IsChecked[7]
  
  eMARS_checkbox_df[i,"acdesc_fire_majorocc_conflagration"]=reportJSON$Data$AccidentDescription_Tab$ADFireMajors$IsChecked[1]
  eMARS_checkbox_df[i,"acdesc_fire_majorocc_poolfire"]=reportJSON$Data$AccidentDescription_Tab$ADFireMajors$IsChecked[2]
  eMARS_checkbox_df[i,"acdesc_fire_majorocc_jetflame"]=reportJSON$Data$AccidentDescription_Tab$ADFireMajors$IsChecked[3]
  eMARS_checkbox_df[i,"acdesc_fire_majorocc_flashfire"]=reportJSON$Data$AccidentDescription_Tab$ADFireMajors$IsChecked[4]
  eMARS_checkbox_df[i,"acdesc_fire_majorocc_fireball"]=reportJSON$Data$AccidentDescription_Tab$ADFireMajors$IsChecked[5]
  eMARS_checkbox_df[i,"acdesc_fire_majorocc_notknown"]=reportJSON$Data$AccidentDescription_Tab$ADFireMajors$IsChecked[6]
  
  eMARS_checkbox_df[i,"acdesc_explosion_majorocc_BLEVE"]=reportJSON$Data$AccidentDescription_Tab$ADExplosionMajors$IsChecked[1]
  eMARS_checkbox_df[i,"acdesc_explosion_majorocc_dustexp"]=reportJSON$Data$AccidentDescription_Tab$ADExplosionMajors$IsChecked[2]
  eMARS_checkbox_df[i,"acdesc_explosion_majorocc_expdesc"]=reportJSON$Data$AccidentDescription_Tab$ADExplosionMajors$IsChecked[3]
  eMARS_checkbox_df[i,"acdesc_explosion_majorocc_pressburst"]=reportJSON$Data$AccidentDescription_Tab$ADExplosionMajors$IsChecked[4]
  eMARS_checkbox_df[i,"acdesc_explosion_majorocc_phasetransexp"]=reportJSON$Data$AccidentDescription_Tab$ADExplosionMajors$IsChecked[5]
  eMARS_checkbox_df[i,"acdesc_explosion_majorocc_runawayexp"]=reportJSON$Data$AccidentDescription_Tab$ADExplosionMajors$IsChecked[6]
  eMARS_checkbox_df[i,"acdesc_explosion_majorocc_VCE"]=reportJSON$Data$AccidentDescription_Tab$ADExplosionMajors$IsChecked[7]
  eMARS_checkbox_df[i,"acdesc_explosion_majorocc_notknown"]=reportJSON$Data$AccidentDescription_Tab$ADExplosionMajors$IsChecked[8]
  
  eMARS_checkbox_df[i,"acdesc_transport_majorocc_air"]=reportJSON$Data$AccidentDescription_Tab$ADTransportMajors$IsChecked[1]
  eMARS_checkbox_df[i,"acdesc_transport_majorocc_rail"]=reportJSON$Data$AccidentDescription_Tab$ADTransportMajors$IsChecked[2]
  eMARS_checkbox_df[i,"acdesc_transport_majorocc_road"]=reportJSON$Data$AccidentDescription_Tab$ADTransportMajors$IsChecked[3]
  eMARS_checkbox_df[i,"acdesc_transport_majorocc_water"]=reportJSON$Data$AccidentDescription_Tab$ADTransportMajors$IsChecked[4]
  
  
  
  eMARS_checkbox_df[i,"causes_planteq_vessel"]=reportJSON$Data$Causes_Tab$CausesPlantEquipments$IsChecked[1]
  eMARS_checkbox_df[i,"causes_planteq_machinery"]=reportJSON$Data$Causes_Tab$CausesPlantEquipments$IsChecked[2]
  eMARS_checkbox_df[i,"causes_planteq_losscontrol"]=reportJSON$Data$Causes_Tab$CausesPlantEquipments$IsChecked[3]
  eMARS_checkbox_df[i,"causes_planteq_corrosion"]=reportJSON$Data$Causes_Tab$CausesPlantEquipments$IsChecked[4]
  eMARS_checkbox_df[i,"causes_planteq_instrument"]=reportJSON$Data$Causes_Tab$CausesPlantEquipments$IsChecked[5]
  eMARS_checkbox_df[i,"causes_planteq_runaway"]=reportJSON$Data$Causes_Tab$CausesPlantEquipments$IsChecked[6]
  eMARS_checkbox_df[i,"causes_planteq_unexpected"]=reportJSON$Data$Causes_Tab$CausesPlantEquipments$IsChecked[7]
  eMARS_checkbox_df[i,"causes_planteq_blockage"]=reportJSON$Data$Causes_Tab$CausesPlantEquipments$IsChecked[8]
  eMARS_checkbox_df[i,"causes_planteq_electrostatic"]=reportJSON$Data$Causes_Tab$CausesPlantEquipments$IsChecked[9]
  eMARS_checkbox_df[i,"causes_planteq_other"]=reportJSON$Data$Causes_Tab$CausesPlantEquipments$IsChecked[10]
  eMARS_checkbox_df[i,"causes_planteq_notknown"]=reportJSON$Data$Causes_Tab$CausesPlantEquipments$IsChecked[11]
  eMARS_checkbox_df[i,"causes_planteq_notidentified"]=reportJSON$Data$Causes_Tab$CausesPlantEquipments$IsChecked[12]
  
  eMARS_checkbox_df[i,"causes_human_operror"]=reportJSON$Data$Causes_Tab$CausesHumans$IsChecked[1]
  eMARS_checkbox_df[i,"causes_human_ophealth"]=reportJSON$Data$Causes_Tab$CausesHumans$IsChecked[2]
  eMARS_checkbox_df[i,"causes_human_disobedience"]=reportJSON$Data$Causes_Tab$CausesHumans$IsChecked[3]
  eMARS_checkbox_df[i,"causes_human_malicious"]=reportJSON$Data$Causes_Tab$CausesHumans$IsChecked[4]
  eMARS_checkbox_df[i,"causes_human_other"]=reportJSON$Data$Causes_Tab$CausesHumans$IsChecked[5]
  eMARS_checkbox_df[i,"causes_human_notknown"]=reportJSON$Data$Causes_Tab$CausesHumans$IsChecked[6]
  eMARS_checkbox_df[i,"causes_human_notidentified"]=reportJSON$Data$Causes_Tab$CausesHumans$IsChecked[7]
  
  eMARS_checkbox_df[i,"causes_org_mngorganization"]=reportJSON$Data$Causes_Tab$CausesOrganisationals$IsChecked[1]
  eMARS_checkbox_df[i,"causes_org_mngattitude"]=reportJSON$Data$Causes_Tab$CausesOrganisationals$IsChecked[2]
  eMARS_checkbox_df[i,"causes_org_procedures"]=reportJSON$Data$Causes_Tab$CausesOrganisationals$IsChecked[3]
  eMARS_checkbox_df[i,"causes_org_training"]=reportJSON$Data$Causes_Tab$CausesOrganisationals$IsChecked[4]
  eMARS_checkbox_df[i,"causes_org_supervision"]=reportJSON$Data$Causes_Tab$CausesOrganisationals$IsChecked[5]
  eMARS_checkbox_df[i,"causes_org_staffing"]=reportJSON$Data$Causes_Tab$CausesOrganisationals$IsChecked[6]
  eMARS_checkbox_df[i,"causes_org_pranalysis"]=reportJSON$Data$Causes_Tab$CausesOrganisationals$IsChecked[7]
  eMARS_checkbox_df[i,"causes_org_design"]=reportJSON$Data$Causes_Tab$CausesOrganisationals$IsChecked[8]
  eMARS_checkbox_df[i,"causes_org_userunfriend"]=reportJSON$Data$Causes_Tab$CausesOrganisationals$IsChecked[9]
  eMARS_checkbox_df[i,"causes_org_manufacture"]=reportJSON$Data$Causes_Tab$CausesOrganisationals$IsChecked[10]
  eMARS_checkbox_df[i,"causes_org_installation"]=reportJSON$Data$Causes_Tab$CausesOrganisationals$IsChecked[11]
  eMARS_checkbox_df[i,"causes_org_isolation"]=reportJSON$Data$Causes_Tab$CausesOrganisationals$IsChecked[12]
  eMARS_checkbox_df[i,"causes_org_manteinance"]=reportJSON$Data$Causes_Tab$CausesOrganisationals$IsChecked[13]
  eMARS_checkbox_df[i,"causes_org_testing"]=reportJSON$Data$Causes_Tab$CausesOrganisationals$IsChecked[14]
  eMARS_checkbox_df[i,"causes_org_other"]=reportJSON$Data$Causes_Tab$CausesOrganisationals$IsChecked[15]
  eMARS_checkbox_df[i,"causes_org_notknown"]=reportJSON$Data$Causes_Tab$CausesOrganisationals$IsChecked[16]
  eMARS_checkbox_df[i,"causes_org_notidentified"]=reportJSON$Data$Causes_Tab$CausesOrganisationals$IsChecked[17]
  
  eMARS_checkbox_df[i,"causes_external_naturalev"]=reportJSON$Data$Causes_Tab$CausesExternals$IsChecked[1]
  eMARS_checkbox_df[i,"causes_external_dominoef"]=reportJSON$Data$Causes_Tab$CausesExternals$IsChecked[2]
  eMARS_checkbox_df[i,"causes_external_transportac"]=reportJSON$Data$Causes_Tab$CausesExternals$IsChecked[3]
  eMARS_checkbox_df[i,"causes_external_struckobj"]=reportJSON$Data$Causes_Tab$CausesExternals$IsChecked[4]
  eMARS_checkbox_df[i,"causes_external_utilitiesf"]=reportJSON$Data$Causes_Tab$CausesExternals$IsChecked[5]
  eMARS_checkbox_df[i,"causes_external_securitydef"]=reportJSON$Data$Causes_Tab$CausesExternals$IsChecked[6]
  eMARS_checkbox_df[i,"causes_external_other"]=reportJSON$Data$Causes_Tab$CausesExternals$IsChecked[7]
  eMARS_checkbox_df[i,"causes_external_notknown"]=reportJSON$Data$Causes_Tab$CausesExternals$IsChecked[8]
  eMARS_checkbox_df[i,"causes_external_notidentified"]=reportJSON$Data$Causes_Tab$CausesExternals$IsChecked[9]
}


    ### ...accident description ####

acdesc_release <- vector(length = nreports)
acdesc_fire <- vector(length = nreports)
acdesc_explosion <- vector(length = nreports)
acdesc_transport <- vector(length = nreports)


#for (i in 1:nreports) {
#acdesc_release[i] <- sum(eMARS_checkbox_df[i,1:7] == TRUE)
#acdesc_fire[i] <- sum(eMARS_checkbox_df[i,8:13] == TRUE)
#acdesc_explosion[i] <- sum(eMARS_checkbox_df[i,14:21] == TRUE)
#acdesc_transport[i] <- sum(eMARS_checkbox_df[i,22:25] == TRUE)
#}

for (i in 1:nreports) {
  acdesc_release[i] <- sum(eMARS_checkbox_df[i,1:6] == TRUE)
  acdesc_fire[i] <- sum(eMARS_checkbox_df[i,8:12] == TRUE)
  acdesc_explosion[i] <- sum(eMARS_checkbox_df[i,14:20] == TRUE)
  acdesc_transport[i] <- sum(eMARS_checkbox_df[i,22:25] == TRUE)
}

acdesc_release_rep <- acdesc_release %>% `>=`(1) %>% which()
acdesc_fire_rep <- acdesc_fire %>% `>=`(1) %>% which()
acdesc_explosion_rep <- acdesc_explosion %>% `>=`(1) %>% which()
acdesc_transport_rep <- acdesc_transport %>% `>=`(1) %>% which()

list_acdesc <- list(Release = acdesc_release_rep, Fire = acdesc_fire_rep, Explosion = acdesc_explosion_rep, Transport = acdesc_transport_rep)

upset(fromList(list_acdesc), point.size = 3, line.size = 1,
      text.scale = c(1.5, 1.5, 1.5, 1.5, 2, 2.5), order.by = "freq") # SAVE 7.5 in x 10 in

length(unique(c(acdesc_release_rep, acdesc_fire_rep, acdesc_explosion_rep, acdesc_transport_rep)))

acdesc_total <- vector(length = nreports)
for (i in 1:nreports) {
  acdesc_total[i] <- sum(eMARS_checkbox_df[i,1:25] == TRUE)
}

acdesc_total_rep <- acdesc_total %>% `>=`(1) %>% which()
length(acdesc_total_rep)

    ### ... release ####

release_check <- vector(length = nreports)
release_liquid <- vector(length = nreports)
release_solid <- vector(length = nreports)
release_gas <- vector(length = nreports)
release_not <- vector(length = nreports)


for (i in 1:nreports) {
  release_liquid[i] <- sum(eMARS_checkbox_df[i,1:2] == TRUE)
  release_solid[i] <- sum(eMARS_checkbox_df[i,4:6] == TRUE)
  release_gas[i] <- sum(eMARS_checkbox_df[i,3] == TRUE)
  release_not[i] <- sum(eMARS_checkbox_df[i,7] == TRUE)
}


liquid_rep <- release_liquid %>% `>=`(1) %>% which()
solid_rep <- release_solid %>% `>=`(1) %>% which()
gas_rep <- release_gas %>% `>=`(1) %>% which()
not_rep <- release_not %>% `>=`(1) %>% which()



list_release_state <- list(Liquid = liquid_rep, Solid = solid_rep, Gas = gas_rep)

upset(fromList(list_release_state), point.size = 3, line.size = 1,
      text.scale = c(1.5, 1.5, 1.5, 1.5, 2, 2.5), order.by = "freq")



    ### ...causes ####

causes_planteq <- vector(length = nreports)
causes_human <- vector(length = nreports)
causes_org <- vector(length = nreports)
causes_external <- vector(length = nreports)

#for (i in 1:nreports) {
  #causes_planteq[i] <- sum(eMARS_checkbox_df[i,26:37] == TRUE)
#causes_human[i] <- sum(eMARS_checkbox_df[i,38:44] == TRUE)
#causes_org[i] <- sum(eMARS_checkbox_df[i,45:61] == TRUE)
#causes_external[i] <- sum(eMARS_checkbox_df[i,62:70] == TRUE)
#}

for (i in 1:nreports) {
  causes_planteq[i] <- sum(eMARS_checkbox_df[i,26:35] == TRUE)
  causes_human[i] <- sum(eMARS_checkbox_df[i,38:42] == TRUE)
  causes_org[i] <- sum(eMARS_checkbox_df[i,45:59] == TRUE)
  causes_external[i] <- sum(eMARS_checkbox_df[i,62:68] == TRUE)
}

causes_planteq_rep <- causes_planteq %>% `>=`(1) %>% which()
causes_human_rep <- causes_human %>% `>=`(1) %>% which()
causes_org_rep <- causes_org %>% `>=`(1) %>% which()
causes_external_rep <- causes_external %>% `>=`(1) %>% which()

list_causes <- list(Plant_Equipment = causes_planteq_rep, Human = causes_human_rep, Organizational = causes_org_rep, External = causes_external_rep)

upset(fromList(list_causes), point.size = 3, line.size = 1,
      text.scale = c(1.5, 1.5, 1.5, 1.5, 2, 2.5), order.by = "freq")

causes_intersection_planteq_org_rep <- intersect(causes_planteq_rep, causes_org_rep)
causes_intersection_org_human_rep <- intersect(causes_org_rep, causes_human_rep)
causes_intersection_planteq_org_humna_rep <- intersect(causes_intersection_planteq_org_rep, causes_human_rep)

length(unique(c(causes_planteq_rep, causes_human_rep, causes_org_rep, causes_external_rep)))


    ### ...word cloud ####
    
    for (i in 1:1076){
      POSTagging_lemma_udpipe_df$Tot_causes[i] <- list(c(unlist(POSTagging_lemma_udpipe_df$Noun_causes[i]),
                                                                unlist(POSTagging_lemma_udpipe_df$Verb_causes[i]),
                                                                unlist(POSTagging_lemma_udpipe_df$Adjective_causes[i])))
    }

    wc_causes_org <- POSTagging_lemma_udpipe_df[causes_org_rep,"Tot_causes"]
    tokens_wc_causes_org <- tokens(wc_causes_org, remove_punct = TRUE, remove_symbols = TRUE, remove_numbers = TRUE)
    tokens_wc_causes_org <- tokens_select(tokens_wc_causes_org, pattern = stopwords("en"), selection = "remove")
    dfm_wc_causes_org <- dfm(tokens_wc_causes_org, tolower = TRUE)
    dfm_wc_causes_org <- dfm_trim(dfm_wc_causes_org, max_docfreq = 350, docfreq_type = "count")
    dfm_wc_causes_org <- dfm_select(dfm_wc_causes_org, pattern =  c("accident", "seveso", "cause", "occur", "mhp", "valf"), selection = "remove")
    dfm_wc_causes_org <- dfm_keep(dfm_wc_causes_org, min_nchar = 3)
    textplot_wordcloud(dfm_wc_causes_org, max_words = 100)
    
    
    
    
    wc_causes_intersection_planteq_org_rep <- POSTagging_lemma_udpipe_df[causes_intersection_planteq_org_rep,"Tot_causes"]
    tokens_wc_causes_intersection_planteq_org_rep <- tokens(wc_causes_intersection_planteq_org_rep, remove_punct = TRUE, remove_symbols = TRUE, remove_numbers = TRUE)
    tokens_wc_causes_intersection_planteq_org_rep <- tokens_select(tokens_wc_causes_intersection_planteq_org_rep, pattern = stopwords("en"), selection = "remove")
    dfm_wc_causes_intersection_planteq_org_rep <- dfm(tokens_wc_causes_intersection_planteq_org_rep, tolower = TRUE)
    dfm_wc_causes_intersection_planteq_org_rep <- dfm_keep(dfm_wc_causes_intersection_planteq_org_rep, min_nchar = 3)
    dfm_wc_causes_intersection_planteq_org_rep <- dfm_trim(dfm_wc_causes_intersection_planteq_org_rep, max_docfreq = 180, docfreq_type = "count")
    textplot_wordcloud(dfm_wc_causes_intersection_planteq_org_rep, max_words = 100)
    
    wc_causes_intersection_org_human_rep <- POSTagging_lemma_udpipe_df[causes_intersection_org_human_rep,"Tot_causes"]
    tokens_wc_causes_intersection_org_human_rep <- tokens(wc_causes_intersection_org_human_rep, remove_punct = TRUE, remove_symbols = TRUE, remove_numbers = TRUE)
    tokens_wc_causes_intersection_org_human_rep <- tokens_select(tokens_wc_causes_intersection_org_human_rep, pattern = stopwords("en"), selection = "remove")
    dfm_wc_causes_intersection_org_human_rep <- dfm(tokens_wc_causes_intersection_org_human_rep, tolower = TRUE)
    dfm_wc_causes_intersection_org_human_rep <- dfm_keep(dfm_wc_causes_intersection_org_human_rep, min_nchar = 3)
    dfm_wc_causes_intersection_org_human_rep <- dfm_trim(dfm_wc_causes_intersection_org_human_rep, max_docfreq = 100, docfreq_type = "count")
    textplot_wordcloud(dfm_wc_causes_intersection_org_human_rep, max_words = 100)
    
    wc_causes_intersection_planteq_org_humna_rep <- POSTagging_lemma_udpipe_df[causes_intersection_planteq_org_humna_rep,"Tot_causes"]
    tokens_wc_causes_intersection_planteq_org_humna_rep <- tokens(wc_causes_intersection_planteq_org_humna_rep, remove_punct = TRUE, remove_symbols = TRUE, remove_numbers = TRUE)
    tokens_wc_causes_intersection_planteq_org_humna_rep <- tokens_select(tokens_wc_causes_intersection_planteq_org_humna_rep, pattern = stopwords("en"), selection = "remove")
    dfm_wc_causes_intersection_planteq_org_humna_rep <- dfm(tokens_wc_causes_intersection_planteq_org_humna_rep, tolower = TRUE)
    dfm_wc_causes_intersection_planteq_org_humna_rep <- dfm_keep(dfm_wc_causes_intersection_planteq_org_humna_rep, min_nchar = 3)
    dfm_wc_causes_intersection_planteq_org_humna_rep <- dfm_select(dfm_wc_causes_intersection_planteq_org_humna_rep, pattern =  c("accident", "seveso", "cause", "occur"), selection = "remove")
    textplot_wordcloud(dfm_wc_causes_intersection_planteq_org_humna_rep, max_words = 100)


#CAS RN ####


all_reports <- c(eMARS_df[,"AccidentDescription"],eMARS_df[,"SiteDescription"],eMARS_df[,"InstallationDescription"],
             eMARS_df[,"SubstanceInvolvedDescription"],eMARS_df[,"CausesDescription"],eMARS_df[,"ConsequenceDescription"],
             eMARS_df[,"EmergencyResponse"],eMARS_df[,"LessonsLearned"])

corpus_all <- corpus(all_reports)
tokens_CAS <- tokens(corpus_all, remove_punct = TRUE)
tokens_list <- as.list(tokens_CAS)
tokens_list <- sapply(tokens_list, paste, collapse = " ")
tokens_list <- strsplit(tokens_list, " ")

tokens_CASdf <- data.frame(Report = rep(1:nreports,8))

tokens_CASdf$Tokens <- tokens_list


for (i in 1:8608){
  words <- unlist(tokens_CASdf$Tokens[i])
  names(words) <- NULL
  words <- words[grepl("[0-9]+[-][0-9]{2}[-][0-9]",words)]
  words <- gsub("[A-z]","",words)
  words <- gsub("\\s*","", words)
  words <- words[grepl("^[0-9]+[-][0-9]{2}[-][0-9]$",words)]
  words <- str_remove(words, "^0+")
  tokens_CASdf$CAS[i] <- list(words)
}



CAS_reportsdf <- data.frame("AccidentID" = eMARS_df$AccidentID, "CAS_text" = vector(length=nreports),"CAS_substances" = vector(length=nreports),"CAS_total" = vector(length=nreports))

for (i in 1:nreports){
  CAS_reportsdf$CAS_text[i] <- list(c(unlist(tokens_CASdf$CAS[i]), unlist(tokens_CASdf$CAS[nreports+i]), unlist(tokens_CASdf$CAS[nreports*2+i]),
                                 unlist(tokens_CASdf$CAS[nreports*3+i]), unlist(tokens_CASdf$CAS[nreports*4+i]),unlist(tokens_CASdf$CAS[nreports*5+i]),
                                 unlist(tokens_CASdf$CAS[nreports*6+i]), unlist(tokens_CASdf$CAS[nreports*7+i])))
}

for (i in 1:nreports){
  reportJSON <- fromJSON(route_report[i,1])
  words <- unlist(reportJSON$Data$SubstancesInvolved_Tab$SubstancesInvolved$CASNumber)
  words <- words[grepl("[0-9]+[-][0-9]{2}[-][0-9]", words)]
  words <- gsub("[A-z]","", words)
  words <- gsub("\\s*","", words)
  words <- words[grepl("^[0-9]+[-][0-9]{2}[-][0-9]$", words)]
  words <- str_remove(words, "^0+")
  CAS_reportsdf$CAS_substances[i] <- list(words)
}

for (i in 1:nreports){
  CAS_reportsdf$CAS_total[i] <- list(unique(c(unlist(CAS_reportsdf$CAS_text[i]),unlist(CAS_reportsdf$CAS_substances[i]))))
}


CAS_reference <- read_excel("C:/Users/polja/Desktop/IQS/MEQ/TFM/CAS_Registry_Number.xlsx")   # https://epa.figshare.com/articles/dataset/DSSTox_Identifiers_Mapped_to_CAS_Numbers_and_Names_File_11_14_2016/5588566
colnames(CAS_reference) <- c("CAS RN", "Name")

CAS_RN_df <- as.data.frame(table(unlist(CAS_reportsdf$CAS_total)))
CAS_RN_df[,3] <- NA
colnames(CAS_RN_df) <- c("CAS_RN", "Frequency", "Substance")

CAS_mergedf <- merge(CAS_RN_df$CAS_RN, CAS_reference, by = 1, all.x = TRUE, all.y = FALSE)
CAS_RN_df$Substance <- CAS_mergedf$Name

length(unique(unlist(CAS_reportsdf$CAS_total)))


sum(CAS_reportsdf$CAS_total == "character(0)")


sum(is.na(CAS_RN_df$Substance))

write_xlsx(CAS_RN_df, "C:\\Users\\polja\\Desktop\\IQS\\MEQ\\TFM\\tablas\\CAS_RN_Ralph.xlsx")


# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #

# CAS Meeting 
CAS_2moresub <- vector()

for (i in 1:nreports){
  if (length(unlist(CAS_reportsdf$CAS_total[i])) >= 2){
    CAS_2moresub <- c(CAS_2moresub, i)
  }
}
CASRN <- vector(length = length(CAS_2moresub))
meeting_CAS_df <- data.frame(AccidentID = eMARS_df$AccidentID[CAS_2moresub], Link = vector(length = length(CAS_2moresub)),
                             CAS_RN = vector(length = length(CAS_2moresub)), Substances = vector(length = length(CAS_2moresub)),
                             Accident_Description = eMARS_df$AccidentDescription[CAS_2moresub], 
                             Substances_involved_description = eMARS_df$SubstanceInvolvedDescription[CAS_2moresub])
                             
for (i in 1:length(CAS_2moresub)){
  meeting_CAS_df$Link[i] <- paste("https://emars.jrc.ec.europa.eu/en/emars/accident/view/",meeting_CAS_df$AccidentID[i], sep="")
  
  CASRN[i] <- list(unlist(CAS_reportsdf$CAS_total[CAS_2moresub[i]]))
  
  sub_name <- merge(CASRN[i], CAS_reference, by = 1, all.x = TRUE, all.y = FALSE)
  colnames(sub_name) <- c("CAS","Name")
  meeting_CAS_df$CAS_RN[i] <- unlist(paste(sub_name$CAS, collapse = "; "))
  meeting_CAS_df$Substances[i] <- unlist(paste(sub_name$Name, collapse = "; "))
}

write_xlsx(meeting_CAS_df, "C:\\Users\\polja\\Desktop\\IQS\\MEQ\\TFM\\tablas\\CAS_Meeting\\reports_CAS_RN_2.xlsx")




# CAS Meeting - Reactor/Reaction

CAS_reactor_2moresub <- vector()

for (i in 1:nreports){
  if (((grepl("reactor", POSTagging_lemma_udpipe_df$Tot_acDesc_causes[i]))|(grepl("reaction", POSTagging_lemma_udpipe_df$Tot_acDesc_causes[i]))) & (length(unlist(CAS_reportsdf$CAS_total[i])) >= 2)){
    CAS_reactor_2moresub <- c(CAS_reactor_2moresub, i)
  }
}

CASRN <- vector(length = length(CAS_reactor_2moresub))
meeting_CAS_reactor_df <- data.frame(AccidentID = eMARS_df$AccidentID[CAS_reactor_2moresub], Link = vector(length = length(CAS_reactor_2moresub)),
                             CAS_RN = vector(length = length(CAS_reactor_2moresub)), Substances = vector(length = length(CAS_reactor_2moresub)),
                             Accident_Description = eMARS_df$AccidentDescription[CAS_reactor_2moresub], 
                             Substances_involved_description = eMARS_df$SubstanceInvolvedDescription[CAS_reactor_2moresub])

for (i in 1:length(CAS_reactor_2moresub)){
  meeting_CAS_reactor_df$Link[i] <- paste("https://emars.jrc.ec.europa.eu/en/emars/accident/view/", meeting_CAS_reactor_df$AccidentID[i], sep="")
  
  CASRN[i] <- list(unlist(CAS_reportsdf$CAS_total[CAS_reactor_2moresub[i]]))
  
  sub_name <- merge(CASRN[i], CAS_reference, by = 1, all.x = TRUE, all.y = FALSE)
  colnames(sub_name) <- c("CAS","Name")
  meeting_CAS_reactor_df$CAS_RN[i] <- unlist(paste(sub_name$CAS, collapse = "; "))
  meeting_CAS_reactor_df$Substances[i] <- unlist(paste(sub_name$Name, collapse = "; "))
}


write_xlsx(meeting_CAS_reactor_df, "C:\\Users\\polja\\Desktop\\IQS\\MEQ\\TFM\\tablas\\CAS_Meeting\\reports_reactor_CAS_RN_2.xlsx")

# # # # # # # # # ## # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 



#POS TAGGING ####
    ### ... udpipe ####

ud_model <- udpipe_download_model(language = "english")
ud_model <- udpipe_load_model(ud_model$file_model)

POSTagging_udpipe_df <- data.frame("AccidentID" = vector(length=nreports), "Title" = vector(length=nreports), 
                            "Noun_acDesc" = vector(length=nreports), "Verb_acDesc" = vector(length=nreports),
                            "Adjective_acDesc" = vector(length=nreports), "Noun_causes" = vector(length=nreports),
                            "Verb_causes" = vector(length=nreports), "Adjective_causes" = vector(length=nreports))

POSTagging_udpipe_df$AccidentID <- eMARS_df$AccidentID
POSTagging_udpipe_df$Title <- eMARS_df$Title




  #tokens

for (i in 1:nreports){
  x <- udpipe_annotate(ud_model, x = eMARS_df$AccidentDescription[i], doc_id = eMARS_df$AccidentID[i])
  POS_acDesc <- as.data.frame(x) 
  POSTagging_udpipe_df$Noun_acDesc[i] <- list(POS_acDesc$token[POS_acDesc$upos=="NOUN"])
  POSTagging_udpipe_df$Verb_acDesc[i] <- list(POS_acDesc$token[POS_acDesc$upos=="VERB"])
  POSTagging_udpipe_df$Adjective_acDesc[i] <- list(POS_acDesc$token[POS_acDesc$upos=="ADJ"])
}
  
for (i in 1:nreports){
  y <- udpipe_annotate(ud_model, x = eMARS_df$CausesDescription[i], doc_id = eMARS_df$AccidentID[i])
  POS_causes <- as.data.frame(y) 
  
  POSTagging_udpipe_df$Noun_causes[i] <- list(POS_causes$token[POS_causes$upos=="NOUN"])
  POSTagging_udpipe_df$Verb_causes[i] <- list(POS_causes$token[POS_causes$upos=="VERB"])
  POSTagging_udpipe_df$Adjective_causes[i] <- list(POS_causes$token[POS_causes$upos=="ADJ"])
}

  #lemma

ud_model <- udpipe_download_model(language = "english")
ud_model <- udpipe_load_model(ud_model$file_model)

POSTagging_lemma_udpipe_df <- data.frame("AccidentID" = vector(length=nreports), "Title" = vector(length=nreports), 
                                   "Noun_acDesc" = vector(length=nreports), "Verb_acDesc" = vector(length=nreports),
                                   "Adjective_acDesc" = vector(length=nreports), "Noun_causes" = vector(length=nreports),
                                   "Verb_causes" = vector(length=nreports), "Adjective_causes" = vector(length=nreports))

POSTagging_lemma_udpipe_df$AccidentID <- eMARS_df$AccidentID
POSTagging_lemma_udpipe_df$Title <- eMARS_df$Title

for (i in 1:nreports){
  x <- udpipe_annotate(ud_model, x = eMARS_firebigram_df$AccidentDescription[i], doc_id = eMARS_firebigram_df$AccidentID[i])
  POS_acDesc <- as.data.frame(x) 
  POSTagging_lemma_udpipe_df$Noun_acDesc[i] <- list(POS_acDesc$lemma[POS_acDesc$upos=="NOUN"])
  POSTagging_lemma_udpipe_df$Verb_acDesc[i] <- list(POS_acDesc$lemma[POS_acDesc$upos=="VERB"])
  POSTagging_lemma_udpipe_df$Adjective_acDesc[i] <- list(POS_acDesc$lemma[POS_acDesc$upos=="ADJ"])
}

for (i in 1:nreports){
  y <- udpipe_annotate(ud_model, x = eMARS_firebigram_df$CausesDescription[i], doc_id = eMARS_firebigram_df$AccidentID[i])
  POS_causes <- as.data.frame(y) 
  
  POSTagging_lemma_udpipe_df$Noun_causes[i] <- list(POS_causes$lemma[POS_causes$upos=="NOUN"])
  POSTagging_lemma_udpipe_df$Verb_causes[i] <- list(POS_causes$lemma[POS_causes$upos=="VERB"])
  POSTagging_lemma_udpipe_df$Adjective_causes[i] <- list(POS_causes$lemma[POS_causes$upos=="ADJ"])
}
x <- udpipe_annotate(ud_model, x = eMARS_firebigram_df$AccidentDescription[6], doc_id = eMARS_df$AccidentID[6])
POS_acDesc <- as.data.frame(x) 


    ### ... pos phrase sequence ####
    
    text_sequence <- vector(length = nreports)
    
    for (i in 1:nreports){
      
      text_sequence[i] <- paste(eMARS_df$AccidentDescription[i],eMARS_df$CausesDescription[i], sep = " ")
      
    }
    
    z <- udpipe_annotate(ud_model, x = text_sequence, doc_id = eMARS_df$AccidentID)
    POS_phrase <- as.data.frame(z) 
    POS_phrase$phrase_tag <- as_phrasemachine(POS_phrase$upos, type = "upos")
    
    stats <- keywords_phrases(x = POS_phrase$phrase_tag, term = tolower(POS_phrase$token), 
                              pattern = "(A|N)*N(P+D*(A|N)*N)*", is_regex = TRUE, detailed = FALSE)
    
    ### ... fire bigram ####
    
    stats_keywords <- stats[stats$ngram == 2,]
    
    bigram_fire <- stats_keywords[grep("fire", stats_keywords$keyword),]
    
    sum(bigram_fire$freq[bigram_fire$freq >= 2])
    sum(bigram_fire$freq)
    
    length(bigram_fire$freq >= 2)
    
    write_xlsx(bigram_fire, "C:\\Users\\polja\\Desktop\\IQS\\MEQ\\TFM\\emarsTM\\fire_bigram.xlsx")
    
    
    fire_stopwords <- c("fire brigade", "fire service", "fire fighting", "fire alarm", "fire brigades", "fire fighters",
                        "fire fighter", "fire services", "fire department", "fire engines", "fire protection", "fire team",
                        "fire extinguishers", "automatic fire", "fire -fighters", "fire monitors", "fire detection",
                        "fire safety", "fire vehicles", "fire extinguisher", "fire detectors", "fire extinguishing")

    
    fire_stop_acdesc <- data.frame("AccidentID" = eMARS_df$AccidentID, "Accident_Description" = eMARS_df$AccidentDescription,
                                   "AcDesc_NO_fire_bigram" = vector(length = nreports), "AcDesc_Nouns" = vector(length = nreports),
                                   "AcDesc_Verbs" = vector(length = nreports), "AcDesc_Adjectives" = vector(length = nreports))
    
    for (i in 1:nreports){
      
      fire_stop_acdesc$AcDesc_NO_fire_bigram[i] <- fire_stop_acdesc$Accident_Description[i]
      
      for (j in 1:length(fire_stopwords)){
      fire_stop_acdesc$AcDesc_NO_fire_bigram[i] <- str_remove_all(fire_stop_acdesc$AcDesc_NO_fire_bigram[i], fire_stopwords[j])
      }
    }
    
    
    ud_model <- udpipe_download_model(language = "english")
    ud_model <- udpipe_load_model(ud_model$file_model)
    
    for (i in 1:nreports){
      f <- udpipe_annotate(ud_model, x = fire_stop_acdesc$AcDesc_NO_fire_bigram[i], doc_id = fire_stop_acdesc$AccidentID[i])
      POS_acDesc_fire_bigram <- as.data.frame(f) 
      fire_stop_acdesc$AcDesc_Nouns[i] <- list(POS_acDesc_fire_bigram$lemma[POS_acDesc_fire_bigram$upos=="NOUN"])
      fire_stop_acdesc$AcDesc_Verbs[i] <- list(POS_acDesc_fire_bigram$lemma[POS_acDesc_fire_bigram$upos=="VERB"])
      fire_stop_acdesc$AcDesc_Adjectives[i] <- list(POS_acDesc_fire_bigram$lemma[POS_acDesc_fire_bigram$upos=="ADJ"])
    }
    
    
    tokens_lemma_nouns_acDesc <- tokens(POSTagging_lemma_udpipe_df$Noun_acDesc, remove_punct = TRUE, remove_symbols = TRUE, remove_numbers = TRUE)
    tokens_lemma_nouns_acDesc <- tokens_select(tokens_lemma_nouns_acDesc, pattern = stopwords("en"), selection = "remove")
    dfm_lemma_nouns_acDesc <- dfm(tokens_lemma_nouns_acDesc, tolower = TRUE)
    dfm_lemma_nouns_acDesc <- dfm_keep(dfm_lemma_nouns_acDesc, min_nchar = 3)
    textplot_wordcloud(dfm_lemma_nouns_acDesc, max_words = 100)
    
    tokens_lemma_nouns_acDesc_fire_bigram <- tokens(fire_stop_acdesc$AcDesc_Nouns, remove_punct = TRUE, remove_symbols = TRUE, remove_numbers = TRUE)
    tokens_lemma_nouns_acDesc_fire_bigram <- tokens_select(tokens_lemma_nouns_acDesc_fire_bigram, pattern = stopwords("en"), selection = "remove")
    dfm_lemma_nouns_acDesc_fire_bigram <- dfm(tokens_lemma_nouns_acDesc_fire_bigram, tolower = TRUE)
    dfm_lemma_nouns_acDesc_fire_bigram <- dfm_keep(dfm_lemma_nouns_acDesc_fire_bigram, min_nchar = 3)
    textplot_wordcloud(dfm_lemma_nouns_acDesc_fire_bigram, max_words = 100)
    
   
    
    
#TOKENS QUANTEDA####
    
    #Nouns
    
    tokens_lemma_nouns_acDesc <- tokens(POSTagging_lemma_udpipe_df$Noun_acDesc, remove_punct = TRUE, remove_symbols = TRUE, remove_numbers = TRUE)
    tokens_lemma_nouns_acDesc <- tokens_select(tokens_lemma_nouns_acDesc, pattern = stopwords("en"), selection = "remove")
    dfm_lemma_nouns_acDesc <- dfm(tokens_lemma_nouns_acDesc, tolower = TRUE)
    #dfm_lemma_nouns_acDesc <- dfm_select(dfm_lemma_nouns_acDesc, pattern =  c("accident", "seveso", "cause", "occur"), selection = "remove")
    dfm_lemma_nouns_acDesc <- dfm_trim(dfm_lemma_nouns_acDesc, max_docfreq = 800, docfreq_type = "count")
    dfm_lemma_nouns_acDesc <- dfm_keep(dfm_lemma_nouns_acDesc, min_nchar = 3)
    textplot_wordcloud(dfm_lemma_nouns_acDesc, max_words = 100)
    
    tokens_lemma_nouns_causes <- tokens(POSTagging_lemma_udpipe_df$Noun_causes, remove_punct = TRUE, remove_symbols = TRUE, remove_numbers = TRUE)
    tokens_lemma_nouns_causes <- tokens_select(tokens_lemma_nouns_causes, pattern = stopwords("en"), selection = "remove")
    dfm_lemma_nouns_causes <- dfm(tokens_lemma_nouns_causes, tolower = TRUE)
    dfm_lemma_nouns_causes <- dfm_trim(dfm_lemma_nouns_causes, max_docfreq = 800, docfreq_type = "count")
    #dfm_lemma_nouns_causes <- dfm_select(dfm_lemma_nouns_causes, pattern =  c("accident", "seveso", "cause", "occur"), selection = "remove")
    dfm_lemma_nouns_causes <- dfm_keep(dfm_lemma_nouns_causes, min_nchar = 3)
    textplot_wordcloud(dfm_lemma_nouns_causes, max_words = 100)
    
    
    #Total 
    
    for (i in 1:1076){
      POSTagging_lemma_udpipe_df$Tot_acDesc[i] <- list(c(unlist(POSTagging_lemma_udpipe_df$Noun_acDesc[i]),
                                                         unlist(POSTagging_lemma_udpipe_df$Verb_acDesc[i]),
                                                         unlist(POSTagging_lemma_udpipe_df$Adjective_acDesc[i])))
    }
    
    
    tokens_lemma_tot_acDesc <- tokens(POSTagging_lemma_udpipe_df$Tot_acDesc, remove_punct = TRUE, remove_symbols = TRUE, remove_numbers = TRUE)
    tokens_lemma_tot_acDesc <- tokens_select(tokens_lemma_tot_acDesc, pattern = stopwords("en"), selection = "remove")
    dfm_lemma_tot_acDesc <- dfm(tokens_lemma_tot_acDesc, tolower = TRUE)
    #dfm_lemma_tot_acDesc <- dfm_select(dfm_lemma_tot_acDesc, pattern =  c("accident", "seveso", "cause", "occur"), selection = "remove")
    dfm_lemma_tot_acDesc <- dfm_trim(dfm_lemma_tot_acDesc, max_docfreq = 800, docfreq_type = "count")
    dfm_lemma_tot_acDesc <- dfm_keep(dfm_lemma_tot_acDesc, min_nchar = 3)
    textplot_wordcloud(dfm_lemma_tot_acDesc, max_words = 100)
    
    tokens_lemma_tot_causes <- tokens(POSTagging_lemma_udpipe_df$Tot_causes, remove_punct = TRUE, remove_symbols = TRUE, remove_numbers = TRUE)
    tokens_lemma_tot_causes <- tokens_select(tokens_lemma_tot_causes, pattern = stopwords("en"), selection = "remove")
    dfm_lemma_tot_causes <- dfm(tokens_lemma_tot_causes, tolower = TRUE)
    dfm_lemma_tot_causes <- dfm_trim(dfm_lemma_tot_causes, max_docfreq = 600, docfreq_type = "count")
    #dfm_lemma_tot_causes <- dfm_select(dfm_lemma_tot_causes, pattern =  c("accident", "seveso", "cause", "occur"), selection = "remove")
    dfm_lemma_tot_causes <- dfm_keep(dfm_lemma_tot_causes, min_nchar = 3)
    textplot_wordcloud(dfm_lemma_tot_causes, max_words = 100)


    
#CLUSTERING ####
    ### ...optimization ####
        
        tokens_lemma_tot_acDesc_causes <- tokens(POSTagging_lemma_udpipe_df$Tot_acDesc_causes, remove_punct = TRUE, remove_symbols = TRUE, remove_numbers = TRUE)
        tokens_lemma_tot_acDesc_causes <- tokens_select(tokens_lemma_tot_acDesc_causes, pattern = stopwords("en"), selection = "remove")
        dfm_lemma_tot_acDesc_causes <- dfm(tokens_lemma_tot_acDesc_causes, tolower = TRUE)
        dfm_lemma_tot_acDesc_causes <- dfm_keep(dfm_lemma_tot_acDesc_causes, min_nchar = 3)
        dfm_lemma_tot_acDesc_causes <- dfm_trim(dfm_lemma_tot_acDesc_causes, max_docfreq = 200, docfreq_type = "count")
        textplot_wordcloud(dfm_lemma_tot_acDesc_causes, max_words = 100)
        
        
        rowsum_skmeans <- rowSums(dfm_lemma_tot_acDesc_causes)
        
        dfm_lemma_tot_acDesc_causes_skm <- dfm_tfidf(dfm_lemma_tot_acDesc_causes)
        
        
        
        parameter_optimum <- vector(mode = "logical", length = 29)
        for (i in 2:30){
          skmeans_opt <- skmeans(dfm_lemma_tot_acDesc_causes_skm, i, control = list(nruns = 50))
          parameter_optimum[i-1] <- skmeans_opt$value
        }
        

        
        plot(2:30, parameter_optimum[1:29],
             type="b", pch = 19, frame = FALSE, 
             xlab="Number of clusters K",
             ylab="Total within-clusters sum of squares")
        
        a <- 2
        a_value <- parameter_optimum[1]
        b <- 30
        b_value <- parameter_optimum[length(parameter_optimum)]
        
      
        values_regression_right <- data.frame()
       
        
        
        RMSE <- vector(length = (length(parameter_optimum)-2))
        
        for (i in 3:length(parameter_optimum)) {
          
          c <- i
          
          values_regression_left <- data.frame(x = a:c, y = parameter_optimum[1:(c-1)])
          
          regression_left <- lm(y ~ x, data=values_regression_left)
          
          
          RSS_left <- c(crossprod(regression_left$residuals))
          MSE_left <- RSS_left / length(regression_left$residuals)
          RMSE_left <- sqrt(MSE_left)
          
          
          values_regression_right <- data.frame(x = (c+1):b, y = parameter_optimum[c:(b-1)])
          
          regression_right <- lm(y ~ x, data=values_regression_right)
        
          
          RSS_right <- c(crossprod(regression_right$residuals))
          MSE_right <- RSS_right / length(regression_right$residuals)
          RMSE_right <- sqrt(MSE_right)
          
          
          RMSE[i-2] <- (c-1)/(b-1)*RMSE_left + (b-c)/(b-1)*RMSE_right
        }
        
        c_RMSE <- data.frame(c = 3:29, RMSE = RMSE)
        
      
        plot(c_RMSE$c, c_RMSE$RMSE,
            type="p", pch = 3, frame = FALSE, 
            xlab="Number of clusters", xlim= c(0,30),
            ylab="RMSE", ylim= c(2,8),
            cex.lab = 1.2, cex.axis = 1.1)
        
        #Plot clusters + lines
        
        c <- 10
        
        values_regression_left <- data.frame(x = a:c, y = parameter_optimum[1:(c-1)])
        regression_left <- lm(y ~ x, data=values_regression_left)
        
        
        xleft <- c(2,c+1)
        yleft <- c(xleft[1]*regression_left$coefficients[2]+regression_left$coefficients[1],
                   xleft[2]*regression_left$coefficients[2]+regression_left$coefficients[1])

        
        values_regression_right <- data.frame(x = (c+1):b, y = parameter_optimum[c:(b-1)])
        regression_right <- lm(y ~ x, data=values_regression_right)
      
        xright <- c(c-1,b)
        yright <- c(xright[1]*regression_right$coefficients[2]+regression_right$coefficients[1],
                    xright[2]*regression_right$coefficients[2]+regression_right$coefficients[1])
        
        plot(2:30, parameter_optimum[1:29],
             type="p", pch = 3, frame = FALSE, 
             xlab="Number of clusters", xlim= c(0,30),
             ylab="Minimized criterion",
             ylim= c(780,920),
             cex.lab = 1.2, cex.axis = 1.1)
        
        lines(xleft,yleft,col="red")
        lines(xright,yright,col="blue")
        
  
        #Wordclouds 
        
        
        
        skm_lemma_tot_acDesc_causes <- skmeans(dfm_lemma_tot_acDesc_causes_skm, 10, control = list(nruns = 50))
        
        cluster_lemma_tot_acDesc_causes$Cluster <-  skm_lemma_tot_acDesc_causes$cluster
        
        tokens_lemma_tot_acDesc_causes_wc <- tokens(POSTagging_lemma_udpipe_df$Tot_acDesc_causes, remove_punct = TRUE, remove_symbols = TRUE, remove_numbers = TRUE)
        tokens_lemma_tot_acDesc_causes_wc <- tokens_select(tokens_lemma_tot_acDesc_causes_wc, pattern = stopwords("en"), selection = "remove")
        dfm_lemma_tot_acDesc_causes_wc <- dfm(tokens_lemma_tot_acDesc_causes_wc, tolower = TRUE)
        dfm_lemma_tot_acDesc_causes_wc <- dfm_keep(dfm_lemma_tot_acDesc_causes_wc, min_nchar = 3)
        #dfm_lemma_tot_acDesc_causes_wc <- dfm_select(dfm_lemma_tot_acDesc_causes_wc, pattern =  c("accident", "seveso", "cause", "mhp", "valf", "occur"), selection = "remove")
        dfm_lemma_tot_acDesc_causes_wc <- dfm_trim(dfm_lemma_tot_acDesc_causes_wc, max_docfreq = 800, docfreq_type = "count")
        textplot_wordcloud(dfm_lemma_tot_acDesc_causes_wc, max_words = 100)
        
        table(cluster_lemma_tot_acDesc_causes$Cluster)
        topfeatures(dfm_lemma_tot_acDesc_causes_wc[cluster_lemma_tot_acDesc_causes$Cluster == 6,])
        
        
        #Cluster 1
        textplot_wordcloud(dfm_lemma_tot_acDesc_causes_wc[cluster_lemma_tot_acDesc_causes$Cluster == 1,], min_count = 3, max_words = 100)
        
        #Cluster 2
        textplot_wordcloud(dfm_lemma_tot_acDesc_causes_wc[cluster_lemma_tot_acDesc_causes$Cluster == 2,], min_count = 3, max_words = 100)
        
        #Cluster 3
        textplot_wordcloud(dfm_lemma_tot_acDesc_causes_wc[cluster_lemma_tot_acDesc_causes$Cluster == 3,], min_count = 3, max_words = 100)
        
        #Cluster 4
        textplot_wordcloud(dfm_lemma_tot_acDesc_causes_wc[cluster_lemma_tot_acDesc_causes$Cluster == 4,], min_count = 3, max_words = 100)
        
        #Cluster 5
        textplot_wordcloud(dfm_lemma_tot_acDesc_causes_wc[cluster_lemma_tot_acDesc_causes$Cluster == 5,], min_count = 3, max_words = 100)
        
        #Cluster 6
        textplot_wordcloud(dfm_lemma_tot_acDesc_causes_wc[cluster_lemma_tot_acDesc_causes$Cluster == 6,], min_count = 3, max_words = 100)
        
        #Cluster 7
        textplot_wordcloud(dfm_lemma_tot_acDesc_causes_wc[cluster_lemma_tot_acDesc_causes$Cluster == 7,], min_count = 3, max_words = 100)
        
        #Cluster 8
        textplot_wordcloud(dfm_lemma_tot_acDesc_causes_wc[cluster_lemma_tot_acDesc_causes$Cluster == 8,], min_count = 3, max_words = 100)
        
        #Cluster 9
        textplot_wordcloud(dfm_lemma_tot_acDesc_causes_wc[cluster_lemma_tot_acDesc_causes$Cluster == 9,], min_count = 3, max_words = 100)
       
        #Cluster 10
        textplot_wordcloud(dfm_lemma_tot_acDesc_causes_wc[cluster_lemma_tot_acDesc_causes$Cluster == 10,], min_count = 3, max_words = 100)
        
        
        counts <- sort(table(cluster_lemma_tot_acDesc_causes$Cluster),decreasing = TRUE)
        names(counts) <- c(1:10)
        barplot(counts ,
                xlab ="Cluster", ylab = "Number of accidents",
                font.lab = 1, cex.axis = 1.1, cex.lab = 1.2,
                cex.names = 1.1, cex.main = 2, ylim= c(0,250)) 

        
#HUMAN CONSEQUENCES ####
    ### ... checkboxes ####
        
        conseq_human_df <- data.frame()
        
        for(i in 1:nreports) {
          conseq_human_df[i, "AccidentID"] <- eMARS_df$AccidentID[i]
          
          reportJSON <- fromJSON(route_report[i,1])
          
          
          if(is.null(reportJSON$Data$Consequence_Tab$HumanOnSite$Quantity[1]) == TRUE) {
            
            conseq_human_df[i,"onsite_risk"]= NA
            
          } else {
            
            conseq_human_df[i,"onsite_risk"]=reportJSON$Data$Consequence_Tab$HumanOnSite$Quantity[1]
          }
          
          
          if(is.null(reportJSON$Data$Consequence_Tab$HumanOnSite$Quantity[2]) == TRUE) {
            
            conseq_human_df[i,"onsite_fatalities"]= NA
            
          } else {
            
            conseq_human_df[i,"onsite_fatalities"]=reportJSON$Data$Consequence_Tab$HumanOnSite$Quantity[2]
          }
          
          if(is.null(reportJSON$Data$Consequence_Tab$HumanOnSite$Quantity[3]) == TRUE) {
            
            conseq_human_df[i,"onsite_injuries"]= NA
            
          } else {
            
            conseq_human_df[i,"onsite_injuries"]=reportJSON$Data$Consequence_Tab$HumanOnSite$Quantity[3]
          }
          
          
          
          if(is.null(reportJSON$Data$Consequence_Tab$HumanOffSite$Quantity[1]) == TRUE) {
            
            conseq_human_df[i,"offsite_risk"]= NA
            
          } else {
            
            conseq_human_df[i,"offsite_risk"]=reportJSON$Data$Consequence_Tab$HumanOffSite$Quantity[1]
          }
          
          
          if(is.null(reportJSON$Data$Consequence_Tab$HumanOffSite$Quantity[2]) == TRUE) {
            
            conseq_human_df[i,"offsite_fatalities"]= NA
            
          } else {
            
            conseq_human_df[i,"offsite_fatalities"]=reportJSON$Data$Consequence_Tab$HumanOffSite$Quantity[2]
          }
          
          if(is.null(reportJSON$Data$Consequence_Tab$HumanOffSite$Quantity[3]) == TRUE) {
            
            conseq_human_df[i,"offsite_injuries"]= NA
            
          } else {
            
            conseq_human_df[i,"offsite_injuries"]=reportJSON$Data$Consequence_Tab$HumanOffSite$Quantity[3]
          }
          
          
        }
        

        
        
        #injury on site

        inj_onsite <- conseq_human_df$onsite_injuries %>% `>`(0) %>% which()
        
        sum(!is.na(conseq_human_df$onsite_injuries) & conseq_human_df$onsite_injuries != 0)
        
        
        #fatality on site
        
        fat_onsite <- conseq_human_df$onsite_fatalities %>% `>`(0) %>% which()
        
        sum(!is.na(conseq_human_df$onsite_fatalities) & conseq_human_df$onsite_fatalities != 0)
        
        #total on site
        
        inj_fat_onsite <- unique(c(inj_onsite, fat_onsite))
        
        sum((!is.na(conseq_human_df$onsite_injuries) & conseq_human_df$onsite_injuries != 0)  | 
              (!is.na(conseq_human_df$onsite_fatalities) & conseq_human_df$onsite_fatalities != 0))
        
        
        #injury off site
  
        inj_offsite <- conseq_human_df$offsite_injuries %>% `>`(0) %>% which()
        
        sum(!is.na(conseq_human_df$offsite_injuries) & conseq_human_df$offsite_injuries != 0) 
        
        #fatality off site
        
        fat_offsite <- conseq_human_df$offsite_fatalities %>% `>`(0) %>% which()
        
        sum(!is.na(conseq_human_df$offsite_fatalities) & conseq_human_df$offsite_fatalities != 0)
        
        #total off site
        
        inj_fat_offsite <- unique(c(inj_offsite, fat_offsite))
        
        sum((!is.na(conseq_human_df$offsite_injuries) & conseq_human_df$offsite_injuries != 0)  | 
              (!is.na(conseq_human_df$offsite_fatalities) & conseq_human_df$offsite_fatalities != 0))
        
        
        #total on site + off site
        
        inj_fat_total <- unique(c(inj_fat_onsite, inj_fat_offsite))
        
        sum((!is.na(conseq_human_df$onsite_injuries) & conseq_human_df$onsite_injuries != 0)  | 
              (!is.na(conseq_human_df$onsite_fatalities) & conseq_human_df$onsite_fatalities != 0) |
              (!is.na(conseq_human_df$offsite_injuries) & conseq_human_df$offsite_injuries != 0)  | 
              (!is.na(conseq_human_df$offsite_fatalities) & conseq_human_df$offsite_fatalities != 0))
        
        
        ##Incidencia
        
        #On Site + Off Site
        sum(((!is.na(conseq_human_df$onsite_injuries) & conseq_human_df$onsite_injuries != 0)  | 
              (!is.na(conseq_human_df$onsite_fatalities) & conseq_human_df$onsite_fatalities != 0)) &
              ((!is.na(conseq_human_df$offsite_injuries) & conseq_human_df$offsite_injuries != 0)  | 
              (!is.na(conseq_human_df$offsite_fatalities) & conseq_human_df$offsite_fatalities != 0)))
        
        #Only On Site
        sum(((!is.na(conseq_human_df$onsite_injuries) & conseq_human_df$onsite_injuries != 0)  | 
              (!is.na(conseq_human_df$onsite_fatalities) & conseq_human_df$onsite_fatalities != 0)) &
              ((is.na(conseq_human_df$offsite_injuries) | conseq_human_df$offsite_injuries == 0)  & 
              (is.na(conseq_human_df$offsite_fatalities) | conseq_human_df$offsite_fatalities == 0)))
        
        #Only Off Site
        sum(((!is.na(conseq_human_df$offsite_injuries) & conseq_human_df$offsite_injuries != 0)  | 
               (!is.na(conseq_human_df$offsite_fatalities) & conseq_human_df$offsite_fatalities != 0)) &
              ((is.na(conseq_human_df$onsite_injuries) | conseq_human_df$onsite_injuries == 0)  & 
                 (is.na(conseq_human_df$onsite_fatalities) | conseq_human_df$onsite_fatalities == 0)))
        
        #No injury/fatality
        sum(((is.na(conseq_human_df$onsite_injuries) | conseq_human_df$onsite_injuries == 0)  & 
               (is.na(conseq_human_df$onsite_fatalities) | conseq_human_df$onsite_fatalities == 0)) &
              ((is.na(conseq_human_df$offsite_injuries) | conseq_human_df$offsite_injuries == 0)  & 
                 (is.na(conseq_human_df$offsite_fatalities) | conseq_human_df$offsite_fatalities == 0)))
     
        
        
        
        
        
        
        
        
        #Incidencia
        
        human_incidece_df <- data.frame("N_Accidents" = c("YES_On_Site", "NO_On_Site"), "YES_Off_Site" = c(sum(((!is.na(conseq_human_df$onsite_injuries) & conseq_human_df$onsite_injuries != 0)  | (!is.na(conseq_human_df$onsite_fatalities) & conseq_human_df$onsite_fatalities != 0)) &((!is.na(conseq_human_df$offsite_injuries) & conseq_human_df$offsite_injuries != 0)  | (!is.na(conseq_human_df$offsite_fatalities) & conseq_human_df$offsite_fatalities != 0))),
                                                                                                           sum(((!is.na(conseq_human_df$offsite_injuries) & conseq_human_df$offsite_injuries != 0)  | (!is.na(conseq_human_df$offsite_fatalities) & conseq_human_df$offsite_fatalities != 0)) & ((is.na(conseq_human_df$onsite_injuries) | conseq_human_df$onsite_injuries == 0)  & (is.na(conseq_human_df$onsite_fatalities) | conseq_human_df$onsite_fatalities == 0)))),
                                        "NO_Off_Site" =  c(sum(((!is.na(conseq_human_df$onsite_injuries) & conseq_human_df$onsite_injuries != 0)  | (!is.na(conseq_human_df$onsite_fatalities) & conseq_human_df$onsite_fatalities != 0)) & ((is.na(conseq_human_df$offsite_injuries) | conseq_human_df$offsite_injuries == 0)  & (is.na(conseq_human_df$offsite_fatalities) | conseq_human_df$offsite_fatalities == 0))),
                                                          sum(((is.na(conseq_human_df$onsite_injuries) | conseq_human_df$onsite_injuries == 0)  & (is.na(conseq_human_df$onsite_fatalities) | conseq_human_df$onsite_fatalities == 0)) & ((is.na(conseq_human_df$offsite_injuries) | conseq_human_df$offsite_injuries == 0)  & (is.na(conseq_human_df$offsite_fatalities) | conseq_human_df$offsite_fatalities == 0)))))
        
        #Incidencia Fatalities
        
        human_incidece_fatalties_df <- data.frame("N_Accidents" = c("YES_On_Site", "NO_On_Site"), "YES_Off_Site" = c(sum((!is.na(conseq_human_df$onsite_fatalities) & conseq_human_df$onsite_fatalities != 0) & (!is.na(conseq_human_df$offsite_fatalities) & conseq_human_df$offsite_fatalities != 0)),
                                                                                                           sum((!is.na(conseq_human_df$offsite_fatalities) & conseq_human_df$offsite_fatalities != 0) & (is.na(conseq_human_df$onsite_fatalities) | conseq_human_df$onsite_fatalities == 0))),
                                        "NO_Off_Site" =  c(sum((!is.na(conseq_human_df$onsite_fatalities) & conseq_human_df$onsite_fatalities != 0) & (is.na(conseq_human_df$offsite_fatalities) | conseq_human_df$offsite_fatalities == 0)),
                                                           sum((is.na(conseq_human_df$onsite_fatalities) | conseq_human_df$onsite_fatalities == 0) & (is.na(conseq_human_df$offsite_fatalities) | conseq_human_df$offsite_fatalities == 0))))
        

        
    ### ... comparative wordcloud ####
       
        for (i in 1:1076){
          POSTagging_lemma_udpipe_df$Tot_acDesc_causes[i] <- list(c(unlist(POSTagging_lemma_udpipe_df$Noun_acDesc[i]),
                                                                    unlist(POSTagging_lemma_udpipe_df$Verb_acDesc[i]),
                                                                    unlist(POSTagging_lemma_udpipe_df$Adjective_acDesc[i]),
                                                                    unlist(POSTagging_lemma_udpipe_df$Noun_causes[i]),
                                                                    unlist(POSTagging_lemma_udpipe_df$Verb_causes[i]),
                                                                    unlist(POSTagging_lemma_udpipe_df$Adjective_causes[i])))
        }
        
        for (i in 1:1076){
          POSTagging_lemma_udpipe_df$Noun_acDesc_causes[i] <- list(c(unlist(POSTagging_lemma_udpipe_df$Noun_acDesc[i]),
                                                                     unlist(POSTagging_lemma_udpipe_df$Noun_causes[i])))
          
        }
        
        eMARS_acdesc_causes  <- vector(length = nreports)
        
        for (i in 1:nreports){
          eMARS_acdesc_causes[i] <-  paste(unlist(POSTagging_lemma_udpipe_df$Tot_acDesc_causes[i]), collapse = " ")
        }
        
        #On Site
        
        reports_inj_fat_oniste <- vector(length = nreports)
        reports_inj_fat_oniste[inj_fat_onsite] <- TRUE
        
        corpus_inj_fat_onsite <- corpus(unlist(eMARS_acdesc_causes), docnames = eMARS_df$AccidentID)
        
        
        docvars(corpus_inj_fat_onsite, field = "Injury") <- reports_inj_fat_oniste
        
        
        dfmat2 <- dfm(corpus_subset(corpus_inj_fat_onsite, Injury %in% c("FALSE", "TRUE")),
                      remove = stopwords("english"), remove_punct = TRUE, remove_symbols = TRUE,
                      remove_numbers = TRUE, groups = "Injury") %>%
          dfm_keep(min_nchar = 3) %>%
          dfm_trim(max_docfreq = 200, docfreq_type = "count")
        
        
        textplot_wordcloud(dfmat2, comparison = TRUE, max_words = 200,
                           color = c("darkgreen", "red"))
        
        
        #Off Site
        
        reports_inj_fat_offiste <- vector(length = nreports)
        reports_inj_fat_offiste[inj_fat_offsite] <- TRUE
        
        corpus_inj_fat_offsite <- corpus(unlist(eMARS_acdesc_causes), docnames = eMARS_df$AccidentID)
        
        
        docvars(corpus_inj_fat_offsite, field = "Injury") <- reports_inj_fat_offiste
        
        
        dfmat2 <- dfm(corpus_subset(corpus_inj_fat_offsite, Injury %in% c("FALSE", "TRUE")),
                      remove = stopwords("english"), remove_punct = TRUE, remove_symbols = TRUE,
                      remove_numbers = TRUE, groups = "Injury") %>%
          dfm_keep(min_nchar = 3) %>%
          dfm_trim(max_docfreq = 200, docfreq_type = "count")
        
        
        textplot_wordcloud(dfmat2, comparison = TRUE, max_words = 200,
                           color = c("darkgreen", "red"))
        
        
        
        #Total
        
        reports_inj_fat_total <- vector(length = nreports)
        reports_inj_fat_total[inj_fat_total] <- TRUE
        
        corpus_inj_fat_total <- corpus(unlist(eMARS_acdesc_causes), docnames = eMARS_df$AccidentID)
        
        
        docvars(corpus_inj_fat_total, field = "Injury") <- reports_inj_fat_total
        
        
        dfmat2 <- dfm(corpus_subset(corpus_inj_fat_total, Injury %in% c("FALSE", "TRUE")),
                      remove = stopwords("english"), remove_punct = TRUE, remove_symbols = TRUE,
                      remove_numbers = TRUE, groups = "Injury") %>%
          dfm_keep(min_nchar = 3) %>%
          dfm_trim(max_docfreq = 200, docfreq_type = "count") %>%
          dfm_select(pattern =  c("accident", "seveso", "cause", "occur", "mhp", "k23", "valf"), selection = "remove")
        
        
        textplot_wordcloud(dfmat2, comparison = TRUE, max_words = 200, min_count = 3,
                           color = c("darkgreen", "red")) 
        
        
    ### ... text reference words ####
          
        ud_model <- udpipe_download_model(language = "english")
        ud_model <- udpipe_load_model(ud_model$file_model)
        
        POSTagging_human_conseq_df <- data.frame("AccidentID" = vector(length=nreports), "Title" = vector(length=nreports), 
                                                 "Sentence_num" = vector(length=nreports), "Sentence" = vector(length=nreports),
                                                 "Nouns" = vector(length=nreports), "Verbs" = vector(length=nreports),
                                                 "Adjectives" = vector(length=nreports), "Total_words" = vector(length=nreports))
        
        human_ref <- c("injure", "injury", "death", "die", "employee", "firefight", "fireman", "firefighter", "worker", "operator", "fatality",	
                       "intoxicate", "person", "people", "hospital", "hospitalise", "hospitalize", "kill", "killed")
        
        human_ref <- c("employee", "worker", "operator", "person", "people")
        
        for (i in 1:nreports){
          y <- udpipe_annotate(ud_model, x = eMARS_df$ConsequenceDescription[i], doc_id = eMARS_df$AccidentID[i])
          POS_human_conseq <- as.data.frame(y) 
          
          numsentence <- vector()
          human_conseq_sentence_df <- data.frame()
          noun_human_conseq <- vector()
          verb_human_conseq <- vector()
          adj_human_conseq <- vector()
          total_human_conseq <- vector()
          sentence_human <- vector()
          sentence_human_unique <- vector()
          
          if((grepl("No people were injured during the accident", eMARS_df$ConsequenceDescription[i], fixed = TRUE)) == FALSE
             & (grepl("no injuries", eMARS_df$ConsequenceDescription[i], fixed = TRUE)) == FALSE
             & (grepl("No injuries", eMARS_df$ConsequenceDescription[i], fixed = TRUE)) == FALSE){
            for (j in 1:length(human_ref)){
              
              h <- POS_human_conseq$sentence_id[POS_human_conseq$lemma == human_ref[j]]
              numsentence <- c(numsentence,h)
            }
            
            
            num_sentence_unique <- unique(numsentence)
            
            
            if (length(numsentence) != 0){
              
              for (k in 1:length(num_sentence_unique)){
                
                human_conseq_sentence_df <- POS_human_conseq[POS_human_conseq$sentence_id == num_sentence_unique[k] & POS_human_conseq$upos == "NOUN",]
                noun_human_conseq <- c(noun_human_conseq,human_conseq_sentence_df$lemma)
                
                human_conseq_sentence_df <- POS_human_conseq[POS_human_conseq$sentence_id == num_sentence_unique[k] & POS_human_conseq$upos == "VERB",]
                verb_human_conseq <- c(verb_human_conseq,human_conseq_sentence_df$lemma)
                
                human_conseq_sentence_df <- POS_human_conseq[POS_human_conseq$sentence_id == num_sentence_unique[k] & POS_human_conseq$upos == "ADJ",]
                adj_human_conseq <- c(adj_human_conseq,human_conseq_sentence_df$lemma)
                
                sentence_human <- POS_human_conseq$sentence[POS_human_conseq$sentence_id == num_sentence_unique[k]]
                sentence_human_unique <- c(sentence_human_unique,sentence_human[1])
              }
              
              total_human_conseq <- c(noun_human_conseq, verb_human_conseq, adj_human_conseq)
            }
          }
          
          POSTagging_human_conseq_df$AccidentID[i] <- eMARS_df$AccidentID[i]
          POSTagging_human_conseq_df$Title[i] <- eMARS_df$Title[i]
          POSTagging_human_conseq_df$Sentence_num[i] <- list(num_sentence_unique)
          POSTagging_human_conseq_df$Sentence[i] <- list(unlist(sentence_human_unique))
          POSTagging_human_conseq_df$Nouns[i] <- list(unlist(noun_human_conseq))
          POSTagging_human_conseq_df$Verbs[i] <- list(unlist(verb_human_conseq))
          POSTagging_human_conseq_df$Adjectives[i] <- list(unlist(adj_human_conseq))
          POSTagging_human_conseq_df$Total_words[i] <- list(unlist(total_human_conseq))
          
        }
        
        sum(POSTagging_human_conseq_df$Sentence_num != "integer(0)")
        
        
        inj_fat_text <- POSTagging_human_conseq_df$Sentence_num %>% `!=` ("integer(0)") %>% which()
        inj_fat_text_total_int <- intersect(inj_fat_text, inj_fat_total)
        inj_fat_text_total_out <- outersect(inj_fat_text, inj_fat_text_total_int)
        
        reports_inj_fat_text <- vector(length = length(inj_fat_text))
        reports_inj_fat_text[1:length(inj_fat_text_total_int)] <- TRUE
        reports_inj_fat_text[(length(inj_fat_text_total_int)+1):length(inj_fat_text)] <- FALSE
        
        inj_fat_text_acdesc_causes <- eMARS_acdesc_causes[inj_fat_text_total_int]
        inj_fat_text_acdesc_causes <- c(inj_fat_text_acdesc_causes, eMARS_acdesc_causes[inj_fat_text_total_out])
        
        ID_inj_fat_text <- eMARS_df$AccidentID[inj_fat_text_total_int]
        ID_inj_fat_text <- c(ID_inj_fat_text, eMARS_df$AccidentID[inj_fat_text_total_out])
        
        
        
        corpus_inj_fat_text <- corpus(unlist(inj_fat_text_acdesc_causes), docnames = ID_inj_fat_text)
        
        docvars(corpus_inj_fat_text, field = "Checkbox") <- reports_inj_fat_text
        
        dfmat2 <- dfm(corpus_subset(corpus_inj_fat_text, Checkbox %in% c("FALSE", "TRUE")),
                      remove = stopwords("english"), remove_punct = TRUE, remove_symbols = TRUE,
                      remove_numbers = TRUE, groups = "Checkbox") %>%
          dfm_keep(min_nchar = 3) %>%
          dfm_trim(max_docfreq = 450, docfreq_type = "count")
        
        textplot_wordcloud(dfmat2, comparison = TRUE, max_words = 200,
                           color = c("darkgreen", "red"))
      
    
        
        
        
    ### ... major accidents ####
        
        # >= 20 injuries
        
        reports_inj20 <- conseq_human_df$onsite_injuries %>% `>=`(20) %>% which()
        
        injury_20_df <- data.frame("AccidentID" = vector(length=length(reports_inj20)), "Injuries"= vector(length=length(reports_inj20)),
                                   "Substance_CAS" = vector(length=length(reports_inj20)), "Substance_Name" = vector(length=length(reports_inj20)),
                                   "Explosion" = vector(length=length(reports_inj20)), "Fire" = vector(length=length(reports_inj20)),
                                   "Release" = vector(length=length(reports_inj20)))
        
        for (i in 1:length(reports_inj20)){
          injury_20_df$AccidentID[i] <- eMARS_df$AccidentID[reports_inj20[i]]
          injury_20_df$Injuries[i] <- conseq_human_df$onsite_injuries[reports_inj20[i]]
          injury_20_df$Substance_CAS[i] <- CAS_reportsdf$CAS_total[reports_inj20[i]]
          
          CAS_name <- vector() 
          for (j in 1:length(unlist(injury_20_df$Substance_CAS[i]))){
             
             CAS_index <- which(CAS_RN_df$CAS_RN == unlist(injury_20_df$Substance_CAS[i])[j])
            
             CAS_name <- c(CAS_name,CAS_RN_df$Substance[CAS_index])
             
           }
          injury_20_df$Substance_Name[i] <- list(CAS_name)
          
          injury_20_df$Explosion[i] <- sum(eMARS_checkbox_df[reports_inj20[i],14:21] == TRUE) >= 1
          injury_20_df$Fire[i] <- sum(eMARS_checkbox_df[reports_inj20[i],1:7] == TRUE) >= 1
          injury_20_df$Release[i] <- sum(eMARS_checkbox_df[reports_inj20[i],8:13] == TRUE) >= 1
        }

        # >= 10 fatalities
        
        reports_fat10 <- conseq_human_df$onsite_fatalities %>% `>=`(10) %>% which()
        
        fatality_10_df <- data.frame("AccidentID" = vector(length=length(reports_fat10)), "Injuries"= vector(length=length(reports_fat10)),
                                   "Substance_CAS" = vector(length=length(reports_fat10)), "Substance_Name" = vector(length=length(reports_fat10)),
                                   "Explosion" = vector(length=length(reports_fat10)), "Fire" = vector(length=length(reports_fat10)),
                                   "Release" = vector(length=length(reports_fat10)))
        
        for (i in 1:length(reports_fat10)){
          fatality_10_df$AccidentID[i] <- eMARS_df$AccidentID[reports_fat10[i]]
          fatality_10_df$Injuries[i] <- conseq_human_df$onsite_fatalities[reports_fat10[i]]
          fatality_10_df$Substance_CAS[i] <- CAS_reportsdf$CAS_total[reports_fat10[i]]
          
          CAS_name <- vector() 
          for (j in 1:length(unlist(fatality_10_df$Substance_CAS[i]))){
            
            CAS_index <- which(CAS_RN_df$CAS_RN == unlist(fatality_10_df$Substance_CAS[i])[j])
            
            CAS_name <- c(CAS_name,CAS_RN_df$Substance[CAS_index])
            
          }
          fatality_10_df$Substance_Name[i] <- list(CAS_name)
          
          fatality_10_df$Explosion[i] <- sum(eMARS_checkbox_df[reports_fat10[i],14:21] == TRUE) >= 1
          fatality_10_df$Fire[i] <- sum(eMARS_checkbox_df[reports_fat10[i],1:7] == TRUE) >= 1
          fatality_10_df$Release[i] <- sum(eMARS_checkbox_df[reports_fat10[i],8:13] == TRUE) >= 1
        }
        

               
    ### ... comparative wordcloud checkboxes vs text ####
        
        
        tokens_human_checkboxes <- tokens(POSTagging_lemma_udpipe_df$Noun_acDesc_causes[inj_fat_total], remove_punct = TRUE, remove_symbols = TRUE, remove_numbers = TRUE)
        tokens_human_checkboxes <- tokens_select(tokens_human_checkboxes, pattern = stopwords("en"), selection = "remove")
        dfm_human_checkboxes <- dfm(tokens_human_checkboxes, tolower = TRUE)
        #dfm_human_checkboxes <- dfm_trim(dfm_human_checkboxes, max_docfreq = 300, docfreq_type = "count")
        #dfm_human_checkboxes <- dfm_select(dfm_human_checkboxes, pattern =  c("accident", "seveso", "cause", "occur"), selection = "remove")
        dfm_human_checkboxes <- dfm_keep(dfm_human_checkboxes, min_nchar = 3)
        textplot_wordcloud(dfm_human_checkboxes, max_words = 100)
        
        tokens_human_text <- tokens(POSTagging_lemma_udpipe_df$Noun_acDesc_causes[POSTagging_human_conseq_df$Sentence_num != "integer(0)"], remove_punct = TRUE, remove_symbols = TRUE, remove_numbers = TRUE)
        tokens_human_text <- tokens_select(tokens_human_text, pattern = stopwords("en"), selection = "remove")
        dfm_human_text <- dfm(tokens_human_text, tolower = TRUE)
        #dfm_human_text <- dfm_trim(dfm_human_text, max_docfreq = 400, docfreq_type = "count")
        #dfm_human_text <- dfm_select(dfm_human_text, pattern =  c("accident", "seveso", "cause", "occur"), selection = "remove")
        dfm_human_text <- dfm_keep(dfm_human_text, min_nchar = 3)
        textplot_wordcloud(dfm_human_text, max_words = 100)
        
        
#FUNCTIONS ####
      
      clean_data_f <- function (txt) {
        
        clean1 <- replace_html(txt)
        clean2 <- gsub("<.*?>","",clean1)
        clean2 <- replace_html(clean2)
        clean2 <- gsub("&nbsp;","",clean2)
        clean2 <- gsub("\n"," ",clean2)
        clean2 <- gsub("st1.*}","",clean2)

        clean2
      }
      


  
      
      
# Jennifer Collister
# 22/09/20

library(glue)
library(lubridate)
library(readxl)

# Load the project config file for filepaths etc
if (!exists("config")) {
  library(yaml)
  config = yaml.load_file("config.yml")
}

# Source the function definitions
# XL: Remove 'Reorganise' in the file path
source(file.path(config$scripts$cleaning, "basic_functions.R"), local = TRUE)

# makeEnum <- function(inputList) {
#   # Borrowed from https://stackoverflow.com/a/41509345
#   myEnum <- as.list(inputList)
#   enumNames <- names(myEnum)
#   if (is.null(enumNames)) {
#     names(myEnum) <- myEnum
#   } else if ("" %in% enumNames) {
#     stop("The inputList has some but not all names assigned. They must be all assigned or none assigned")
#   }
#   return(myEnum)
# }
# visits <- makeEnum(list(baseline = c("0", "baseline assessment"), 
#                     repeat_visit = c("1", "repeat visit"), 
#                     imaging = c("2", "imaging visit"), 
#                     repeat_imaging = c("3","repeat imaging visit")))

# Formatting of existing UKB variables

ID <- function() {
  list(
    name = "ID",
    source = "ID",
    mapper = FN_id,
    post_exclusion = FALSE,
    display_name = "ID",
    description = "The unique participant identifier"
  )
}


BaC_Sex <- function() {
  list(
    name = "BaC_Sex",
    source = "BaC_Sex.0.0",
    mapper = FN_unorder,
    post_exclusion = FALSE,
    display_name = "Gender",
    description = "Participant's self-reported gender"
  )
}

Rec_DateAssess <- function() {
  list(
    name = "Rec_DateAssess",
    source = c("Rec_DateAssess.0.0"),
    mapper = FN_toDate,
    post_exclusion = FALSE,
    display_name = "Date of baseline assessment",
    description = "Date of baseline assessment"
  )
}

TEU_BaC_DateOfBirth <- function() {
  list(
    name = "TEU_BaC_DateOfBirth",
    source = c("BaC_BirthMonth.0.0", "BaC_BirthYear.0.0"),
    mapper = FN_MYtoDate(
      day = 15,
      monthField = "BaC_BirthMonth.0.0",
      yearField = "BaC_BirthYear.0.0"
    ),
    post_exclusion = FALSE,
    display_name = "Date of Birth",
    description = "The participant's approximate date of birth, derived from self-reported month and year with date estimated as 15th"
  )
}

TEU_BaC_AgeAtRec <- function() {
  list(
    name = "TEU_BaC_AgeAtRec",
    source = c("TEU_BaC_DateOfBirth", "Rec_DateAssess"),
    mapper = function(data) {
      as.numeric(round(difftime(data[["Rec_DateAssess"]], data[["TEU_BaC_DateOfBirth"]], unit =
                                  "days") / 365.25,
                       digits = 2))
    },
    post_exclusion = FALSE,
    display_name = "Age at recruitment, years",
    description = "The participant's approximate age at recruitment, derived from date of assessment centre visit and self-reported month and year of birth (date of birth estimated as 15th of the month)"
  )
}

TEU_BlP_SBP.0.0 <- function() {
  list(
    name = "TEU_BlP_SBP.0.0",
    source = c("BlP_SBPAuto.0.0", "BlP_SBPMan.0.0"),
    mapper = function(data) {
      coalesce(data[["BlP_SBPAuto.0.0"]], data[["BlP_SBPMan.0.0"]])
    },
    post_exclusion = FALSE,
    display_name = "First SBP at baseline",
    description = "First SBP measurement at baseline, either automated or manual"
  )
}

TEU_BlP_SBP.0.1 <- function() {
  list(
    name = "TEU_BlP_SBP.0.1",
    source = c("BlP_SBPAuto.0.1", "BlP_SBPMan.0.1"),
    mapper = function(data) {
      coalesce(data[["BlP_SBPAuto.0.1"]], data[["BlP_SBPMan.0.1"]])
    },
    post_exclusion = FALSE,
    display_name = "Second SBP at baseline",
    description = "Second SBP measurement at baseline, either automated or manual"
  )
}

TEU_BlP_DBP.0.0 <- function() {
  list(
    name = "TEU_BlP_DBP.0.0",
    source = c("BlP_DBPAuto.0.0", "BlP_DBPMan.0.0"),
    mapper = function(data) {
      coalesce(data[["BlP_DBPAuto.0.0"]], data[["BlP_DBPMan.0.0"]])
    },
    post_exclusion = FALSE,
    display_name = "First DBP at baseline",
    description = "First DBP measurement at baseline, either automated or manual"
  )
}

TEU_BlP_DBP.0.1 <- function() {
  list(
    name = "TEU_BlP_DBP.0.1",
    source = c("BlP_DBPAuto.0.1", "BlP_DBPMan.0.1"),
    mapper = function(data) {
      coalesce(data[["BlP_DBPAuto.0.1"]], data[["BlP_DBPMan.0.1"]])
    },
    post_exclusion = FALSE,
    display_name = "Second DBP at baseline",
    description = "Second DBP measurement at baseline, either automated or manual"
  )
}

TEU_BlP_nSBP <- function() {
  list(
    name = "TEU_BlP_nSBP",
    source = c("TEU_BlP_SBP.0.0", "TEU_BlP_SBP.0.1"),
    mapper = function(data) {
      rowSums(!is.na(data[, c("TEU_BlP_SBP.0.0", "TEU_BlP_SBP.0.1")]))
    },
    post_exclusion = FALSE,
    display_name = "No. SBP",
    description = "Number of SBP measurements taken at baseline"
  )
}

TEU_BlP_nDBP <- function() {
  list(
    name = "TEU_BlP_nDBP",
    source = c("TEU_BlP_DBP.0.0", "TEU_BlP_DBP.0.1"),
    mapper = function(data) {
      rowSums(!is.na(data[, c("TEU_BlP_DBP.0.0", "TEU_BlP_DBP.0.1")]))
    },
    post_exclusion = FALSE,
    display_name = "No. DBP",
    description = "Number of DBP measurements taken at baseline"
  )
}

TEU_BlP_SBP.avg <- function() {
  list(
    name = "TEU_BlP_SBP.avg",
    source = c("TEU_BlP_SBP.0.0",
               "TEU_BlP_SBP.0.1"),
    mapper = FN_average(colnames = c("TEU_BlP_SBP.0.0",
                                     "TEU_BlP_SBP.0.1")),
    post_exclusion = FALSE,
    display_name = "Baseline SBP",
    description = "The average systolic blood pressure at baseline"
  )
}

TEU_BlP_SBP_quintiles <- function() {
  list(
    name = "TEU_BlP_SBP_quintiles", 
    source = c("TEU_BlP_SBP.avg"), 
    mapper = FN_quantiles(quant=5),
    post_exclusion = TRUE,
    display_name = "Baseline Systolic BP quintiles",
    description = "Quintiles of the measured systolic blood pressure at baseline"
  )
}

TEU_BlP_DBP.avg <- function() {
  list(
    name = "TEU_BlP_DBP.avg",
    source = c("TEU_BlP_DBP.0.0",
               "TEU_BlP_DBP.0.1"),
    mapper = FN_average(colnames = c("TEU_BlP_DBP.0.0",
                                     "TEU_BlP_DBP.0.1")),
    post_exclusion = FALSE,
    display_name = "Baseline DBP",
    description = "The average diastolic blood pressure at baseline"
  )
}

TEU_Rec_AssessCentre <- function() {
  list(
    name = "TEU_Rec_AssessCentre",
    source = "Rec_AssessCentre.0.0",
    mapper = function(x) {
      map <- read.csv_kdrive(file.path(config$cleaning$coding,"coding10_AssessmentCentre.csv"))
      # https://biobank.ndph.ox.ac.uk/showcase/coding.cgi?id=10
      y <- map[["meaning"]][match(x, map$Code)]
    },
    post_exclusion = FALSE,
    display_name = "AssessCentre",
    description = "Which assessment centre did the participant attend"
  )
}

TEU_Rec_Country <- function() {
  list(
    name = "TEU_Rec_Country",
    source = "Rec_AssessCentre.0.0",
    mapper = function(x) {
      y <- dplyr::case_when(
        x %in% c(
          10003,
          11001,
          11002,
          11006,
          11007,
          11008,
          11009,
          11010,
          11011,
          11012,
          11013,
          11014,
          11016,
          11017,
          11018,
          11020,
          11021,
          11024,
          11025,
          11026,
          11027,
          11028
        ) ~ "England",
        x %in% c(11004, 11005) ~ "Scotland",
        x %in% c(11003, 11022, 11023) ~ "Wales",
        TRUE ~ x
      )
      if (!all(y %in% c("England", "Scotland", "Wales"))) {
        warning(paste0("Unrecognised centre code: ", y[!y %in% c("England", "Scotland", "Wales")]))
      }
      y <- factor(y, levels=c("England", "Scotland", "Wales"))
      return(y)
    },
    post_exclusion = FALSE,
    display_name = "UK country of residence",
    description = "Which country does the participant live in"
  )
}


TEU_BaC_AgeCat <- function() {
  list(
    name = "TEU_BaC_AgeCat",
    source = "TEU_BaC_AgeAtRec",
    mapper = FN_buckets(
      breaks = c(40, 50, 60, 70),
      labels = c("40-49", "50-59", "60-69"),
      right = FALSE
    ),
    post_exclusion = FALSE,
    display_name = "Age group, years",
    description = "Categorised age in years"
  )
}


TEU_ethnicgrp <- function() {
  list(
    name = "TEU_ethnicgrp",
    source = "Eth_Ethnicity.0.0",
    mapper = function(x) {
      y <- dplyr::case_when(
        x %in% c("White", "British", "Irish", "Any other white background") ~ "White",
        x %in% c(
          "Mixed",
          "White and Black Caribbean",
          "White and Black African",
          "White and Asian",
          "Any other mixed background"
        ) ~ "Mixed",
        x %in% c("Indian", "Pakistani", "Bangladeshi") ~ "S. Asian",
        x %in% c(
          "Black or Black British",
          "Caribbean",
          "African",
          "Any other Black background"
        ) ~ "Black",
        x %in% c(
          "Other ethnic group",
          "Asian or Asian British",
          "Any other Asian background",
          "Chinese"
        ) ~ "Other",
        x %in% c("Do not know", "Prefer not to answer") ~ "Unanswered",
        is.na(x) ~ "Unanswered",
        TRUE ~ "Error"
      )
      y <-
        factor(
          y,
          ordered = FALSE,
          levels = c("White", "Black", "S. Asian", "Mixed",
                     "Other", "Unanswered")
        )
      return(y)
    },
    post_exclusion = FALSE,
    display_name = "Ethnic group",
    description = "The participant's self-reported ethnicity, condensed into categories.\n'White', 'British', 'Irish' and 'Any other white background' were coded as 'White'.\n'Indian', 'Pakinstani' and 'Bangladeshi' were coded as 'S. Asian'.\n'Black or Black British', 'Carribean', 'African' and 'Any other Black background' were coded as 'Black'.\n'Mixed', 'White and Black Caribbean', 'White and Black African', 'White and Asian' and 'Any other mixed background' were coded as 'Mixed'.\n'Other ethnic group', 'Asian or Asian British', 'Any other Asian background' and 'Chinese' were coded as 'Other'"
  )
}

TEU_SBP_PRS <- function() {
  list(
    name = "TEU_SBP_PRS", 
    source = c("ID"), 
    mapper = FN_JoinPRS(filepath="K:/TEU/UKB_Genetic_Data/PRS_Pipeline/prs/projects/htn-evangelou2018/outputs/prs_SBP_20211102.sscore",
                        colname="SCORE1_AVG"),
    post_exclusion = FALSE,
    display_name = "SBP polygenic risk score",
    description = "SBP polygenic risk score from Evangelou 2018 paper"
  )
}

TEU_SBP_PRS_Warren <- function() {
  list(
    name = "TEU_SBP_PRS_Warren", 
    source = c("ID"), 
    mapper = FN_JoinPRS(filepath="K:/TEU/UKB_Genetic_Data/PRS_Pipeline/prs/projects/htn-warren2017/outputs/prs_20211102.sscore",
                        colname="SCORE1_AVG"),
    post_exclusion = FALSE,
    display_name = "SBP polygenic risk score",
    description = "SBP polygenic risk score from Warren 2017 paper"
  )
}

TEU_BrCa_313_PRS <- function() {
  list(
    name = "TEU_BrCa_313_PRS", 
    source = c("ID"), 
    mapper = FN_JoinPRS(filepath="K:/TEU/UKB_Genetic_Data/PRS_Pipeline/prs/projects/brca313-mavaddat2018/outputs/prs_20211102.sscore",
                        colname="SCORE1_AVG"),
    post_exclusion = FALSE,
    display_name = "BrCa 100k PRS",
    description = "Breast cancer polygenic risk score, 313 SNPs from Mavaddat 2018 paper"
  )
}

TEU_BrCa_100k_PRS <- function() {
  list(
    name = "TEU_BrCa_100k_PRS", 
    source = c("ID"), 
    mapper = FN_JoinPRS(filepath="K:/TEU/UKB_Genetic_Data/PRS_Pipeline/prs/projects/brca100k-fritsche2020/outputs/prs_20211102.sscore",
                        colname="SCORE1_AVG"),
    post_exclusion = FALSE,
    display_name = "BrCa 100k PRS",
    description = "Breast cancer polygenic risk score, 100k SNPs from Fritsche 2020 paper"
  )
}

TEU_DementiaAlz_PRS <- function() {
  list(
    list(
      name = "Alz_PRS_Ebenau", 
      source = c("ID"),
      mapper = FN_JoinPRS(filepath="K:/TEU/UKB_Genetic_Data/PRS_Pipeline/prs/projects/alz39_Ebenau/outputs/prs_20211102.sscore",
                          colname="SCORE1_AVG"),
      post_exclusion = FALSE,
      display_name = "Alzheimers polygenic risk score, Ebenau2021",
      description = "Alzheimers polygenic risk score, 39 SNPs, from Ebenau 2021 paper"
    ),
    list(
      name = "Alz_PRS_Najar", 
      source = c("ID"),
      mapper = FN_JoinPRS(filepath="K:/TEU/UKB_Genetic_Data/PRS_Pipeline/prs/projects/alz57_Najar/outputs/prs_20220531.sscore",
                          colname="SCORE1_AVG"),
      post_exclusion = FALSE,
      display_name = "Alzheimers polygenic risk score, Najar2021",
      description = "Alzheimers polygenic risk score, 57 SNPs, from Najar 2021 paper"
    )
  )
}

GeP_PC <- function(pc=1) {
  list(
    name = paste0("GeP_PC_", pc), 
    source = glue("GeP_PC.0.{pc}"), 
    mapper = FN_id,
    post_exclusion = FALSE,
    display_name = glue("Principal component {pc}"),
    description = glue("Genetic principal component {pc}, from Bycroft")
  )
}

GeP_Batch <- function() {
  list(
    name = "GeP_Batch", 
    source = "GeP_Batch.0.0", 
    mapper = function(x) {
      coding <- read.csv_kdrive(file.path(config$cleaning$coding, "coding22000_flat_GenotypingArray.csv"))
      y <- factor(coding$L1[match(x, coding$Code)])
    },
    post_exclusion = FALSE,
    display_name = "Genotype measurement batch",
    description = "Genotype measurement batch"
  )
}

GeP_Array <- function() {
  list(
    name = "GeP_Array", 
    source = "GeP_Batch.0.0", 
    mapper = function(x){
      coding <- read.csv_kdrive(file.path(config$cleaning$coding, "coding22000_flat_GenotypingArray.csv"))
      y <- factor(coding$L0[match(x, coding$Code)], levels=c("Axiom", "BiLEVE"))
    },
    post_exclusion = FALSE,
    display_name = "Genotype array",
    description = "Genotype array - UK BiLEVE or Biobank Axiom Array"
  )
}

GeP_ethnic <- function() {
  list(
    name = "GeP_ethnic", 
    source = "GeP_ethnic.0.0", 
    mapper = FN_id,
    post_exclusion = FALSE,
    display_name = "Genotype ethnic grouping",
    description = "Genotype ethnic grouping"
  )
}


GeP_UsedInPCA <- function() {
  list(
    name = "GeP_UsedInPCA", 
    source = c("GeP_UsedInPCA.0.0"), 
    mapper = FN_id,
    post_exclusion = FALSE,
    display_name = "string",
    description = "text"
  )
}

GeP_Outliers <- function() {
  list(
    name = "GeP_Outliers", 
    source = c("GeP_Outliers.0.0"), 
    mapper = FN_id,
    post_exclusion = FALSE,
    display_name = "string",
    description = "text"
  )
}

GeP_Plate <- function() {
  list(
    name = "GeP_Plate", 
    source = c("GeP_Plate.0.0"), 
    mapper = FN_id,
    post_exclusion = FALSE,
    display_name = "string",
    description = "text"
  )
}

GeP_Sex <- function() {
  list(
    name = "GeP_Sex", 
    source = c("GeP_Sex.0.0"), 
    mapper = FN_id,
    post_exclusion = FALSE,
    display_name = "string",
    description = "text"
  )
}

GeP_Kinship <- function() {
  list(
    name = "GeP_Kinship", 
    source = c("GeP_Kinship.0.0"), 
    mapper = FN_id,
    post_exclusion = FALSE,
    display_name = "string",
    description = "text"
  )
}

#--------------------------------------------------------------------------------------------------------------
# XL add: MACE (status) at baseline (TEU_MACE_prev) 

# 1. HES source 
TEU_HES_MACE_prev<-function(record_level=FALSE){
  list(
    name='TEU_HES_MACE_prev',
    source=if(record_level){
      c("ID","Rec_DateAssess")
    } else {
      c("ID", "Rec_DateAssess",
        paste0("HES_ICD9Diag.0.", seq(0, 46, by=1)),
        paste0("HES_ICD9DateFirst.0.",seq(0,46,by=1)),
        paste0("HES_ICD10Diag.0.", seq(0, 212, by=1)),
        paste0("HES_ICD10DateFirst.0.",seq(0,212,by=1)),
        paste0("HES_OPCS4Code.0.",seq(0,116,by=1)),
        paste0("HES_OPCS4DateFirst.0.",seq(0,116,by=1)))
    },
    mapper=FN_HES_First(ICD9_xlsx = file.path(config$cleaning$mapping,'MACE/HES_ICD9_Mapping_20210128.xlsx'),
                        ICD10_xlsx = file.path(config$cleaning$mapping,'MACE/HES_ICD10_Mapping_20210128.xlsx'),
                        OPCS4_xlsx = file.path(config$cleaning$mapping,'MACE/HES_OPCS4_Mapping_20210128.xlsx'),
                        condition = 'MACE',
                        return_label = 'baseline',
                        record_level = record_level),
    post_exclusion=FALSE,
    display_name='MACE status identified from HES data prior to or at baseline',
    description='MACE status identified from HES (ICD-9, ICD-10, OPCS-4) data prior to or at baseline'
  )
}


# 2. Verbal Interview (VI)
TEU_VeI_MACE_nonc<-function(condition='MACE'){
  list(
    name = 'TEU_VeI_MACE_nonc',
    source=c("ID", "Rec_DateAssess",
      paste0("VeI_NonCancerCode.0.", seq(0, 33, by=1)),
      paste0("VeI_NonCancerYear.0.", seq(0, 33, by=1))),
    mapper= function(data){
      # read in analysis codings xlsx
      mapping=read.xlsx_kdrive(file.path(config$cleaning$mapping,'MACE/VI_NonCancerIllness_Mapping_20210128.xlsx'),col_types = c('text'))
      
      dx_codes<-as.numeric(mapping[which(mapping$Conditions==condition),]$Code)
      
      y<- FN_VI_filtercodes(dx_codes = dx_codes,
                            colname = "VeI_NonCancer",
                            instance = 0,
                            return_label = "dx",
                            mapper = file.path(config$cleaning$coding,"coding6_flat_NonCancerIllness.csv"))(data)
      # If not blank, assign yes
      y<- factor(ifelse(is.na(y), 0, 1), levels = c(0,1), labels = c('No','Yes'))
      
      return(y)
    },      
    post_exclusion = FALSE,
    display_name = "Self-reported MACE from Verbal interview at baseline",
    description = "Self-reported MACE from Verbal interview (Non-cancer illness) at baseline"
  )
}


TEU_VeI_MACE_op<-function(condition='MACE'){
  list(
    name = 'TEU_VeI_MACE_op',
    source = c("ID", "Rec_DateAssess",
               paste0("VeI_OperationCode.0.", seq(0, 31, by=1)),
               paste0("VeI_OperationYear.0.", seq(0, 31, by=1))),
    mapper = function(data){
      
      mapping=read.xlsx_kdrive(file.path(config$cleaning$mapping,'MACE/VI_Operations_Mapping_20210128.xlsx'),col_types = c('text'))
      
      dx_codes<-as.numeric(mapping[which(mapping$Conditions==condition),]$Code)
      y<- FN_VI_filtercodes(dx_codes = dx_codes,
                            colname = "VeI_Operation",
                            instance = 0,
                            return_label = "dx",
                            mapper = file.path(config$cleaning$coding,"coding5_flat_Operation.csv"))(data)
      # If not blank, assign yes
      y<- factor(ifelse(is.na(y), 0, 1), levels = c(0,1), labels = c('No','Yes'))
      
      return(y)
    },
    post_exclusion = FALSE,
    display_name = 'Self-reported MACE operation from Verbal interview at baseline',
    description = 'Self-reported MACE operation from Verbal interview (Operations) at baseline'
  )
  
}


# XL add: MACE (TQ)
TEU_HMH_MACE_prev <- function() {
  list(
    name = "TEU_HMH_MACE_prev", 
    source = c(paste0("HMH_HeartProbs.0.", c(0:3))), 
    mapper = function(data){
      y<-FN_Vascular_condition(condition=c("Stroke", "Heart attack"), string="MACE")(data)
      levels(y)<-c('Yes','No','Unanswered')
      return(y)
      },
    post_exclusion = FALSE,
    display_name = "Self-reported MACE on TQ at baseline",
    description = "Self-reported MACE (Stroke or Heart attack) on touchscreen questionnaire at baseline"
  )
}

# MACE at baseline (HES + VI + TQ)
TEU_MACE_prev <- function(){
  list(
    name = 'TEU_MACE_prev',
    source = c('TEU_HES_MACE_prev','TEU_VeI_MACE_nonc','TEU_VeI_MACE_op','TEU_HMH_MACE_prev'),
    mapper = function(data){
      y<-factor(apply(data,1, function(x) any(x %in% 'Yes')),levels = c('FALSE','TRUE'),labels = c('No','Yes'))
      return(y)
    },
    post_exclusion = FALSE,
    display_name = 'MACE status at baseline',
    description = 'MACE status at baseline identified from HES, Verbal Interview (VI), Touchscreen (TQ)'
  )
}


# Admin censoring date (hospital inpatient)
#https://biobank.ndph.ox.ac.uk/showcase/exinfo.cgi?src=Data_providers_and_dates#:~:text=Censoring%20dates,that%20provider%20is%20mostly%20complete.
# England 31/03/2017, Scotland 31/10/2016, Wales 29/02/2016
Admin_HES_CensorDate<-function(record_level=FALSE){
  list(
    name = 'Admin_HES_CensorDate',
    source = 'TEU_Rec_Country',
    mapper = function(x){
      if(record_level){
        HES <- read_yaml(file.path(config$data$portal$HES, "censoring.yml")) %>%
          lapply(., FUN=FN_toDate)
        # deaths <- read_yaml(file.path(config$data$portal$deaths, "censoring.yml"))
        # datelist <- lapply(x, FUN = function(z) {min(FN_toDate(HES[[z]]), FN_toDate(deaths[[z]]))})
        # y <- do.call(c, datelist)
      }
      else {
        HES <- read_yaml(file.path(dirname(config$data$database), "censoring.yml"))$HES %>%
          lapply(., FUN=FN_toDate)
      } 
      y <- dplyr::case_when(
        x=='England' ~ HES$England,
        x=='Scotland' ~ HES$Scotland,
        x=='Wales' ~ HES$Wales
      )
      return(y)
    },
    post_exclusion = FALSE,
    display_name = 'Administrative censoring date from UKB HES',
    description = 'Censoring date according to origin of hospital data'
  )
}

# Admin censoring date (Cancer outcome)
# https://biobank.ndph.ox.ac.uk/showcase/exinfo.cgi?src=Data_providers_and_dates
# Note there is no record-level option for cancer registry data
Admin_CaR_CensorDate<-function(){
  list(
    name = 'Admin_CaR_CensorDate',
    source = 'TEU_Rec_Country',
    mapper = function(x){

      CaR <- read_yaml(file.path(dirname(config$data$database), "censoring.yml"))$Cancer %>%
        lapply(., FUN=FN_toDate)
      
      y <- dplyr::case_when(
        x=='England' ~ CaR$England,
        x=='Scotland' ~ CaR$Scotland,
        x=='Wales' ~ CaR$Wales
      )
      return(y)
    },
    post_exclusion = FALSE,
    display_name = 'Administrative censoring date for cancer registries data',
    description = 'Administrative censoring date for cancer registries data by country, using assessment centre as a proxy for country'
  )
}

# Admin censoring date (Death registry)
# https://biobank.ndph.ox.ac.uk/showcase/exinfo.cgi?src=Data_providers_and_dates

Admin_Dth_CensorDate<-function(record_level=FALSE){
  list(
    name = 'Admin_Dth_CensorDate',
    source = 'TEU_Rec_Country',
    mapper = function(x){
      if(record_level){
        deaths <- read_yaml(file.path(config$data$portal$deaths, "censoring.yml")) %>%
          lapply(., FUN=FN_toDate)
      } else {
        deaths <- read_yaml(file.path(dirname(config$data$database), "censoring.yml"))$Death %>%
          lapply(., FUN=FN_toDate)
      }
      
      y <- dplyr::case_when(
        x=='England' ~ deaths$England,
        x=='Scotland' ~ deaths$Scotland,
        x=='Wales' ~ deaths$Wales
      )
      return(y)
    },
    post_exclusion = FALSE,
    display_name = 'Administrative censoring date for death registry data',
    description = 'Administrative censoring date for death registries data by country, using assessment centre as a proxy for country'
  )
}

Admin_CensorDate <- function(sources=c("HES", "Dth", "CaR")){
  list(
    name = 'Admin_CensorDate',
    source = glue("Admin_{sources}_CensorDate"),
    mapper = function(data){
      y <- do.call(pmin, data[,glue("Admin_{sources}_CensorDate")])
      return(y)
    },
    post_exclusion = FALSE,
    display_name = 'Administrative censoring date',
    description = 'Administrative censoring date, taken as the minimum of (death censoring and the maximum of (cancer registry and HES censoring))'
  )
}


# Lost to follow-up
BaC_LostFUDate<-function(){
  list(
    name = 'BaC_LostFUDate',
    source = 'BaC_DateLostFU.0.0',
    mapper = FN_toDate,
    post_exclusion = FALSE,
    display_name = 'Date lost to follow-up',
    description = 'Date lost to follow-up'
  )
}


TEU_BreastCancer <- function() {
  code_list <- list(ICD10="C50", ICD9="174")
  type <- "prevalent"
  
  list(
    list(
      name = "BrCaDate_Prevalent",
      source = c("ID", "Rec_DateAssess",
                 paste0("CaR_DiagDate.", seq(0, 16, by=1), ".0"),
                 paste0("CaR_DiagICD10.", seq(0, 16, by=1), ".0"),
                 paste0("CaR_DiagICD9.", seq(0, 14, by=1), ".0")),
      mapper = FN_Cancer(colgroup="CaR_", not_na="DiagICD", 
                         code_list=code_list, type="prevalent", keepcol="DiagDate"),
      post_exclusion = FALSE,
      display_name = "Prevalent BrCa dx date",
      description = "Date of first breast cancer diagnosis before baseline assessment, from cancer registries"
    ),
    list(
      name = "BrCaDx_Prevalent",
      source = c("ID", "Rec_DateAssess",
                 paste0("CaR_DiagDate.", seq(0, 16, by=1), ".0"),
                 paste0("CaR_DiagICD10.", seq(0, 16, by=1), ".0"),
                 paste0("CaR_DiagICD9.", seq(0, 14, by=1), ".0")),
      mapper = function(data) {
        codingICD10 <- read.csv(file.path(config$cleaning$coding, "coding19_flat_Icd10.csv"))
        ICD10 <- FN_Cancer(colgroup="CaR_", not_na="DiagICD", 
                           code_list=code_list, type="prevalent", keepcol="DiagICD10")(data)
        meaningICD10 <- codingICD10$meaning[match(ICD10, codingICD10$Code)]
        
        codingICD9 <- read.csv(file.path(config$cleaning$coding, "coding87_flat_Icd9.csv"))
        ICD9 <- FN_Cancer(colgroup="CaR_", not_na="DiagICD", 
                          code_list=code_list, type="prevalent", keepcol="DiagICD9")(data)
        meaningICD9 <- codingICD9$meaning[match(ICD9, codingICD9$Code)]
        
        y <- coalesce(meaningICD10, meaningICD9)
        return(y)
      },
      post_exclusion = FALSE,
      display_name = "Prevalent BrCa dx",
      description = "Type of first breast cancer diagnosis before baseline assessment, from cancer registries"
    ),
    list(
      name = "BrCaDate_Incident",
      source = c("ID", "Rec_DateAssess", "TEU_HES_BrCa_incdate",
                 paste0("CaR_DiagDate.", seq(0, 16, by=1), ".0"),
                 paste0("CaR_DiagICD10.", seq(0, 16, by=1), ".0"),
                 paste0("CaR_DiagICD9.", seq(0, 14, by=1), ".0")),
      mapper = function(data){
        CaR <- FN_Cancer(colgroup="CaR_", not_na="DiagICD", 
                         code_list=code_list, type="incident", keepcol="DiagDate")(data)
        y <- coalesce(CaR, data$TEU_HES_BrCa_incdate)
        },
      post_exclusion = FALSE,
      display_name = "Incident BrCa dx date",
      description = "Date of first breast cancer diagnosis after baseline assessment, from cancer registries"
    ),
    list(
      name = "BrCaDx_Incident",
      source = c("ID", "Rec_DateAssess", "TEU_HES_BrCa_inc", "TEU_HES_BrCa_incdate", 
                 paste0("CaR_DiagDate.", seq(0, 16, by=1), ".0"),
                 paste0("CaR_DiagICD10.", seq(0, 16, by=1), ".0"),
                 paste0("CaR_DiagICD9.", seq(0, 14, by=1), ".0")),
      mapper = function(data) {
        codingICD10 <- read.csv(file.path(config$cleaning$coding, "coding19_flat_Icd10.csv"))
        ICD10 <- FN_Cancer(colgroup="CaR_", not_na="DiagICD", 
                           code_list=code_list, type="incident", keepcol="DiagICD10")(data)
        meaningICD10 <- codingICD10$meaning[match(ICD10, codingICD10$Code)]
        
        codingICD9 <- read.csv(file.path(config$cleaning$coding, "coding87_flat_Icd9.csv"))
        ICD9 <- FN_Cancer(colgroup="CaR_", not_na="DiagICD", 
                          code_list=code_list, type="incident", keepcol="DiagICD9")(data)
        meaningICD9 <- codingICD9$meaning[match(ICD9, codingICD9$Code)]
        
        CaR <- coalesce(meaningICD10, meaningICD9)
        y <- coalesce(CaR, data$TEU_HES_BrCa_inc)
        return(y)
      },
      post_exclusion = FALSE,
      display_name = "Incident BrCa dx",
      description = "Type of first breast cancer diagnosis after baseline assessment, from cancer registries"
    ),
    list(
      name = "BrCaInSitu_Prevalent",
      source = c("ID", "Rec_DateAssess",
                 paste0("CaR_DiagDate.", seq(0, 16, by=1), ".0"),
                 paste0("CaR_DiagICD10.", seq(0, 16, by=1), ".0"),
                 paste0("CaR_DiagICD9.", seq(0, 14, by=1), ".0")),
      mapper = FN_Cancer(colgroup="CaR_", not_na="DiagICD", 
                         code_list=list(ICD10="D05", ICD9="2330"), type="prevalent", keepcol="DiagDate"),
      post_exclusion = FALSE,
      display_name = "Prevalent breast carcinoma in situ dx date",
      description = "Date of first breast carcinoma in situ diagnosis before baseline assessment, from cancer registries"
    ),
    list(
      name = "BrCaInSitu_Incident",
      source = c("ID", "Rec_DateAssess",
                 paste0("CaR_DiagDate.", seq(0, 16, by=1), ".0"),
                 paste0("CaR_DiagICD10.", seq(0, 16, by=1), ".0"),
                 paste0("CaR_DiagICD9.", seq(0, 14, by=1), ".0")),
      mapper = FN_Cancer(colgroup="CaR_", not_na="DiagICD", 
                         code_list=list(ICD10="D05", ICD9="2330"), type="incident", keepcol="DiagDate"),
      post_exclusion = FALSE,
      display_name = "Incident breast carcinoma in situ dx date",
      description = "Date of first breast carcinoma in situ diagnosis after baseline assessment, from cancer registries"
    )
  )
}

# Other cause dth date
TEU_Dth_NotBrCa_dthdate <-function(record_level=FALSE, ICD10_codes, exclude=FALSE){
  list(
    name = 'TEU_Dth_NotBrCa_dthdate',
    source = if(record_level){c("ID")} else {c('ID',"Dth_ICD10Underlying.0.0", "Dth_ICD10Underlying.1.0","Dth_Date.0.0", "Dth_Date.1.0")},
    mapper = function(data){
      
      y<-FN_Dth_filtercodes(ICD10_codes = ICD10_codes,return_label = 'dth_date', record_level=record_level, exclude=exclude)(data)
      return(y)
    },
    post_exclusion = FALSE,
    display_name = 'Non BrCa death date',
    description = 'Death date caused by non BrCa from Death Registry data'
  )
}


# BrCa censoring date (Based on Death date by non Breast Cancer + Admin censoring date + lost to follow-up + Incident Mastectomy)
BrCa_censordate<-function(){
  list(
    name = 'BrCa_censordate',
    source = c('TEU_Dth_NotBrCa_dthdate','Admin_CensorDate','BaC_LostFUDate','TEU_HES_Mast_incdate'),
    mapper = function(data){
      y<-pmin(data$TEU_Dth_NotBrCa_dthdate,data$Admin_CensorDate,data$BaC_LostFUDate,data$TEU_HES_Mast_incdate,
              na.rm = TRUE)
      return(y)
    },
    post_exclusion = FALSE,
    display_name = 'BrCa censoring date',
    description = 'Censoring date for BrCa outcome'
  )
}

# BrCa event status (0=censored 1=BrCa event)
TEU_BrCa_status<-function(){
  list(
    name = 'TEU_BrCa_status',
    source = c('BrCa_censordate','BrCaDate_Incident'),
    mapper = function(data){
      # Check if censoring date has NA
      if (anyNA(data$BrCa_censordate)==TRUE){
        warning('Missing Censoring Date: Need to double check!')
      }
      data<-data%>%
        mutate(status=case_when(
          !is.na(BrCaDate_Incident) & BrCaDate_Incident<=BrCa_censordate ~ 1,
          is.na(BrCaDate_Incident) |(!is.na(BrCaDate_Incident)&BrCaDate_Incident>BrCa_censordate) ~ 0))
      
      return(data$status)
      
    },
    post_exclusion = FALSE,
    display_name = 'BrCa event status',
    description = 'Event status of BrCa (0=censored, 1=BrCa event)'
    
  )
}

# BrCa follow-up time
TEU_BrCa_time<-function(){
  list(
    name = 'TEU_BrCa_time',
    source = c('TEU_BrCa_status','BrCa_censordate','BrCaDate_Incident','Rec_DateAssess'),
    mapper = function(data){
      
      data=data %>%
        mutate(
          time=case_when(
            TEU_BrCa_status==0 ~ as.numeric(difftime(BrCa_censordate, Rec_DateAssess, unit='days'))/365.25,
            TEU_BrCa_status==1 ~ as.numeric(difftime(BrCaDate_Incident, Rec_DateAssess, unit='days'))/365.25
            )
          ) %>%
        mutate(
          time=ifelse(time < 0, 0, time)
        )
      
      return(data$time)
      
    },
    post_exclusion = FALSE,
    display_name = 'BrCa follow up time',
    description = 'If event status=0, this fields returns time difference in years between censoring date and baseline date.
    If event status=1, this fields returns time to BrCa event.'
  )
}

# 1. HES source 
TEU_HES_BrCa_inc<-function(record_level=FALSE){
  list(
    name='TEU_HES_BrCa_inc',
    source=if(record_level){
      c("ID","Rec_DateAssess", "Admin_CaR_CensorDate")
    } else {
      c("ID", "Rec_DateAssess", "Admin_CaR_CensorDate", 
        paste0("HES_ICD9Diag.0.", seq(0, 46, by=1)),
        paste0("HES_ICD9DateFirst.0.",seq(0,46,by=1)),
        paste0("HES_ICD10Diag.0.", seq(0, 212, by=1)),
        paste0("HES_ICD10DateFirst.0.",seq(0,212,by=1))#,
        #paste0("HES_OPCS4Code.0.",seq(0,116,by=1)),
        #paste0("HES_OPCS4DateFirst.0.",seq(0,116,by=1))
        )
    },
    mapper=function(data){
      # Only interested in HES dx after cancer registry censoring
      data <- data %>% mutate(Rec_DateAssess = Admin_CaR_CensorDate)
      y <- FN_HES_First(
        ICD9_xlsx = file.path(config$cleaning$mapping, 'BrstCancer', 'HES_ICD9_Mapping_20210707.xlsx'),
        ICD10_xlsx = file.path(config$cleaning$mapping, 'BrstCancer', 'HES_ICD10_Mapping_20210707.xlsx'),
        #OPCS4_xlsx = file.path(config$cleaning$mapping,'HES_OPCS4_Mapping_20210707.xlsx'),
        condition = 'BrCa',
        return_label = 'followup',
        record_level = record_level)(data)
      return(y)
      },
    post_exclusion=FALSE,
    display_name='Breast cancer in HES',
    description='Breast cancer status identified from HES (ICD-9, ICD-10) data after baseline'
  )
}

TEU_HES_BrCa_incdate<-function(record_level=FALSE){
  list(
    name='TEU_HES_BrCa_incdate',
    source=if(record_level){
      c("ID","Rec_DateAssess", "Admin_CaR_CensorDate")
    } else {
      c("ID", "Rec_DateAssess", "Admin_CaR_CensorDate",
        paste0("HES_ICD9Diag.0.", seq(0, 46, by=1)),
        paste0("HES_ICD9DateFirst.0.",seq(0,46,by=1)),
        paste0("HES_ICD10Diag.0.", seq(0, 212, by=1)),
        paste0("HES_ICD10DateFirst.0.",seq(0,212,by=1))#,
        #paste0("HES_OPCS4Code.0.",seq(0,116,by=1)),
        #paste0("HES_OPCS4DateFirst.0.",seq(0,116,by=1))
        )
    },
    mapper=function(data){
      # Only interested in HES dx after cancer registry censoring
      data <- data %>% mutate(Rec_DateAssess = Admin_CaR_CensorDate)
      y <- FN_HES_First(
        ICD9_xlsx = file.path(config$cleaning$mapping, 'BrstCancer', 'HES_ICD9_Mapping_20210707.xlsx'),
        ICD10_xlsx = file.path(config$cleaning$mapping, 'BrstCancer', 'HES_ICD10_Mapping_20210707.xlsx'),
        #OPCS4_xlsx = file.path(config$cleaning$mapping,'HES_OPCS4_Mapping_20210707.xlsx'),
        condition = 'BrCa',
        return_label = 'followup_date',
        record_level = record_level)(data)
      return(y)
      },
    post_exclusion=FALSE,
    display_name='Date of breast cancer in HES',
    description='Date of breast cancer identified from HES (ICD-9, ICD-10) data after baseline'
  )
}


# Mastectomy

TEU_Mastectomy<-function(record_level=FALSE){
  list(
    list(
      name='TEU_HES_Mast_prev',
      source=if(record_level){
        c("ID","Rec_DateAssess")
      } else {
        c("ID", "Rec_DateAssess",
          #paste0("HES_ICD9Diag.0.", seq(0, 46, by=1)),
          #paste0("HES_ICD9DateFirst.0.",seq(0,46,by=1)),
          #paste0("HES_ICD10Diag.0.", seq(0, 212, by=1)),
          #paste0("HES_ICD10DateFirst.0.",seq(0,212,by=1)),
          paste0("HES_OPCS4Code.0.",seq(0,116,by=1)),
          paste0("HES_OPCS4DateFirst.0.",seq(0,116,by=1)))
      },
      mapper=FN_HES_First(#ICD9_xlsx = file.path(config$cleaning$mapping,'HES_ICD9_Mapping_20210128.xlsx'),
        #ICD10_xlsx = file.path(config$cleaning$mapping,'HES_ICD10_Mapping_20210707.xlsx'),
        OPCS4_xlsx = file.path(config$cleaning$mapping, 'BrstCancer', 'HES_OPCS4_Mapping_20210707.xlsx'),
        condition = 'BrCa',
        return_label = 'baseline',
        record_level = record_level),
      post_exclusion=FALSE,
      display_name='Mastectomy status identified from HES data prior to or at baseline',
      description='Mastectomy status identified from HES (OPCS-4) data prior to or at baseline'
    ),
    list(
      name='TEU_HES_Mast_prevtype',
      source=if(record_level){
        c("ID","Rec_DateAssess")
      } else {
        c("ID", "Rec_DateAssess",
          #paste0("HES_ICD9Diag.0.", seq(0, 46, by=1)),
          #paste0("HES_ICD9DateFirst.0.",seq(0,46,by=1)),
          #paste0("HES_ICD10Diag.0.", seq(0, 212, by=1)),
          #paste0("HES_ICD10DateFirst.0.",seq(0,212,by=1)),
          paste0("HES_OPCS4Code.0.",seq(0,116,by=1)),
          paste0("HES_OPCS4DateFirst.0.",seq(0,116,by=1)))
      },
      mapper=FN_HES_First(#ICD9_xlsx = file.path(config$cleaning$mapping,'HES_ICD9_Mapping_20210128.xlsx'),
        #ICD10_xlsx = file.path(config$cleaning$mapping,'HES_ICD10_Mapping_20210707.xlsx'),
        OPCS4_xlsx = file.path(config$cleaning$mapping, 'BrstCancer', 'HES_OPCS4_Mapping_20210707.xlsx'),
        condition = 'BrCa',
        return_label = 'baseline_comp',
        record_level = record_level),
      post_exclusion=FALSE,
      display_name='Mastectomy status identified from HES data prior to or at baseline',
      description='Mastectomy status identified from HES (OPCS-4) data prior to or at baseline'
    ),
    list(
      name='TEU_HES_Mast_prevdate',
      source=if(record_level){
        c("ID","Rec_DateAssess")
      } else {
        c("ID", "Rec_DateAssess",
          #paste0("HES_ICD9Diag.0.", seq(0, 46, by=1)),
          #paste0("HES_ICD9DateFirst.0.",seq(0,46,by=1)),
          #paste0("HES_ICD10Diag.0.", seq(0, 212, by=1)),
          #paste0("HES_ICD10DateFirst.0.",seq(0,212,by=1)),
          paste0("HES_OPCS4Code.0.",seq(0,116,by=1)),
          paste0("HES_OPCS4DateFirst.0.",seq(0,116,by=1)))
      },
      mapper=FN_HES_First(#ICD9_xlsx = file.path(config$cleaning$mapping,'HES_ICD9_Mapping_20210128.xlsx'),
        #ICD10_xlsx = file.path(config$cleaning$mapping,'HES_ICD10_Mapping_20210707.xlsx'),
        OPCS4_xlsx = file.path(config$cleaning$mapping, 'BrstCancer', 'HES_OPCS4_Mapping_20210707.xlsx'),
        condition = 'BrCa',
        return_label = 'baseline_date',
        record_level = record_level),
      post_exclusion=FALSE,
      display_name='Mastectomy status identified from HES data prior to or at baseline',
      description='Mastectomy status identified from HES (OPCS-4) data prior to or at baseline'
    ),
    list(
      name='TEU_HES_Mast_inc',
      source=if(record_level){
        c("ID","Rec_DateAssess")
      } else {
        c("ID", "Rec_DateAssess",
          #paste0("HES_ICD9Diag.0.", seq(0, 46, by=1)),
          #paste0("HES_ICD9DateFirst.0.",seq(0,46,by=1)),
          #paste0("HES_ICD10Diag.0.", seq(0, 212, by=1)),
          #paste0("HES_ICD10DateFirst.0.",seq(0,212,by=1)),
          paste0("HES_OPCS4Code.0.",seq(0,116,by=1)),
          paste0("HES_OPCS4DateFirst.0.",seq(0,116,by=1)))
      },
      mapper=FN_HES_First(#ICD9_xlsx = file.path(config$cleaning$mapping,'HES_ICD9_Mapping_20210128.xlsx'),
        #ICD10_xlsx = file.path(config$cleaning$mapping,'HES_ICD10_Mapping_20210707.xlsx'),
        OPCS4_xlsx = file.path(config$cleaning$mapping, 'BrstCancer', 'HES_OPCS4_Mapping_20210707.xlsx'),
        condition = 'BrCa',
        return_label = 'followup',
        record_level = record_level),
      post_exclusion=FALSE,
      display_name='Mastectomy status identified from HES data after baseline',
      description='Mastectomy status identified from HES (OPCS-4) data after baseline'
    ),
    list(
      name='TEU_HES_Mast_incdate',
      source=if(record_level){
        c("ID","Rec_DateAssess")
      } else {
        c("ID", "Rec_DateAssess",
          #paste0("HES_ICD9Diag.0.", seq(0, 46, by=1)),
          #paste0("HES_ICD9DateFirst.0.",seq(0,46,by=1)),
          #paste0("HES_ICD10Diag.0.", seq(0, 212, by=1)),
          #paste0("HES_ICD10DateFirst.0.",seq(0,212,by=1)),
          paste0("HES_OPCS4Code.0.",seq(0,116,by=1)),
          paste0("HES_OPCS4DateFirst.0.",seq(0,116,by=1)))
      },
      mapper=FN_HES_First(#ICD9_xlsx = file.path(config$cleaning$mapping,'HES_ICD9_Mapping_20210128.xlsx'),
        #ICD10_xlsx = file.path(config$cleaning$mapping,'HES_ICD10_Mapping_20210707.xlsx'),
        OPCS4_xlsx = file.path(config$cleaning$mapping, 'BrstCancer', 'HES_OPCS4_Mapping_20210707.xlsx'),
        condition = 'BrCa',
        return_label = 'followup_date',
        record_level = record_level),
      post_exclusion=FALSE,
      display_name='Mastectomy date from HES data after baseline',
      description='Mastectomy date from HES (OPCS-4) data after baseline'
    )
  )
}


# XL: Added Admin_HES_CensorDate at the back because we are using operation data from HES.
Admin_CensorDate_BrCaHES <- function(){
  list(
    name = 'Admin_CensorDate',
    source = c("Admin_HES_CensorDate", "Admin_Dth_CensorDate", "Admin_CaR_CensorDate"),
    mapper = function(data){
      y <- pmin(data$Admin_Dth_CensorDate, pmax(data$Admin_CaR_CensorDate, data$Admin_HES_CensorDate))
      return(y)
    },
    post_exclusion = FALSE,
    display_name = 'Administrative censoring date',
    description = 'Administrative censoring date, taken as the minimum of (death censoring and the maximum of (cancer registry and HES censoring))'
  )
}


# Dementia/Alzheimers

TEU_Dementia <- function(record_level=FALSE){
  list(
    list(
      name='TEU_HES_Alz_prev',
      source=if(record_level){
        c("ID","Rec_DateAssess")
      } else {
        c("ID", "Rec_DateAssess",
          paste0("HES_ICD9Diag.0.", seq(0, 46, by=1)),
          paste0("HES_ICD9DateFirst.0.",seq(0,46,by=1)),
          paste0("HES_ICD10Diag.0.", seq(0, 212, by=1)),
          paste0("HES_ICD10DateFirst.0.",seq(0,212,by=1))#,
          # paste0("HES_OPCS4Code.0.",seq(0,116,by=1)),
          # paste0("HES_OPCS4DateFirst.0.",seq(0,116,by=1))
          )
      },
      mapper=FN_HES_First(ICD9_xlsx = file.path(config$cleaning$mapping, 'Dementia', 'HES_ICD9_Mapping_20211006.xlsx'),
        ICD10_xlsx = file.path(config$cleaning$mapping,'Dementia','HES_ICD10_Mapping_20211006.xlsx'),
        #OPCS4_xlsx = file.path(config$cleaning$mapping, 'BrstCancer', 'HES_OPCS4_Mapping_20210707.xlsx'),
        condition = 'Dementia',
        return_label = 'baseline',
        record_level = record_level),
      post_exclusion=FALSE,
      display_name='Dementia status identified from HES data prior to or at baseline',
      description='Dementia status identified from HES data prior to or at baseline'
    ),
    list(
      name='TEU_HES_Alz_prevdate',
      source=if(record_level){
        c("ID","Rec_DateAssess")
      } else {
        c("ID", "Rec_DateAssess",
          paste0("HES_ICD9Diag.0.", seq(0, 46, by=1)),
          paste0("HES_ICD9DateFirst.0.",seq(0,46,by=1)),
          paste0("HES_ICD10Diag.0.", seq(0, 212, by=1)),
          paste0("HES_ICD10DateFirst.0.",seq(0,212,by=1))#,
          #paste0("HES_OPCS4Code.0.",seq(0,116,by=1)),
          #paste0("HES_OPCS4DateFirst.0.",seq(0,116,by=1))
          )
      },
      mapper=FN_HES_First(ICD9_xlsx = file.path(config$cleaning$mapping,'Dementia','HES_ICD9_Mapping_20211006.xlsx'),
        ICD10_xlsx = file.path(config$cleaning$mapping,'Dementia','HES_ICD10_Mapping_20211006.xlsx'),
        #OPCS4_xlsx = file.path(config$cleaning$mapping, 'BrstCancer', 'HES_OPCS4_Mapping_20210707.xlsx'),
        condition = 'Dementia',
        return_label = 'baseline_date',
        record_level = record_level),
      post_exclusion=FALSE,
      display_name='Dementia status identified from HES data prior to or at baseline',
      description='Dementia status identified from HES data prior to or at baseline'
    ),
    list(
      name='TEU_HES_Alz_inc',
      source=if(record_level){
        c("ID","Rec_DateAssess")
      } else {
        c("ID", "Rec_DateAssess",
          paste0("HES_ICD9Diag.0.", seq(0, 46, by=1)),
          paste0("HES_ICD9DateFirst.0.",seq(0,46,by=1)),
          paste0("HES_ICD10Diag.0.", seq(0, 212, by=1)),
          paste0("HES_ICD10DateFirst.0.",seq(0,212,by=1))#,
          #paste0("HES_OPCS4Code.0.",seq(0,116,by=1)),
          #paste0("HES_OPCS4DateFirst.0.",seq(0,116,by=1))
        )
      },
      mapper=FN_HES_First(ICD9_xlsx = file.path(config$cleaning$mapping,'Dementia','HES_ICD9_Mapping_20211006.xlsx'),
        ICD10_xlsx = file.path(config$cleaning$mapping,'Dementia','HES_ICD10_Mapping_20211006.xlsx'),
        #OPCS4_xlsx = file.path(config$cleaning$mapping, 'BrstCancer', 'HES_OPCS4_Mapping_20210707.xlsx'),
        condition = 'Dementia',
        return_label = 'followup',
        record_level = record_level),
      post_exclusion=FALSE,
      display_name='Dementia status identified from HES data after baseline',
      description='Dementia status identified from HES data after baseline'
    ),
    list(
      name='TEU_HES_Alz_incdate',
      source=if(record_level){
        c("ID","Rec_DateAssess")
      } else {
        c("ID", "Rec_DateAssess",
          paste0("HES_ICD9Diag.0.", seq(0, 46, by=1)),
          paste0("HES_ICD9DateFirst.0.",seq(0,46,by=1)),
          paste0("HES_ICD10Diag.0.", seq(0, 212, by=1)),
          paste0("HES_ICD10DateFirst.0.",seq(0,212,by=1))#,
          #paste0("HES_OPCS4Code.0.",seq(0,116,by=1)),
          #paste0("HES_OPCS4DateFirst.0.",seq(0,116,by=1))
        )
      },
      mapper=FN_HES_First(ICD9_xlsx = file.path(config$cleaning$mapping,'Dementia','HES_ICD9_Mapping_20211006.xlsx'),
        ICD10_xlsx = file.path(config$cleaning$mapping,'Dementia','HES_ICD10_Mapping_20211006.xlsx'),
        #OPCS4_xlsx = file.path(config$cleaning$mapping, 'BrstCancer', 'HES_OPCS4_Mapping_20210707.xlsx'),
        condition = 'Dementia',
        return_label = 'followup_date',
        record_level = record_level),
      post_exclusion=FALSE,
      display_name='Dementia date from HES data after baseline',
      description='Dementia date from HES data after baseline'
    ),
    list(
      name='TEU_HES_Alz_inctype',
      source=if(record_level){
        c("ID","Rec_DateAssess")
      } else {
        c("ID", "Rec_DateAssess",
           paste0("HES_ICD9Diag.0.", seq(0, 46, by=1)),
           paste0("HES_ICD9DateFirst.0.",seq(0,46,by=1)),
           paste0("HES_ICD10Diag.0.", seq(0, 212, by=1)),
           paste0("HES_ICD10DateFirst.0.",seq(0,212,by=1))#,
           #paste0("HES_OPCS4Code.0.",seq(0,116,by=1)),
           #paste0("HES_OPCS4DateFirst.0.",seq(0,116,by=1))
        )
      },
      mapper=FN_HES_First(ICD9_xlsx = file.path(config$cleaning$mapping,'Dementia','HES_ICD9_Mapping_20211006.xlsx'),
        ICD10_xlsx = file.path(config$cleaning$mapping,'Dementia','HES_ICD10_Mapping_20211006.xlsx'),
        #OPCS4_xlsx = file.path(config$cleaning$mapping, 'BrstCancer', 'HES_OPCS4_Mapping_20210707.xlsx'),
        condition = 'Dementia',
        return_label = 'followup_comp',
        record_level = record_level),
      post_exclusion=FALSE,
      display_name='Dementia type from HES data after baseline',
      description='Dementia type from HES data after baseline'
    ),
    list(
      name = "TEU_Alz_Dthdate",
      source = if(record_level){c("ID")} else {c('ID',"Dth_ICD10Underlying.0.0", "Dth_ICD10Underlying.1.0","Dth_Date.0.0", "Dth_Date.1.0")},
      mapper = function(data){
        mapping <- read.xlsx_kdrive(file.path(config$cleaning$mapping,'Dementia','HES_ICD10_Mapping_20211006.xlsx'),
                                    col_types=c('text')) %>%
          filter(Conditions=="Dementia")
        y<-FN_Dth_filtercodes(ICD10_codes = mapping$Code,return_label = 'dth_date', record_level=record_level, exclude=FALSE)(data)
        return(y)
      },
      post_exclusion = FALSE,
      display_name = 'Dementia death date',
      description = 'Death date attributed to Dementia from Death Registry data'
    ),
    list(
      name = "TEU_nonAlz_Dthdate",
      source = if(record_level){c("ID")} else {c('ID',"Dth_ICD10Underlying.0.0", "Dth_ICD10Underlying.1.0","Dth_Date.0.0", "Dth_Date.1.0")},
      mapper = function(data){
        mapping <- read.xlsx_kdrive(file.path(config$cleaning$mapping,'Dementia','HES_ICD10_Mapping_20211006.xlsx'),
                                    col_types=c('text')) %>%
          filter(Conditions=="Dementia")
        y<-FN_Dth_filtercodes(ICD10_codes = mapping$Code,return_label = 'dth_date', record_level=record_level, exclude=TRUE)(data)
        return(y)
      },
      post_exclusion = FALSE,
      display_name = 'Dementia death date',
      description = 'Death date attributed to Dementia from Death Registry data'
    ),
    list(
      name = 'TEU_Alz_eventdate',
      source = c('TEU_HES_Alz_incdate','TEU_Alz_Dthdate'),
      mapper = function(data){
        y<-pmin(data$TEU_HES_Alz_incdate,data$TEU_Alz_Dthdate,na.rm = TRUE)
        return(y)
      },
      post_exclusion = FALSE,
      display_name = 'Dementia event date',
      description = 'Dementia event date at follow-up based on HES and Death Registry data'
    ),
    list(
      name = 'TEU_Alz_censordate',
      source = c('TEU_nonAlz_Dthdate', 'Admin_HES_CensorDate', 'Admin_Dth_CensorDate', 'BaC_LostFUDate'),
      mapper = function(data){
        y<-pmin(data$TEU_nonAlz_Dthdate,data$Admin_HES_CensorDate,data$BaC_LostFUDate,na.rm = TRUE)
        return(y)
      },
      post_exclusion = FALSE,
      display_name = 'Dementia censoring date',
      description = 'Censoring date for dementia outcome'
    ),
    list(
      name = "TEU_Alz_Status",
      source = c('TEU_Alz_censordate','TEU_Alz_eventdate'),
      mapper = function(data){
        # Check if censoring date has NA
        if (anyNA(data$TEU_Alz_censordate)==TRUE){
          warning('Missing Censoring Date: Need to double check!')
        }
        data<-data%>%
          mutate(status=case_when(
            !is.na(TEU_Alz_eventdate) & TEU_Alz_eventdate<=TEU_Alz_censordate ~ 1,
            is.na(TEU_Alz_eventdate) |(!is.na(TEU_Alz_eventdate)&TEU_Alz_eventdate>TEU_Alz_censordate) ~ 0))
        
        return(data$status)
      },
      post_exclusion = FALSE,
      display_name = "Dementia status",
      description = "Dementia status (0 = no event, 1 = dementia event)"
    ),
    list(
      name = 'TEU_Alz_time',
      source = c('TEU_Alz_Status','TEU_Alz_censordate','TEU_Alz_eventdate','Rec_DateAssess'),
      mapper = function(data){
        
        data=data%>%
          mutate(time=case_when(
            TEU_Alz_Status==0 ~ as.numeric(difftime(TEU_Alz_censordate, Rec_DateAssess, unit='days')),
            TEU_Alz_Status==1 ~ as.numeric(difftime(TEU_Alz_eventdate, Rec_DateAssess, unit='days'))))
        
        return(data$time)
        
      },
      post_exclusion = FALSE,
      display_name = 'Dementia follow up time',
      description = 'If censoring status=0, this fields returns time difference in days between censoring date and baseline date.
    If censoring status=1, this fields returns time to Dementia event.'
    )
  )
}

# Jennifer Collister
# 30/09/20

# Load the project config file for filepaths etc
if (!exists("config")) {
  library(yaml)
  config = yaml.load_file("config.yml")
}


specs <- function() {
  
  # If you're here to write a new spec, you can run this source line interactively
  # to load all the variable derivation objects into your working environment
  # so you get autocomplete when typing them!
  source(file.path(config$scripts$cleaning, "derivation_objects.R"),
         local = if (sys.nframe() == 0L) {
           FALSE
           } else {
             TEUmaps <- new.env()
             }
         )
  if (exists("TEUmaps")) {
    attach(TEUmaps)
    on.exit(detach(TEUmaps))
  }
  
  # Dataset specifications
  
  TEUvars_common <- list(
    ID,
    BaC_Sex,
    TEU_BaC_DateOfBirth,
    Rec_DateAssess,
    TEU_BaC_AgeAtRec,
    TEU_ethnicgrp,
    TEU_Rec_AssessCentre,
    TEU_Rec_Country
  )
  
  TEUvars_BP <- list(
    ID,
    TEU_BlP_SBP.0.0,
    TEU_BlP_SBP.0.1,
    TEU_BlP_DBP.0.0,
    TEU_BlP_DBP.0.1,
    TEU_BlP_nSBP,
    TEU_BlP_nDBP,
    TEU_BlP_SBP.avg,
    TEU_BlP_DBP.avg,
    GeP_ethnic
  )

  
  UKB_genetic <- list(
    ID,
    GeP_UsedInPCA, # Identifies participants which met UKB QC for inclusion in PCA
    GeP_Outliers, # Identifies participants who are outliers for missingness and heterozygosity
    GeP_Kinship,
    GeP_ethnic, # Identifies participants with genetic White British ancestry
    GeP_Array, # We should adjust our PRS analyses by array
    GeP_Batch, # We may wish to adjust for batch effect
    # GeP_Plate, # We may wish to adjust for plate effect
    GeP_PC(pc=1),
    GeP_PC(pc=2),
    GeP_PC(pc=3),
    GeP_PC(pc=4),
    GeP_PC(pc=5),
    GeP_PC(pc=6),
    GeP_PC(pc=7),
    GeP_PC(pc=8),
    GeP_PC(pc=9),
    GeP_PC(pc=10), # Genetic Principal Components of ancestry
    GeP_Sex, # Used to check for sex discordance
    BaC_Sex # Used to check for sex discordance
  )
  
  MACE_recordlevel <- c(
    TEUvars_common,
    list(
      # MACE at baseline
      TEU_HES_MACE_prev(record_level=TRUE),
      TEU_VeI_MACE_nonc,
      TEU_VeI_MACE_op,
      TEU_HMH_MACE_prev,
      TEU_MACE_prev
    )
    
  )
  
  HTN_control <- c(
    TEUvars_common,
    TEUvars_BP,
    list(
      TEU_BaC_AgeCat,
      TEU_SBP_PRS
    )
  )


  HTN_control_MACE <- c(
    HTN_control,
    UKB_genetic,
    MACE_recordlevel,
    GeP_Array
  )
  
    
  BrCa_PRS <- c(
    TEUvars_common,
    TEU_BreastCancer(),
    TEU_Mastectomy(record_level=TRUE),
    UKB_genetic,
    list(
      Admin_HES_CensorDate(record_level=TRUE),
      Admin_CaR_CensorDate,
      Admin_Dth_CensorDate(record_level=TRUE),
      Admin_CensorDate_BrCaHES(),
      BaC_LostFUDate,
      TEU_BaC_AgeCat,
      TEU_Dth_NotBrCa_dthdate(record_level=TRUE, ICD10_codes=paste0("C50", seq(1, 9, by=1)), exclude=TRUE),
      BrCa_censordate,
      TEU_BrCa_status,
      TEU_BrCa_time,
      TEU_BrCa_313_PRS,
      TEU_BrCa_100k_PRS,
      TEU_HES_BrCa_inc(record_level=TRUE),
      TEU_HES_BrCa_incdate(record_level=TRUE)
    )
  )  
  
  PRS_agreement <- c(
    BrCa_PRS,
    HTN_control_MACE,
    TEU_SBP_PRS_Warren,
    TEU_DementiaAlz_PRS(),
    TEU_Dementia(record_level=TRUE)
  )
  
 
  return(environment())
}

TEU_SPECS <- specs()

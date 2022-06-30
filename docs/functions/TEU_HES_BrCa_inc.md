This file has been automatically generated during the data derivation process.
It contains the function used to derive the selected data field, and should be viewed in conjunction with the data dictionary html.
It will not run on its own.


```
TEU_HES_BrCa_inc <- function(data){
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
      }
```



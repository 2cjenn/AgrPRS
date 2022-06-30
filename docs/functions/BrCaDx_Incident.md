This file has been automatically generated during the data derivation process.
It contains the function used to derive the selected data field, and should be viewed in conjunction with the data dictionary html.
It will not run on its own.


```
BrCaDx_Incident <- function(data) {
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
      }
```



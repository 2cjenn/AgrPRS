This file has been automatically generated during the data derivation process.
It contains the function used to derive the selected data field, and should be viewed in conjunction with the data dictionary html.
It will not run on its own.


```
TEU_BrCa_status <- function(data){
      # Check if censoring date has NA
      if (anyNA(data$BrCa_censordate)==TRUE){
        warning('Missing Censoring Date: Need to double check!')
      }
      data<-data%>%
        mutate(status=case_when(
          !is.na(BrCaDate_Incident) & BrCaDate_Incident<=BrCa_censordate ~ 1,
          is.na(BrCaDate_Incident) |(!is.na(BrCaDate_Incident)&BrCaDate_Incident>BrCa_censordate) ~ 0))
      
      return(data$status)
      
    }
```



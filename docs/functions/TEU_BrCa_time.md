This file has been automatically generated during the data derivation process.
It contains the function used to derive the selected data field, and should be viewed in conjunction with the data dictionary html.
It will not run on its own.


```
TEU_BrCa_time <- function(data){
      
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
      
    }
```



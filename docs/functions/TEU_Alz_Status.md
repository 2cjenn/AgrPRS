This file has been automatically generated during the data derivation process.
It contains the function used to derive the selected data field, and should be viewed in conjunction with the data dictionary html.
It will not run on its own.


```
TEU_Alz_Status <- function(data){
        # Check if censoring date has NA
        if (anyNA(data$TEU_Alz_censordate)==TRUE){
          warning('Missing Censoring Date: Need to double check!')
        }
        data<-data%>%
          mutate(status=case_when(
            !is.na(TEU_Alz_eventdate) & TEU_Alz_eventdate<=TEU_Alz_censordate ~ 1,
            is.na(TEU_Alz_eventdate) |(!is.na(TEU_Alz_eventdate)&TEU_Alz_eventdate>TEU_Alz_censordate) ~ 0))
        
        return(data$status)
      }
```



This file has been automatically generated during the data derivation process.
It contains the function used to derive the selected data field, and should be viewed in conjunction with the data dictionary html.
It will not run on its own.


```
TEU_Alz_time <- function(data){
        
        data=data%>%
          mutate(time=case_when(
            TEU_Alz_Status==0 ~ as.numeric(difftime(TEU_Alz_censordate, Rec_DateAssess, unit='days')),
            TEU_Alz_Status==1 ~ as.numeric(difftime(TEU_Alz_eventdate, Rec_DateAssess, unit='days'))))
        
        return(data$time)
        
      }
```



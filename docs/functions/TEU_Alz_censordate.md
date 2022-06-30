This file has been automatically generated during the data derivation process.
It contains the function used to derive the selected data field, and should be viewed in conjunction with the data dictionary html.
It will not run on its own.


```
TEU_Alz_censordate <- function(data){
        y<-pmin(data$TEU_nonAlz_Dthdate,data$Admin_HES_CensorDate,data$BaC_LostFUDate,na.rm = TRUE)
        return(y)
      }
```



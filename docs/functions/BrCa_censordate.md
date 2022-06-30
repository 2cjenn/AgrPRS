This file has been automatically generated during the data derivation process.
It contains the function used to derive the selected data field, and should be viewed in conjunction with the data dictionary html.
It will not run on its own.


```
BrCa_censordate <- function(data){
      y<-pmin(data$TEU_Dth_NotBrCa_dthdate,data$Admin_CensorDate,data$BaC_LostFUDate,data$TEU_HES_Mast_incdate,
              na.rm = TRUE)
      return(y)
    }
```



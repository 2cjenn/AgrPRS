This file has been automatically generated during the data derivation process.
It contains the function used to derive the selected data field, and should be viewed in conjunction with the data dictionary html.
It will not run on its own.


```
TEU_Alz_eventdate <- function(data){
        y<-pmin(data$TEU_HES_Alz_incdate,data$TEU_Alz_Dthdate,na.rm = TRUE)
        return(y)
      }
```



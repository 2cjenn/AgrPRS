This file has been automatically generated during the data derivation process.
It contains the function used to derive the selected data field, and should be viewed in conjunction with the data dictionary html.
It will not run on its own.


```
Admin_CensorDate <- function(data){
      y <- pmin(data$Admin_Dth_CensorDate, pmax(data$Admin_CaR_CensorDate, data$Admin_HES_CensorDate))
      return(y)
    }
```



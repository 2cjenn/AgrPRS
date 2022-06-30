This file has been automatically generated during the data derivation process.
It contains the function used to derive the selected data field, and should be viewed in conjunction with the data dictionary html.
It will not run on its own.


```
Admin_CaR_CensorDate <- function(x){

      CaR <- read_yaml(file.path(dirname(config$data$database), "censoring.yml"))$Cancer %>%
        lapply(., FUN=FN_toDate)
      
      y <- dplyr::case_when(
        x=='England' ~ CaR$England,
        x=='Scotland' ~ CaR$Scotland,
        x=='Wales' ~ CaR$Wales
      )
      return(y)
    }
```



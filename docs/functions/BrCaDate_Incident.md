This file has been automatically generated during the data derivation process.
It contains the function used to derive the selected data field, and should be viewed in conjunction with the data dictionary html.
It will not run on its own.


```
BrCaDate_Incident <- function(data){
        CaR <- FN_Cancer(colgroup="CaR_", not_na="DiagICD", 
                         code_list=code_list, type="incident", keepcol="DiagDate")(data)
        y <- coalesce(CaR, data$TEU_HES_BrCa_incdate)
        }
```



This file has been automatically generated during the data derivation process.
It contains the function used to derive the selected data field, and should be viewed in conjunction with the data dictionary html.
It will not run on its own.


```
TEU_Alz_Dthdate <- function(data){
        mapping <- read.xlsx_kdrive(file.path(config$cleaning$mapping,'Dementia','HES_ICD10_Mapping_20211006.xlsx'),
                                    col_types=c('text')) %>%
          filter(Conditions=="Dementia")
        y<-FN_Dth_filtercodes(ICD10_codes = mapping$Code,return_label = 'dth_date', record_level=record_level, exclude=FALSE)(data)
        return(y)
      }
```



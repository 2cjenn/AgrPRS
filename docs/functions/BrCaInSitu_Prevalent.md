This file has been automatically generated during the data derivation process.
It contains the function used to derive the selected data field, and should be viewed in conjunction with the data dictionary html.
It will not run on its own.


```
BrCaInSitu_Prevalent <- function(data) {
    long <- data %>% pivot_longer(
      cols = starts_with(colgroup),
      names_to = c(".value", "instance"),
      names_pattern = paste0(colgroup, "(.*)\\.(.*).0")
    ) %>%
      filter_at(vars(starts_with(not_na)),any_vars(!is.na(.)))
    
    filtered <- long %>%
      # Keep only cancer of interest
      {
        if(exclude==FALSE){
          filter(., substr(DiagICD10, 1, nchar(code_list$ICD10[1])) %in% code_list$ICD10 | 
                   substr(DiagICD9, 1, nchar(code_list$ICD9[1])) %in% code_list$ICD9)
        } else {
          filter(., !substr(DiagICD10, 1, nchar(code_list$ICD10[1])) %in% code_list$ICD10 & 
                   !substr(DiagICD9, 1, nchar(code_list$ICD9[1])) %in% code_list$ICD9)
        }
      } %>%
      # Convert date field to date type
      mutate(DiagDate = FN_toDate(DiagDate)) %>%
      {
        if(type=="all"){
          .
        } else if (type=="prevalent"){
          filter(., DiagDate <= Rec_DateAssess)
        } else if (type=="incident"){
          filter(., DiagDate > Rec_DateAssess)
        }
      } %>%
      # Keep first diagnosis per individual, prefer ICD10 diagnoses over ICD9
      group_by(ID) %>%
      arrange(DiagDate, DiagICD10, .by_group = TRUE) %>%
      filter(row_number()==1)
    
    y <- filtered[[keepcol]][match(data$ID, filtered$ID)]
    
    return(y)
  }
```



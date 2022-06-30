This file has been automatically generated during the data derivation process.
It contains the function used to derive the selected data field, and should be viewed in conjunction with the data dictionary html.
It will not run on its own.


```
TEU_BaC_AgeCat <- function(x){
    cut(x, breaks=breaks, labels=labels, right=right)
  }
```



# BeautifulLmTables
## Description
Creates a list with a matrix and a latex table with the results from a set of linear regressions (ussing command lm())
## Usage
lmtable(form,datav,namescol,namesrow,stars,wgh,rbst,clst,dec)
## Arguments
- form: list with formulas
- datav: data for regressions
- namescol: tittles of regression
- namesrow: regressors name
- stars: TRUE for stars in the table
- wgh: name of variable used to weight regression
- rbst: TRUE for robust errors 
- clst: name of variable used to cluster errors
- dec: number of decimals in the table
## Value
- $matrix returns a matrix with all the results from the regression
- $latex returns a latex table with all the results from the regression
## Example
```
#Don't run
#lmtable_res<-lmtable(form,datav,namescol,namesrow,stars,wgh,rbst,clst,dec)
#lmtable_res$matrix
#lmtable_res$latex
```

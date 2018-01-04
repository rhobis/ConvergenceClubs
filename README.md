ConvergenceClubs
======================================================

[![CRAN\_Status\_Badge](https://www.r-pkg.org/badges/version/ConvergenceClubs)](https://cran.r-project.org/package=ConvergenceClubs)

Description 
-----------------

ConvergenceClubs provides functions for clustering regions that form convergence clubs, 
according to the definition of Phillips and Sul (2009) <doi:10.1002/jae.1080>.

The main functions are:

- `findClubs()`: finds clubs of convergence, given a dataset with regions in rows and
    years in columns, returning an object of class `convergence.clubs`. 
- `mergeClubs()`: takes as argument an object of class `convergence.clubs` and
    applies the clustering procedure to the convergence clubs contained in the argument,
    according to either Phillips and Sul (2009) or  von Lyncker and Thoennessen (2016) procedure.


Installation
------------

To install the package from CRAN, simply run the following code in *R*:
``` r
install.packages("ConvergenceClubs")
```

Or, if you want to install the development version from GitHub:
``` r
# if not present, install 'devtools' package
install.packages("devtools")
devtools::install_github("rhobis/ConvergenceClubs")
```

Usage
-----

``` r
library(ConvergenceClubs)

data("countryGDP")

# Cluster Countries using GDP from year 2000 to year 2014, with 2014 as reference year
clubs <- findClubs(countryGDP, dataCols=2:35, regions = 1, refCol=35, 
                    time_trim = 1/3, cstar = 0, HACmethod = "AQSB")
summary(clubs)

# Merge clusters using Phillips and Sul (2009) method
mclubs <- mergeClubs(clubs, mergeMethod='PS', mergeDivergent=FALSE)
summary(mclubs)

# Merge clusters using von Lyncker and Thoennessen (2016) method
mclubs <- mergeClubs(clubs, mergeMethod='vLT', mergeDivergent=FALSE)
summary(mclubs)

```

More
----

- Please, report any bug or issue [here](https://github.com/rhobis/ConvergenceClubs/issues).
- For more information, please contact the manteiner at `roberto.sichera@unipa.it`. 

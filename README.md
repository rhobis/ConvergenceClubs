ConvergenceClubs
======================================================

[![Travis-CI Build Status](https://travis-ci.org/rhobis/ConvergenceClubs.svg?branch=master)](https://travis-ci.org/rhobis/ConvergenceClubs)
[![CRAN\_Status\_Badge](https://www.r-pkg.org/badges/version/ConvergenceClubs)](https://cran.r-project.org/package=ConvergenceClubs)
[![](https://cranlogs.r-pkg.org/badges/grand-total/ConvergenceClubs)](https://cran.r-project.org/package=ConvergenceClubs)
[![DOI:10.1007/978-3-319-76207-4_15](https://zenodo.org/badge/DOI/10.32614/RJ-2019-021.svg)](https://doi.org/10.32614/RJ-2019-021)
[![R badge](https://img.shields.io/badge/-Support%20us-brightgreen)](https://www.buymeacoffee.com/rhobis)

Description 
-----------------

ConvergenceClubs provides functions for clustering regions that form convergence clubs, 
according to the definition by Phillips and Sul (2009).

The main functions are:

- `findClubs()`: finds clubs of convergence, given a dataset with regions in rows and
    years in columns, returning an object of class `convergence.clubs`. 
- `mergeClubs()`: takes as argument an object of class `convergence.clubs` and
    applies the clustering procedure to the convergence clubs contained in the argument,
    according to either Phillips and Sul (2009) or  von Lyncker and Thoennessen (2017) procedure.

For class `convergence.clubs`, the following methods are available:

- `summary()` : shows the number of regions for each club of convergence and the number of divergent         regions;
- `print()` : prints main information about the clubs and divergent units in the
            `convergence.clubs` object (unit IDs, beta coefficient, p-value, ...);
- `dim()` : return a vector of two elements, representing the number of clubs and the number of 
    divergent units;
- `plot()` : plots transition path.

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

data("filteredGDP")

# Cluster Countries using GDP from year 1970 to year 2003
clubs <- findClubs(filteredGDP, dataCols=2:35, unit_names = 1, refCol=35,
                   time_trim = 1/3, cstar = 0, HACmethod = "FQSB")
summary(clubs)

# Merge clusters
mclubs <- mergeClubs(clubs, mergeMethod='PS', mergeDivergent=FALSE)
summary(mclubs)

mclubs <- mergeClubs(clubs, mergeMethod='vLT', mergeDivergent=FALSE)
summary(mclubs)


# Plot Transition Paths for all regions in each club and average Transition Path
# for all clubs
plot(mclubs)

# Plot Only average Transition Paths
plot(mclubs, clubs=NULL)
plot(mclubs, clubs=NULL, legend=TRUE)

```

More
----

- Please, report any bug or issue [here](https://github.com/rhobis/ConvergenceClubs/issues).
- For more information, please contact the maintainer at `roberto.sichera@unipa.it`. 

<br/>

<a href="https://www.buymeacoffee.com/rhobis" target="_blank"><img src="https://cdn.buymeacoffee.com/buttons/v2/default-yellow.png" alt="Buy Me A Coffee" style="height: 30px !important;width: 100px !important;" ></a>

<br/>


# racmoR

## Overview
racmoR is a personal package for interacting with and manipulating output from the RACMO regional climate model.
The package is an ongoing work in progress, is not foolproof, and is essentially a lot of syntactic wrapping around the [terra](https://rspatial.org) package to ease repeated processes.

It is necessary to separately download the RACMO data. 
The RACMO2.3p3 data that this package was initially created to handle can be accessed online at [https://doi.org/10.5281/zenodo.7639053] and was created by Christiaan van Dalum at IMAU, Utrecht University.

## Instructions
This package is mainly a personal package, so it is not available on CRAN.
To download this package directly from GitHub, you'll need to use the "devtools" package.
It's easiest to do this within RStudio.

1) Install the [devtools](https://github.com/hadley/devtools) and [terra](https://rspatial.org) packages from CRAN: 
``` R
install.packages("devtools")
install.packages("terra")
```

2) Load the devtools package:
```R
library(devtools)
```

3) Install racmoR directly from GitHub:
```R
devtools::install_github("polarSaunderson/figuR")
```

4) Some of the functions in `racmoR` require my `terrapin` package too.
If you want to use these functions, it is necessary to also install this package:
```R
devtools::install_github("polarSaunderson/terrapin")
```

5) Load the racmoR package
```R
library(racmoR)
```

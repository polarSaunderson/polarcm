# polarcm

## Overview
`polarcm` is a personal package for interacting with and manipulating output 
from the polar regional climate models (RCM) RACMO and MAR. It has mainly been 
developed with RACMO2.3p3 data and (currently) only focuses on Antarctica. It is 
however being actively expanded to handle other RACMO versions and MAR data, and
could one day also be expanded to work with data in Greenland.

**WARNING** 
This package is an ongoing work-in-progress. It should be treated with caution 
as it is being developed, and can be prone to unexpected results when used in 
new situations. Please let me know if you encounter such behaviour.

It is also worth noting that many of the examples are conceptual and are wrapped
in `\dontrun` commands. The documentation will be improved as the package 
develops.

**Dependencies**
This package relies heavily on the [terra](https://rspatial.org) package, and 
another personal package I am developing, called [terrapin](https://github.com/polarSaunderson/terrapin). See the instructions
below for how to install those.

**Important** 
This package **DOES NOT** include the RCM data, nor download it for you. It is
necessary to download the RCM data first. 

The RACMO2.3p3 data can be accessed on Zenodo at 
[https://doi.org/10.5281/zenodo.5512076](). 
See this paper by Dr. Christiaan van Dalum in The Cryosphere for more 
information on RACMO2.3p3 in Antarctica: 
[https://doi.org/10.5194/tc-16-1071-2022](). 

`polarcm` will also work best with the MEaSURES dataset from Mouginot et al. 
(2017).
Version 2 of the dataset can be downloaded here:
[https://doi.org/10.5067/AXE4121732AD]().

## Instructions
This package is currently a personal package, so it is not available on CRAN.
To download this package directly from GitHub, you'll need to use the "devtools" package.
It's easiest to do this within RStudio.

1) Install the [devtools](https://github.com/hadley/devtools) and [terra](https://rspatial.org) packages from CRAN: 
``` R
install.packages("devtools")
install.packages("terra")      # vital for polarcm to work
```

2) Load the devtools package:
```R
library(devtools)
```

3) Install polarcm directly from GitHub:
```R
devtools::install_github("polarSaunderson/polarcm")
```

4) Some of the functions in `polarcm` require my `terrapin` package. If that
was not automatically installed from the previous line of code, it is 
necessary to also install the `terrapin` package manually. `terrapin` is also
under development.
```R
devtools::install_github("polarSaunderson/terrapin")
```

5) Load the polarcm package
```R
library(polarcm)
```

**IMPORTANT**
There is a function named `configure_polarcm` that needs to run at the start of
each R session `polarcm` is used in. This function lets the package know where 
the datasets are and makes sure functions know how to talk to each other.

The necessary information needs to be defined in an ".Rprofile" file. There is 
an extensive explanation for how to tell this function what it wants to know on 
the help page:
```R
?configure_polarcm
```

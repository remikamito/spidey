# spidey
This is an R package for generating spider plots (radar charts) for individual patient tract Z-scores.

## Installation
spidey can be installed in R by typing the following commands:

    library(devtools)
    install_github("remikamito/spidey")

## Required R packages
There are a number of R packages that are required for spidey. These include: [devtools](https://cran.r-project.org/web/packages/devtools/index.html), [fmsb](https://cran.r-project.org/web/packages/fmsb/index.html), [RNOmni](https://cran.r-project.org/web/packages/RNOmni/index.html), [dplyr](https://cran.r-project.org/web/packages/dplyr/index.html), [tidyr](https://cran.r-project.org/web/packages/tidyr/index.html). 

In addition, the neuroCombat R package should be installed if you wish to perform ComBat on tract data:

    library(devtools)
    install_github("jfortin1/neuroCombatData")
    install_github("jfortin1/neuroCombat_Rpackage")

## Usage
This package can generate Z-scores from tract data for individual patients/participants, and plot these Z-scores on spider plots (also known as radar charts).
The two main functions in this package are: `RUNTRACTZ` and `SPIDERZ`. This section details the main steps that should be taken.

### 1. Input data
The expected input data is a dataframe containing individual participants as rows, and tract-specific values as columns. 
In addition, columns corresponding to a participant ID and the participant group (e.g., healthy control or patient) should be included.
If tract data have been extracted from a cohort scanned on different scanners or at different sites, columns corresponding to scanner ID and participant age should also be included.

Sample data is available within this package.

### 2. Compute tract Z-scores
Tract Z-scores can be computed using the `RUNTRACTZ` function. This function will output a dataframe with Z-scores for a given participant. 
As an example, we can run `RUNTRACTZ` on our included sample data:

    library(spidey)
    Zscores <- RUNTRACTZ(tractdata, 14:42, "P01", combat=TRUE)

This will output a dataframe (Zscores), which contains Z-scores for each tract included within the tract range (here, columns 14 through to 42) for a given patient (ID = P01).
As we have included the option to perform ComBat, the tract data will first be harmonised across the scanners/sites.

### 3. Generate spider plots
To be completed.

# Labels or taxes: why not boht? -- data and analyses

This repository contains data and analysis for the paper by Paolo Crosetto, Laurent Muller 
and Bernard Ruffieux "*Label or taxes: Why not both? Testing nutritional mixed policies in the lab*". 
It allows anyone to reproduce all the analyses carried out in the paper and to download the data for further analysis.

The paper is published in JEBO as Open Access,  [download the paper here](https://www.sciencedirect.com/science/article/pii/S0167268124004396)

## Dependencies

To run the analysis you need R and the following packages (available on CRAN):

-   `tidyverse` -- a set of tools to work with tidy -- i.e. well behaved -- data. Two extra packages from the `tidyverse` collection are also needed, `scales` and `magrittr`.
-   `broom` -- a library to tidy the output of tests, regressions, so that it can be used with tidyverse
-   `fixest`, `lme4`, `car`, `clubSandwich` and `modelsummary` -- a set of libraries to estimate and report fixed-effect and random-effects models
-   `kable` and `kableExtra` -- a tool to make beautiful tables
-   `hrbrthemes`, `scico`, `ggbeeswarm`, `gghalves`, `ggridges`, `waffle` and `patchwork` -- a set of extensions gand themes for `ggplot`

## How to run the analysis

-   Download or clone this repository.
-   Open the .Rproj file.
-   Open and execute the Analysis.R file.

The analysis is fully carried out in the file Analysis.R. This file:

-   loads the packages (do install them first if you do not have them yet)
-   loads the data
-   calls on individual scripts in the `/Scripts` folder to generate individual figures or tables

For each figure or table in the paper, there is one dedicated file.

### Figures

Figures are saved to the `Figures/` folder. 
They are the high-resolution images (and do not fit well in the github preview screen) included in the paper. 
This repo also includes the extra figures created for presentation purposes; those are not included in the paper, and are a visual representation of the results that are mainly exposed using tables in the paper.

### Tables

Tables are saved to the `Tables/` folder. 
They are .pdf version of the latex-compiled tables included in the paper, as well as the .tex version and sometimes a .png version.

## License

Creative Commons Attribution-NonCommercial-ShareAlike -- CC BY-NC-SA
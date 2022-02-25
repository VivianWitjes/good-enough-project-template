# Best Practices writing reproducible code (good enough project template)

## Project organization: folder structure
- PG = project-generated
- HW = human-writable
- RO = read only
```
.
├── .gitignore
├── CITATION.md
├── LICENSE.md
├── README.md
├── requirements.txt
├── data               <- All project data
│   ├── processed      <- The final, canonical data sets for modeling. (PG)
│   ├── raw            <- The original, immutable data dump. (RO)
│   └── temp           <- Intermediate data that has been transformed. (PG)
├── docs               <- Documentation notebook for users (HW)
│   ├── manuscript     <- Manuscript source, e.g., LaTeX, Markdown, etc. (HW)
├── results
│   ├── figures        <- Figures for the manuscript or reports (PG)
│   └── output         <- Other output for the manuscript or reports (PG)
└── scripts            <- Source code for this project (HW)


```

## Project title

Descriptive analysis behavior and physical score.

## Project description

Descriptive analysis of the relation between a physical score and the proportion of behaviors performed 1 or 2
days before collecting the physical score in pigs.


## Download project

Make sure the latest version of R studio is downloaded from 
https://www.rstudio.com/products/rstudio/download/#download 

## Launch R script

1. Clone the repository content from VivianWitjes/good-enough-project-template on git hub using git clone
	into the desired local folder
2. Open the good-enough-project-template.Rproj in the good-enough-project-template folder
3. In R studio, go to the files tab in the bottom left window and navigate to the scripts folder
4. Open the ExampleScript_BestPracticesreproducibleCode.R file
5. You can now run the script!

## R and packages version info

This script was produced in:
R version 4.0.3 (2020-10-10)
Platform: x86_64-w64-mingw32/x64 (64-bit)
Running under: Windows 10 x64 (build 19042)

Base packages (under version 4.0.3):
stats 
graphics 
grDevices 
utils 
datasets 
methods 
base 

Extra packages:
ggplot2 version 3.3.5
gridExtra version 2.3
pastecs version 1.3.21
Hmisc version 4.4-1
docstring version 1.0.0
TempPackage version 1.0
Formula version 1.2-4
survival version 3.2-7
lattice version 0.20-41

download from:
https://cran.r-project.org/web/packages/available_packages_by_name.html

## Badges

binder: [![Binder](https://mybinder.org/badge_logo.svg)](https://mybinder.org/v2/gh/VivianWitjes/good-enough-project-template/main)`

## License

This project is licensed under the terms of the [MIT License](/LICENSE.md)





## Replication instructions for the paper

The replication archive is organized into two directories (note that these files are also available on Github at [https://github/s7minhas/conflictEvolution](https://github.com/s7minhas/conflictEvolution)):

- **main**: contains the data files and scripts necessary to reproduce the main results in the paper
- **appendix**: contains the data files and scripts necessary to reproduce the results in the appendix

Replicating the figures and tables in the **main** text will take only a few minutes on a standard laptop if the provided `.rda` files are used. If the provided results are deleted, the running time for the **main** text figures and tables is detailed below. 

#### Setup information

All of the analyses reported in the manuscript and the appendix were run on a Macbook 15" Pro with the following session information for R (further information on packages used in the analysis is included at the end of the README): 

```
R version 3.5.0 (2018-04-23)
Platform: x86_64-apple-darwin15.6.0 (64-bit)
Running under: macOS High Sierra 10.13.6

Matrix products: default
BLAS: /System/Library/Frameworks/Accelerate.framework/Versions/A/Frameworks/vecLib.framework/Versions/A/libBLAS.dylib
LAPACK: /Library/Frameworks/R.framework/Versions/3.5/Resources/lib/libRlapack.dylib

locale:
[1] C

attached base packages:
[1] stats     graphics  grDevices utils     datasets  methods   base     

loaded via a namespace (and not attached):
[1] compiler_3.5.0
```

#### Running the scripts

Scripts should be run in the following order (each script assumes that the working directory matches the directory in which the script is stored): 

- figure2.R: Create longitudinal, spatial visualization of conflict and stores results in `main/floats/figure2.pdf`
- figure3.R: Network visualization of conflict in Nigeria from 2000-2016.
- figure4.R: Estimates parameters using AME in the paper.
    + Original results from the authors are already included, if the relevant files are deleted, then this script will take approximately one hour to run. 
- figure5.R: Network visualization of conflict in Nigeria from 2000-2016 highlighting pre and post Boko Haram changes in relationships.
- figure6.R: Visualization of additive sender/receiver effects from AME framework.
- figure7.R: Visualization of multiplicative effects from AME framework.
- figure8.R: Cross-validation based performance comparison. 
    + Original results from the authors are already included, if the relevant files are deleted, then this script will take approximately one hour to run assuming that the script is run in parallel using six cores.

We have also included all the analysis necessary to generate the results in the appendix. The appendix related scripts are labeled as `figureA[...].R` and `tableA1.R`. All the results from the appendix scripts will be stored in the `appendix/floats/` directory.

#### R package build notes

Last, please note the version of each of the libraries that our project relies on (each library was built using R 3.5.0). 

|                    |                     |                |                   |
|:-------------------|:--------------------|:---------------|:------------------|
|RColorBrewer: 1.1-2 |ROCR: 1.0-7          |s7minhas/amen: 1.4       |btergm: 1.9.1      |
|caTools: 1.17.1     |countrycode: 0.16    |cshapes: 0.6    |doParallel: 1.0.11 |
|foreach: 1.4.4      |ggmap: 2.6.1         |ggplot2: 3.0.0  |igraph: 1.2.1      |
|latex2exp: 0.4.0    |lmtest: 0.9-36       |magrittr: 1.5   |network: 1.13.0.1  |
|plyr: 1.8.4         |randomForest: 4.6-14 |reshape2: 1.4.3 |tidyr: 0.8.0       |
|xtable: 1.8-2       |                     |                |                   |

If you find any errors or have any further questions, please address them to me via email at minhassh@msu.edu.
# Advanced R Programming Tutorial 2022: Make your health economic model Shiny!
This repository stores the code, presentations, and material used in the R-HTA in LMICs Advanced 2022 Workshop (held in January 2023). The following sections provide a breakdown of the primary documents and guidance on how to use them for your own personal training. The tutorial is based on several open-source frameworks for R in HTA (see [citations](https://github.com/R-HTA-in-LMICs/Advanced-Tutorial-2022#citation) section), developed by the [DARTH](https://darthworkgroup.com) group and the Shiny framework for HTA developed by [Dark Peaks](https://darkpeakanalytics.com).

## Navigation

The [`R`](https://github.com/R-HTA-in-LMICs/Advanced-Tutorial-2022/tree/main/R) folder includes a standalone R script code that replicates an original model presented in chapter two of '*Decision Modelling for Health Economic Evaluation* by Briggs A., et al.

The [`Excel`](https://github.com/R-HTA-in-LMICs/Advanced-Tutorial-2022/tree/main/Excel) folder includes the original model's solution template, provided by Oxford's [Health Economics Research Centre (HERC)](https://www.herc.ox.ac.uk/downloads/decision-modelling-for-health-economic-evaluation).

The [`shiny`](https://github.com/R-HTA-in-LMICs/Advanced-Tutorial-2022/tree/main/shiny/hiv_model) folder includes the code used to develop and run the Shiny application version of the cost-effectiveness model.

## Preliminaries

-   Install [RStudio](https://www.rstudio.com/products/rstudio/download/)
-   Install [`dampack`](https://cran.r-project.org/web/packages/dampack/index.html) R package from CRAN
-   Install [`shiny`](https://cran.r-project.org/web/packages/shiny/index.html) R package from CRAN

```{r, eval=FALSE}
# Install release version from CRAN
install.packages(c("dampack", "shiny"))

# For dampack specifically, you can also install the development version of the
# package from GitHub
# devtools::install_github("DARTH-git/dampack")
```

-   Install `devtools` to install [`darthtools`](https://github.com/DARTH-git/darthtools) R package from [DARTH's GitHub](https://github.com/DARTH-git)

```{r, eval=FALSE}
# Install release version from CRAN
install.packages("devtools")

# Or install development version from GitHub
# devtools::install_github("r-lib/devtools")
```

-   Install `darthtools` using `devtools`

```{r, eval=FALSE}
# Install development version from GitHub
devtools::install_github("DARTH-git/darthtools")
```

To run the CEA, you require [`dampack`: Decision-Analytic Modeling Package](https://cran.r-project.org/web/packages/dampack/index.html), an R package for analysing and visualizing the health economic outputs of decision models.

We also recommend familiarising with the useful [DARTH](http://darthworkgroup.com) coding framework described in:

-   Alarid-Escudero F, Krijkamp EM, Pechlivanoglou P, Jalal HJ, Kao SYZ, Yang A, Enns EA. [A Need for Change! A Coding Framework for Improving Transparency in Decision Modeling](https://link.springer.com/article/10.1007/s40273-019-00837-x). [PharmacoEconomics](https://www.springer.com/journal/40273), 2190;37(11):1329--1339. <https://doi.org/10.1007/s40273-019-00837-x>

Lastly, we *strongly* recommended familiarising yourself with Shiny before undertaking the tutorial

- [Getting started with Shiny](https://ourcodingclub.github.io/tutorials/shiny/)
- [A Gentle Introduction to creating R Shiny Web Apps](https://www.youtube.com/watch?v=jxsKUxkiaLI)

## Citation

This tutorial is based on open-source R code frameworks for HTA:

> Alarid-Escudero F, Krijkamp EM, Enns EA, Yang A, Hunink MGM, Pechlivanoglou P, Jalal H. A Tutorial on Time-Dependent Cohort State-Transition Models in R using a Cost-Effectiveness Analysis Example (<https://arxiv.org/abs/2108.13552>). arXiv:2108.13552v2. 2022:1-37.

> Alarid-Escudero F, Krijkamp EM, Enns EA, Yang A, Hunink MGM, Pechlivanoglou P, Jalal H (2022). R Code for A Tutorial on Time-Dependent Cohort State-Transition Models in R using a Cost-Effectiveness Analysis Example (Version v0.2.0). Zenodo. [10.5281/zenodo.6620902](https://www.doi.org/10.5281/zenodo.6620902). Last accessed 7 June 2022.

> Smith R and Schneider P. [Making health economic models Shiny](https://doi.org/10.12688/wellcomeopenres.15807.1): A tutorial. Wellcome Open Res 2020, 5:69.

# Additional Information

Visit our [webpage](https://r-hta-in-lmics.github.io/) and follow the links to our social media to keep up-to-date with our latest tutorials. Alternatively, follow us on [EventBrite](https://www.eventbrite.co.uk/o/r-hta-in-lmics-46016978693) to receive notifications for when new events go live!
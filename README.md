# Characterising the longitudinal relationship between social media use and psychiatric diagnoses in secondary care in adolescents in England

This repository contains the supplementary data for the paper "Characterising the longitudinal relationship between social media use and psychiatric diagnoses in secondary care in adolescents in England" (preprint coming soon).

Please review the licensing information at [`LICENCE.md`](https://github.com/tom-metherell/hes-social-media/blob/main/LICENCE.md) and [`THIRDPARTY.md`](https://github.com/tom-metherell/hes-social-media/blob/main/THIRDPARTY.md) before reusing any software in this repository.

## Data source

The data used in this study are from the [Millennium Cohort Study](https://cls.ucl.ac.uk/cls-studies/millennium-cohort-study/). The main survey data are available from the [UK Data Service](https://beta.ukdataservice.ac.uk/datacatalogue/series/series?id=2000031). The linked Hospital Episode Statistics dataset is available on application to the [UK Data Service SecureLab](https://ukdataservice.ac.uk/find-data/access-conditions/secure-application-requirements/apply-to-access-non-ons-data/) or the [UK Longitudinal Linkage Collaboration](https://ukllc.ac.uk/).

## Data analysis

The data analysis in this study uses R version 4.4.0 in the `renv` environment present in the repository, via the `targets` pipeline.

## Supplementary information

The Quarto-derived HTML files are rendered automatically as part of the `targets` pipeline, and the output `.html` files are also provided in the repository. To view these, you should download both the `.html` file and the associated `_files` subdirectory to ensure that graphics are rendered correctly.

You can also view the reports on [descriptive statistics](https://tom-metherell.github.io/hes-social-media/descriptives_report.html), [derivation of weights](https://tom-metherell.github.io/hes-social-media/weights_derivation.html), [multiple imputation](https://tom-metherell.github.io/hes-social-media/multiple_imputation.html) and [model outputs](https://tom-metherell.github.io/hes-social-media/model_outputs.html) online.

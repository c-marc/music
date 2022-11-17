# dev history
file.create("dev_history.R")
use_build_ignore("dev_history.R")


use_description()
# R > 4.1.0 for |> and lambda functions
# use_package(...)
# cli, dplyr, glue, rlang, stringr, tidyr, tidyselect, zeallot


use_gpl_license()
use_readme_rmd()


use_package_doc()
# importFrom rlang .data :=
# glue import is required for glue syntax, but through note check if nothing is imported... so:
# importFrom glue glue
# importFrom zeallot %<-%

use_testthat()
use_data_raw()


# Landing vignette
use_vignette("music")
# dev vignette
use_vignette("_WIP")
use_build_ignore("vignettes/_WIP.Rmd")

use_news_md()
use_pkgdown()
use_pkgdown_github_pages()

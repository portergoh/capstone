#' Helper function to install the smur package
#'
smur_install <- function ()
{
  # Define some vars
  smur_tgz  = "smur_0.1.0.tgz"
  smur_from = paste0 ("https://github.com/portergoh/capstone/raw/master/download/",smur_tgz)
  smur_to = file.path (getwd(), smur_tgz, fsep=.Platform$file.sep )

  # Download source.
  download.file (smur_from, smur_to, method="auto", mode="wb")

  # Install source.
  install.packages(smur_to, type="source", repos=NULL)

  # Clean up
  unlink (smur_to)

  # Load the prerequisite libraries into R session
  source("https://raw.githubusercontent.com/portergoh/capstone/master/smur/R/requirements.R")
}

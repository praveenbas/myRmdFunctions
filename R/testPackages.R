#' pkgTest
#'
#' @author praveen Baskran \email{spemannpraveen@gmail.com}
#' @description Test package is installed if not installs it.
#' see \link{install.packages} for more details
#' @param package   Package to be checked
#' @return Checks and install package is not found
#' @export

pkgTest <- function(package){
  if (!suppressWarnings(require(package,character.only = TRUE,quietly = TRUE)))
  {
    message(sprintf("Required package :::: %s :::: is not installed",package))
    message ("Type yes if you want to install [yes/no]:::: ")
    User_check= readLines(con=stdin(),1)

    if(User_check %in% c("y","Y","Yes","yes","YES")){
      #install.packages(package,dep=TRUE)

      if(!require("BiocInstaller",character.only = T)){
        biocLite("BiocInstaller")
        source("https://bioconductor.org/biocLite.R")
        biocLite("BiocInstaller")
      }else{
        biocLite(package)
      }
      # load and check the installed packages
      # incase of install from github, remove the where thing before and including "/"
      if(!require(gsub(pattern = ".*/",replacement = "",package),character.only = TRUE)) {
        stop("package not found")
      }
    }else{
      stop()
    }
  }
}

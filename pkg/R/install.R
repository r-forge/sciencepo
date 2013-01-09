install <-
function(packages=NULL, install=TRUE) {

if(install){install.packages(packages, dependencies=TRUE, repos = "http://cran.r-project.org")}
lapply(packages, library, character.only = TRUE)
}

.onLoad <- function(libname, pkgname) {
    msg <- strwrap(sprintf(
              "*** Deprecation warning ***: Package '%s' is deprecated
              and will not be supported after Bioconductor release
              %s.", pkgname, "2.11"), exdent=4)
    packageStartupMessage(paste(msg, collapse="\n"))
}
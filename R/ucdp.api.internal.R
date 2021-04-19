
.onAttach <-
  function(libname, pkgname) {
    packageStartupMessage("\nUCDP data must be cited as indicated on the UCDP download page. \n")
    packageStartupMessage("The package may be cited as:")
    packageStartupMessage('Dworschak, Christoph. 2021. "Ucdp.api: Automated Retrieval of UCDP Conflict ')
    packageStartupMessage('Data." R package development version 0.0.1. \n')
  }

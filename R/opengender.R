.pkgenv <- new.env(parent=emptyenv())

.onLoad <- function(libname, pkgname) {

   myWd <- og_find_workingdir()
   .pkgenv[["dicts"]] <- og_init_dict()
   myOpt <- list(
       opengender.workingdir = myWd,
       opengender.apikey = "",
       opengender.retries = 3,
       opengender.delay = 10
    )
    options(myOpt)
}

list_dict <- function() {
  .pkgenv[["dicts"]][c("name","desc")]
}

og_init_dict<-function() {
  tibble::tribble(
    ~name, ~desc, ~type, ~loader,  ~uri,
    "wgen2",   "world gender dictionary 2.0", "external", "wgen", "",
    "kantro",  "kantowitz NLTK dictionary", "internal", "internal", "",
    "genderize",  "kantowitz NLTK dictionary", "api", "genderize", ""
  )
}

og_find_workingdir <- function() {
  return(".")
}

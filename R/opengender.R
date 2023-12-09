.pkgenv <- new.env(parent=emptyenv())

.onLoad <- function(libname, pkgname) {

   myDd <- og_find_workingdir()
   myCd <- og_find_workingdir()

  .pkgenv[["dicts"]] <- og_init_dict()
   myOpt <- list(
       opengender.datadir = myDd,
       opengender.cachedir = myCd,
       opengender.apikey = "",
       opengender.retries = 3,
       opengender.delay = 10
    )
    options(myOpt)
}


og_init_dict<-function() {
  tibble::tribble(
    ~name, ~desc, ~type, ~loader,  ~uri,
    "wgen2",   "world gender dictionary 2.0", "external", "wgen", "",
    "kantro",  "kantowitz NLTK dictionary", "internal", "internal", "",
    "genderize",  "genderize", "api", "genderize", ""
  )
}

og_find_workingdir <- function() {
  rappdirs::user_data_dir(
    appname = "opengender",
    appauthor = appname
  )

  # TODO: makedir or fail to temp
  }

og_find_workingdir <- function() {
  rappdirs::user_cache_dir(
    appname = "opengender",
    appauthor = appname
  )
  # TODO: makedir or fail to temp
  }

list_dict <- function() {
  .pkgenv[["dicts"]][c("name","desc","type")]
}

load_dict <- function(name="kantro") {

}

og_load_dict_internal <- function(url) {

}

og_load_dict_external <- function(url) {

}

og_load_dict_genderize <- function(url) {

}

.pkgenv <- new.env(parent=emptyenv())

.onLoad <- function(libname, pkgname) {

   myDd <- og_find_workingdir()
   myCd <- og_find_cachedir()

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
    ~name, ~desc, ~version, ~type, ~loader,  ~uri,
    "wgen2",   "world gender dictionary", 2, "external", "wgen", "",
    "kantro",  "kantrowitz  NLTK dictionary", 1, "internal", "internal", "",
    "genderize",  "genderize", 1,  "api", "genderize", ""
  )
}

og_find_cachedir <- function() {
  tmpd <- rappdirs::user_cache_dir(
    appname = "opengender",
    appauthor = appname
  )
  if (!dir.exists(tmpd) & !dir.create(tmpd, recursive=TRUE)) {
    tmpd <- tmpdir(check=TRUE)
  }
  tmpd
}

og_find_datair <- function() {
  tmpd <- rappdirs::user_cache_dir(
    appname = "opengender",
    appauthor = appname
  )
  if (!dir.exists(tmpd) & !dir.create(tmpd, recursive=TRUE)) {
    tmpd <- tmpdir(check=TRUE)
  }
  tmpd
}


list_dict <- function() {
  .pkgenv[["dicts"]][c("name","desc","type")]
}

load_dict <- function(name="kantro") {

}

og_load_dict_internal <- function(name, url) {
  data(name,package="opengender")
}

og_load_dict_external <- function(name, url) {

}

og_load_dict_api <- function(name, url) {

}

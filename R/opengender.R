OG_DICT_EXT <- "_dict"
OG_DICT_FILE_EXT <- ".rds"

.pkgenv <- new.env(parent=emptyenv())

.onLoad <- function(libname, pkgname) {

   myDd <- og_find_workingdir()
   myCd <- og_find_cachedir()

  .pkgenv[["dicts"]] <- og_init_dict()
  .pkgenv[["cacheobj"]] <- og_init_cache()
   myOpt <- list(
       opengender.datadir = myDd,
       opengender.cachedir = myCd,
       opengender.cachesize = 10 * 1024^2,
       opengender.cacheage = 3600*24*30,
       opengender.apikey = "",
       opengender.retries = 3,
       opengender.delay = 10
    )
    options(myOpt)
}


og_init_dict<-function() {
  tibble::tribble(
    ~name, ~desc, ~version, ~type, ~loader,  ~uri,
    "wgen2",   "world gender dictionary", 2, "external", "wgen", "https://dataverse.harvard.edu/api/access/datafile/4750352",
    "kantro",  "kantrowitz  NLTK dictionary", 1, "internal", "internal", "https://raw.githubusercontent.com/nltk/nltk_data/gh-pages/packages/corpora/names.zip",
    "genderize",  "genderize", 1,  "api", "genderize", ""
  )
  #TODO: scan data directory for dictionaries
}

og_init_cache<-function() {
  cacheobj <- cachem::cache_disk(
    dir=options()[["opengender.cachedir"]],
    max_size = options()[["opengender.cachesize"]],
    max_age = options()[["opengender.cacheage"]]
    )

  cacheobj
}

og_find_cachedir <- function() {
  tmpd <- rappdirs::user_cache_dir(
    appname = "opengender",
    appauthor = appname
  )
  if (!dir.exists(tmpd) && !dir.create(tmpd, recursive=TRUE)) {
    tmpd <- tmpdir(check=TRUE)
    tmpd <- file.path(tmpd/opengender)
    if (!dir.exists(tmpd)) {dir.create(tmpd, recursive=TRUE)}  }
  tmpd
}

og_find_datadir <- function() {
  tmpd <- rappdirs::user_cache_dir(
    appname = "opengender",
    appauthor = appname
  )
  if (!dir.exists(tmpd) && !dir.create(tmpd, recursive=TRUE)) {
    tmpd <- tmpdir(check=TRUE)
    tmpd <- file.path(tmpd/opengender)
    if (!dir.exists(tmpd)) {dir.create(tmpd, recursive=TRUE)}
  }
  tmpd
}

list_dict <- function() {
  .pkgenv[["dicts"]][c("name","desc","type")]
}

load_dict <- function(name="kantro", force=FALSE) {
    dict_entry <-  .pkgenv[["dicts"]] %>% select('name'=name)
    if (dim(dict_entry)[1]!=1) {
      stop("Dictionary not found ", name)
    }

    if (!force && exists(og_gen_dictname(name), envir=.pkgenv) ) {
        return(TRUE)
    }

    rv <- do.call(paste("og_load_dict_",dict_entry[1,"loader"]),
            args = list(entry=dict_entry)
            )
    return(rv)
}

import_dict <- function(data,name="",save=FALSE) {
  # check for name conflicts
  # normalize
  # insert in environment
  # TODO: save to data dir
}

clean_dicts<-function(cleancache=TRUE, cleandata=TRUE) {
  if(cleandata) {
    file.remove(dir(options[["opengender.datadir"]],pattern=paste0('.*',OG_DICT_EXT,OG_DICT_FILE_EXT) ))
  }
  if (cleancache) {
    .pkgenv[["cacheobj"]]$reset()
  }
}

og_normalize_dict<-function(x) {
  x
}

og_load_dict_internal <- function(name, entry) {
  data(name,package="opengender")
  assign(og_gen_dictname(name), og_normalize_dict(name),  envir=.pkgenv)
  invisible(TRUE)
}

og_load_dict_external <- function(name, entry) {
  dict_file<-og_gen_dictfilepath(name)
  if (!file.exists(file.path(load_dir,dict_file))) {
    do.call(og_gen_dictname(name),
            args = list(entry=dict_entry)
    )
  }
  tmpdict <- read_rds(file.path(load_dir,dict_file))
  assign(og_gen_dictname(name), tmpdict ,  envir=.pkgenv)
}

og_gen_dictname(name="") [
  paste0(name,OG_DICT_EXT)
]

og_gen_dictfilepath(name) {
  load_dir <- options("opengender.datadir")
  rv <- paste0(og_normalize_dict(name), OG_DICT_FILE_EXT)
  rv
}

og_load_dict_api <- function(name, entry) {

}

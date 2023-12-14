
# Package: Constants --------------------------------------------------------
OG_DICT_EXT <- "_dict"
OG_DICT_FILE_EXT <- ".rds"

# Package: Variables --------------------------------------------------------
.pkgenv <- new.env(parent=emptyenv())

# Internals: Startup --------------------------------------------------------
.onLoad <- function(libname, pkgname) {

   myDd <- og_find_workingdir()
   myCd <- og_find_cachedir()

  .pkgenv[["dicts"]] <- og_init_dictlist()
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

og_init_dictlist<-function() {
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

# Internals: dictionary manipulation  --------------------------------------------------------

og_normalize_dict<-function(x) {
  # column naming
  # column cleaning: given, country, year, pr_f, N
  # aggregation by given
  # fill in missing
  x
}

og_load_dict_internal <- function(name, entry) {
  data(name,package="opengender")
  assign(og_gen_dictname(name), og_normalize_dict(name),  envir=.pkgenv)
  invisible(TRUE)
}

og_load_dict_imported <- function(name, entry) {
  #TODO
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

# Internals: dictionary-specific source extraction  --------------------------------------------------------


# Public: Dictionary Manipulation --------------------------------------------------------


#' Title
#'
#' @return
#' @export
#'
#' @examples
list_dict <- function() {
  .pkgenv[["dicts"]][c("name","desc","type")]
}

#' Title
#'
#' @param name
#' @param force
#'
#' @return
#' @export
#'
#' @examples
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

#' Title
#'
#' @param data
#' @param name
#' @param save
#'
#' @return
#' @export
#'
#' @examples
import_dict <- function(data,name="",save=FALSE) {
  # check for name conflicts
  # normalize
  # insert in environment
  # TODO: save to data dir
}

#' Title
#'
#' @param cleancache
#' @param cleandata
#'
#' @return
#' @export
#'
#' @examples
clean_dicts<-function(cleancache=TRUE, cleandata=TRUE) {
  if(cleandata) {
    file.remove(dir(options[["opengender.datadir"]],pattern=paste0('.*',OG_DICT_EXT,OG_DICT_FILE_EXT) ))
  }
  if (cleancache) {
    .pkgenv[["cacheobj"]]$reset()
  }
}

# Public: Imputation --------------------------------------------------------

#' Title
#'
#' @param given
#' @param year
#' @param country
#' @param dicts
#'
#' @return
#' @export
#'
#' @examples
impute_gender<-function(given,year,country,dicts="kantro") {
  purrr::walk(dicts,load_dict)
  dicts.tbl <- og_combine_dicts(dicts)

  # threshold
  # normalize input
  # unique matches
  # fuzzy matches
}

# Public: Estimation --------------------------------------------------------

#' Title
#'
#' @param x
#'
#' @return
#' @export
#'
#' @examples
gender_mean <-function(x) {

}

#' Title
#'
#' @param x
#'
#' @return
#' @export
#'
#' @examples
gender_se <-function(x) {

}

#' Title
#'
#' @param x
#'
#' @return
#' @export
#'
#' @examples
gender_ci <- function(x) {

}

#' Title
#'
#' @param x
#'
#' @return
#' @export
#'
#' @examples
gender_sample <- function(x) {

}

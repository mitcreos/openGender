

# Package: Constants --------------------------------------------------------
OG_DICT_EXT <- "_dict"
OG_DICT_FILE_EXT <- ".rds"
OG_DICT_NOYEAR <- 3000
OG_DICT_NOCOUNTRY <- "00"


# Package: Variables --------------------------------------------------------
.pkgenv <- new.env(parent = emptyenv())

# Internals: Startup --------------------------------------------------------
.onLoad <- function(libname, pkgname) {
  myDd <- og_find_datadir()
  myCd <- og_find_cachedir()

  myOpt <- list(
    opengender.datadir = myDd,
    opengender.cachedir = myCd,
    opengender.cachesize = 10 * 1024 ^ 2,
    opengender.cacheage = 3600 * 24 * 30,
    opengender.apikey = "",
    opengender.retries = 3,
    opengender.backoff = 10,
    opengender.dict.minsize = 25
  )
  options(myOpt)


  .pkgenv[["dicts"]] <- og_init_dictlist()
  .pkgenv[["cacheobj"]] <- og_init_cache()

  mem_og_api_call_inner <-
    memoise::memoise(og_api_call_inner,
                     cache =  .pkgenv[["cacheobj"]],
                     omit_args = c("apikey", "host"))
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

og_init_cache <- function() {
  cacheobj <- cachem::cache_disk(dir = options()[["opengender.cachedir"]],
                                 max_size = options()[["opengender.cachesize"]],
                                 max_age = options()[["opengender.cacheage"]])

  cacheobj
}

og_find_cachedir <- function() {
  tmpd <- rappdirs::user_cache_dir(appname = "opengender",
                                   appauthor = appname)
  if (!dir.exists(tmpd) && !dir.create(tmpd, recursive = TRUE)) {
    tmpd <- tmpdir(check = TRUE)
    tmpd <- file.path(tmpd / opengender)
    if (!dir.exists(tmpd)) {
      dir.create(tmpd, recursive = TRUE)
    }
  }
  tmpd
}

og_find_datadir <- function() {
  tmpd <- rappdirs::user_cache_dir(appname = "opengender",
                                   appauthor = appname)
  if (!dir.exists(tmpd) && !dir.create(tmpd, recursive = TRUE)) {
    tmpd <- tmpdir(check = TRUE)
    tmpd <- file.path(tmpd / opengender)
    if (!dir.exists(tmpd)) {
      dir.create(tmpd, recursive = TRUE)
    }
  }
  tmpd
}

# Internals: dictionary manipulation  --------------------------------------------------------

og_dict_normalize <- function(x, threshold) {
  v_req <- c("given","pr_f")
  v_opt <- c("country","year","N")

  # column checks

  if (!all(v_req %in% colnames(x))) {
    stop("missing required variables -- check ",
         paste(v_req[!(v_req %in% colnames(x))],","))
  }

  if (length(setdiff(colnames(x),c(vreq,v_opt)))) {
    warning("dropping additional variables")
  }

  data_norm <- x

  data_norm %<>% select(all_of(v_req),any_of(v_opt))

  # fill missing cols
  if (!"N" %in% colnames(data_norm) ) {
    data_norm %>% mutate(N=NA)
  }

  if (!"year" %in% colnames(data_norm) ) {
    data_norm %>% mutate(year=OG_DICT_NOYEAR)
  }

  if (!"country" %in% colnames(data_norm) ) {
    data_norm %>% mutate(N=OG_DICT_NOCOUNTRY)
  }

  # clean columns: given, country, year, pr_f, N

  # reaggregation by given

  # fill in missing
  data_norm
}

og_dict_load_internal <- function(name, entry) {
  data(name, package = "opengender")
  assign(og_dict_genname(name), og_normalize_dict(name),  envir = .pkgenv)
  invisible(TRUE)
}

og_dict_import <- function(data, name, save=TRUE, normalize=TRUE, type=c("imported","external","api","internal")) {
  # normalize
  if (normalize) {
    data_norm <- og_normalize_dict(data)
  }
  # insert in environment
  assign(og_dict_genname(name), data_norm ,  envir = .pkgenv)

  # TODO: save to data dir
  if (save) {
    og_save_dict(data_norm,)
  }
}

og_dict_save<- function(name) {

}

og_dict_load_added <- function(name, entry) {
  #TODO
  invisible(TRUE)
}

og_dict_load_external <- function(name, entry) {
  dict_file <- og_gen_dictfilepath(name)
  if (!file.exists(file.path(load_dir, dict_file))) {
    do.call(og_dict_genname(name),
            args = list(entry = dict_entry))
  }
  tmpdict <- read_rds(file.path(load_dir, dict_file))
  og_import_dict(tmpdict,
                 name,
                 save = FALSE,
                 normalize = TRUE,
                 type = "external")
}

og_dict_genname<- function(name = "") {
  paste0(name, OG_DICT_EXT)
}

og_dict_genfilepath<-function(name) {
  load_dir <- options("opengender.datadir")
  rv <- file.path(load_dir,paste0(og_dict_genname(name), OG_DICT_FILE_EXT))
  rv
}

og_dict_load_api <- function(name, entry) {

}

og_dict_combine <- function(dicts) {

}

og_dict_fetch_entry<-function(name) {
  val_ <- name
  dict_entry <-  .pkgenv[["dicts"]] %>% dplyr::filter(`name` == {{ val_ }} )
  if (dim(dict_entry)[[1]]==0) {
    dict_entry <- tibble::tibble()
  }
  return(dict_entry)
}

og_clean_given <- function(x) {
  x %>%
    stringr::str_squish() %>%
    stringr::str_to_lower() %>%
    stringi::stri_trans_general(id = "Latin-ASCII") %>%
    stringr::str_replace_all('\\s', '-') %>%
    stringr::str_remove_all('[\\W0-9--\\-]') %>%
    stringr::str_replace_all('-.', '-')
}

og_clean_year<-function(x,ymin=1000,ymax=2050) {
  x %>%
    trunc() %>%
    pmax(ymin) %>%
    pmin(ymax) %>%
    tidyr::replace_na(OG_DICT_NOYEAR)
}

og_clean_country <- function(x) {
  codes <- c(iso3166[["country_2d"]],OG_DICT_NOCOUNTRY)
  rv <- as.character(x)
  mismatch <- setdiff(unique(rv),codes)
  if (length(mismatch)>0) {
    warning("country codes not matched to iso3166:", paste(mismatch, sep=","))
  }
  rv %>%
    dplyr::case_match(
      mismatch ~ OG_DICT_NOCOUNTRY,
      .default = x
    ) %>%
    tidyr::replace_na(OG_DICT_NOCOUNTRY)
}


# Internals: dictionary-specific source extraction  --------------------------------------------------------
og_api_call <- function(given, country, year, apikey, host, service) {
  res <- mem_og_api_call_inner(given, country, year, apikey, host, service)
  if (!res[["complete"]]) {
    memoise::drop_cache(mem_og_api_call_inner)(given, country, year, apikey, host, service)
  }
  # complete response columns

  if (!"given" %in% colnames(res)) {
    res %<>% mutate(given = NA)
  }
  if (!"count" %in% colnames(res)) {
    res %<>% mutate(count = NA)
  }
  if (!"country" %in% colnames(res)) {
    if (missing(country)) {
      country_res <- OG_DICT_NOCOUNTRY
    } else {
      country_res <- country
    }
    res %<>% mutate(country = country_res)
  }
  if (!"year" %in% colnames(res)) {
    if (missing(year)) {
      country_res <- OG_DICT_NOYEAR
    } else {
      year_res <- year
    }
    res %<>% mutate(year = year_res)
  }

  res
}


# Not used directly -- wrapped in memoise at startup
og_api_call_inner <-
  function(given, country, year, apikey, host, service) {
    ql <- list(given = given, service = service)

    if (!missing(apikey)) {
      ql[["apikey"]] <- apikey
    } else  if (!(is.null(options()[["opengender.apikey"]]))) {
      ql[["apikey"]] <- options()[["opengender.apikey"]]
    }

    if (!missing(country)) {
      ql[["country"]] <- country
    }
    if (!missing(host)) {
      ql[["host"]] <- host
    }
    if (!missing(year)) {
      ql[["year"]] <- year
    }

    res <- do.call(paste0(og_api_call_, service), ql)
    res
  }

og_url_build_genderize<-function(host = "api.genderize.io", given, country, year, apikey, service) {
  uri <- list()
  uri[["scheme"]] <- "https"
  uri[["port"]] <- 80
  uri[["hostname"]] <- host
  uri[["path"]] <- ""

  ql <- list()
  if (!missing(apikey)) {
    ql[["apikey"]] <- apikey
  }
  if (!missing(country)) {
    ql[["country_id"]] <- country
  }
  ql[["name"]] <- given
  uri[["query"]] <- ql

  url <- httr2::url_buid(uri)

  httr2::request(url) %>%
    httr2::req_retry(max_tries = options()[["opengender.retries"]]) %>%
    httr2::req_perform() -> resp

  if (httr2::response_is_error(resp)) {
    warning(
      service,
      " call failed:",
      httr2::resp_status(resp),
      httr2::resp_status_desc(resp)
    )
    if (httr2::resp_status(resp) %in% c("401", "402")) {
      warning("check your API key")
    }
    if (httr2::resp_status(resp) %in% c("429")) {
      warning("out of tokens -- retry tomorrow")
    }
    rv  <- tibble(complete = FALSE)
  } else {
    httr2::resp_body_json(resp) %>%
      jsonlite::fromJSON() -> resp_body.ls

    if (resp.body.ls[["gender"]] == "male") {
      pr_f <- 1 - resp.body.ls[["probability"]]
    } else {
      pr_f <- resp.body.ls[["probability"]]
    }

    rv  <-
      tibble(pr_f = pr_f,
             given_count = resp.body.ls[["count"]],
             complete = TRUE)

  }

  rv
}



# Public: Dictionary Manipulation --------------------------------------------------------


#' Title
#'
#' @return
#' @export
#'
#' @examples
list_dict <- function() {
  .pkgenv[["dicts"]][c("name", "desc", "type")]
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
load_dict <- function(name = "kantro", force = FALSE) {
  dict_entry <- og_dict_fetch_entry(name)
  if (length(dict_entry)==0) {
    stop("Dictionary not found ", name)
  }

  if (!force && exists(og_dict_genname(name), envir = .pkgenv)) {
    return(TRUE)
  }

  rv <- do.call(paste0("og_dict_load_", dict_entry[[1, "loader"]]),
                args = list(name=name, entry = dict_entry))
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
add_local_dict <- function(data, name = "", save = FALSE) {
  dict_entry <- og_dict_fetch_entry(name)
  if (length(dict_entry)==0) {
    stop("Dictionary not found ", name)
  }


    og_import_dict(
    data = data,
    name = name,
    save = save,
    normalize = TRUE,
    type = "local"
  )
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
clean_dicts <- function(cleancache = TRUE,
                        cleandata = TRUE) {
  if (cleandata) {
    file.remove(dir(options[["opengender.datadir"]], pattern = paste0('.*', OG_DICT_EXT, OG_DICT_FILE_EXT)))
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
impute_gender <- function(given, year, country, dicts = "kantro") {
  purrr::walk(dicts, load_dict)
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
gender_mean <- function(x) {

}

#' Title
#'
#' @param x
#'
#' @return
#' @export
#'
#' @examples
gender_se <- function(x) {

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

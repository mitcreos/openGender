#' @importFrom magrittr "%<>%"
#' @importFrom magrittr "%>%"
#' @import rlang

# Package: Constants --------------------------------------------------------
OG_DICT_EXT <- "_dict"
OG_DICT_FILE_EXT <- ".rds"
OG_DICT_NOYEAR <- 3000
OG_DICT_NOCOUNTRY <- "00"
OG_DICT_NON <- -1

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
    opengender.fuzzymax = .05,
    opengender.dict.minsize = 26
  )
  options(myOpt)


  .pkgenv[["dicts"]] <- og_init_dictlist()
  .pkgenv[["cacheobj"]] <- og_init_cache()

  mem_og_api_call_inner <-
    memoise::memoise(og_api_call_inner,
                     cache =  .pkgenv[["cacheobj"]],
                     omit_args = c("apikey", "host"))
}

#' @importFrom tibble tribble
og_init_dictlist<-function() {
  tibble::tribble(
    ~name, ~desc, ~version, ~type, ~loader,  ~uri,
    "wgen2",   "world gender dictionary", 2, "external", "wgen", "https://dataverse.harvard.edu/api/access/datafile/4750352",
    "kantro",  "kantrowitz  NLTK dictionary", 1, "internal", "internal", "https://raw.githubusercontent.com/nltk/nltk_data/gh-pages/packages/corpora/names.zip",
    "genderize",  "genderize", 1,  "api", "genderize", ""
  )
  #TODO: scan data directory for dictionaries
}

#' @importFrom cachem cache_disk
og_init_cache <- function() {
  cacheobj <- cachem::cache_disk(dir = options()[["opengender.cachedir"]],
                                 max_size = options()[["opengender.cachesize"]],
                                 max_age = options()[["opengender.cacheage"]])

  cacheobj
}

#' @importFrom rappdirs user_cache_dir
og_find_cachedir <- function() {
  tmpd <- rappdirs::user_cache_dir(appname = "opengender")
  if (!dir.exists(tmpd) && !dir.create(tmpd, recursive = TRUE)) {
    tmpd <- tmpdir(check = TRUE)
    tmpd <- file.path(tmpd / opengender)
    if (!dir.exists(tmpd)) {
      dir.create(tmpd, recursive = TRUE)
    }
  }
  tmpd
}

#' @importFrom rappdirs user_cache_dir
og_find_datadir <- function() {
  tmpd <- rappdirs::user_cache_dir(appname = "opengender")
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

#' @importFrom dplyr select
#' @importFrom dplyr distinct
#' @importFrom dplyr arrange
#' @importFrom dplyr mutate
#' @importFrom dplyr arrange
#' @importFrom dplyr ungroup
#' @importFrom dplyr group_by
#' @importFrom dplyr filter
#' @importFrom tibble tibble
#' @importFrom tidyr pivot_wider

og_dict_normalize <- function(x, threshold) {
  v_req <- c("given","gender")
  v_opt <- c("country","year","n")

  # column checks

  if (!all(v_req %in% colnames(x))) {
    stop("missing required variables -- check ",
         paste(v_req[!(v_req %in% colnames(x))],","))
  }

  if (length(setdiff(colnames(x),c(v_req,v_opt)))) {
    warning("dropping additional variables")
  }

  data_norm <- x

  data_norm %<>% dplyr::select(all_of(v_req),any_of(v_opt))

  # fill missing cols
  if (!"n" %in% colnames(x) ) {
    # temporarily set to 1 for reaggregation -- set to special value at end
    data_norm %<>% dplyr::mutate(n = 1 )
  }

  if (!"year" %in% colnames(x) ) {
    data_norm %<>% dplyr::mutate( year = OG_DICT_NOYEAR)
  }

  if (!"country" %in% colnames(x) ) {
    data_norm %<>% dplyr::mutate(country = OG_DICT_NOCOUNTRY)
  }

  # clean columns: given, country, year, pr_f, N

  data_norm %<>% dplyr::mutate(given=og_clean_given(given))
  data_norm %<>%
    dplyr::bind_rows(
      tibble::tibble(given=c("[DUMMY]"), gender=c("M","F","O"))
      ) # ensure gender types are populated
  data_norm %<>% dplyr::mutate(year=og_clean_year(year))
  data_norm %<>% dplyr::mutate(country=og_clean_country(country))
  data_norm %<>% dplyr::mutate(gender=og_clean_gender(gender))

  # reaggregation by given

  data_norm %<>%
    dplyr::group_by(given,year,country) %>%
    dplyr::mutate(ng = sum(n)) %>%
    dplyr::group_by(given,gender, year,country) %>%
    dplyr::mutate(pr = n/ng) %>%
    dplyr::ungroup() %>%
    dplyr::select(given,gender,year,country,pr,n) %>%
    dplyr::distinct() %>%
    tidyr::pivot_wider(names_from="gender", names_prefix="pr_", values_from="pr", values_fill=0) %>%
    dplyr::arrange(given) %>%
    dplyr::filter(given!="[DUMMY]") %>%
    dplyr::select(given,year,country,pr_F,pr_M,n)

  # fill in missing N
  if (!"n" %in% colnames(x) ) {
    data_norm %<>%
      dplyr::mutate(n = OG_DICT_NON )
  }

  data_norm
}

og_dict_load_internal <- function(name, entry) {
  data(list=as.character(name), package = "opengender")
  ds <- eval(as.symbol(name))
  og_dict_import( ds , name, save_data = FALSE, normalize = TRUE)
  return(invisible(TRUE))
}

og_dict_import <- function(data, name, save_data=TRUE, normalize=TRUE) {
  # normalize
  if (normalize) {
    data_norm <- og_dict_normalize(data)
  }
  # insert in environment
  assign(og_dict_genname(name), data_norm ,  envir = .pkgenv)

  # TODO: save to data dir
  if (save_data) {
    saveRDS(data_norm, file = og_dict_genfilepath(name))
  }
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
  # - if save file, load it into environment, otherwise generate empty tibble
}

#' @importFrom dplyr mutate
#' @importFrom dplyr group_by
#' @importFrom dplyr summarise
#' @importFrom dplyr case_match
#' @importFrom purrr map
#' @importFrom purrr list_rbind


og_dict_combine <- function(dicts,
              missing_n_weight = options("opengender.dict.minsize")[[1]]
              ) {
  dc.df <- purrr::map(dicts, show_dict) %>% list_rbind()
  dc.df %<>% dplyr::mutate(n=dplyr::case_match(n, OG_DICT_NON ~ missing_n_weight, .default=n))

  dc.df %>%
    dplyr::mutate(n_F = pr_F * n, n_M = pr_M *n) %>%
    dplyr::group_by(given,year,country) %>%
    dplyr::summarise(n=sum(n),pr_F = sum(n_F)/n, pr_M=sum(n_M)/n) %>%
    ungroup()
}

#' @importFrom dplyr filter
og_dict_fetch_entry<-function(name) {
  val_ <- name
  dict_entry <-  .pkgenv[["dicts"]] %>% dplyr::filter(`name` == {{ val_ }} )
  if (dim(dict_entry)[[1]]==0) {
    dict_entry <- tibble::tibble()
  }
  return(dict_entry)
}

#' @import stringr
#' @importFrom stringi stri_trans_general

og_clean_given <- function(x) {
  x %>%
    stringr::str_squish() %>%
    stringr::str_to_lower() %>%
    stringi::stri_trans_general(id = "Latin-ASCII") %>%
    stringr::str_replace_all('\\s', '-') %>%
    stringr::str_remove_all('[\\W0-9--\\-]') %>%
    stringr::str_replace_all('-.', '-')
}

#' @import stringr
og_clean_gender <- function(x) {
  x %>%
    stringr::str_squish() %>%
    stringr::str_to_lower() %>%
    case_match(
      c("m","man","men","male") ~ "M",
      c("f","woman","women","female") ~ "F",
      .default = "O"
    )
}

#' @importFrom tidyr replace_na
og_clean_year<-function(x,ymin=1000,ymax=2050) {
  x %>%
    as.integer() %>%
    pmax(ymin) %>%
    pmin(ymax) %>%
    tidyr::replace_na(OG_DICT_NOYEAR)
}

#' @importFrom tidyr replace_na
#' @importFrom dplyr case_match
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
#' @importFrom dplyr mutate
#' @importFrom memoise drop_cache
og_api_call <- function(given, country, year, apikey, host, service) {
  res <- mem_og_api_call_inner(given, country, year, apikey, host, service)
  if (!res[["complete"]]) {
    memoise::drop_cache(mem_og_api_call_inner)(given, country, year, apikey, host, service)
  }
  # complete response columns

  if (!"given" %in% colnames(res)) {
    res %<>%  dplyr::mutate(given = NA)
  }
  if (!"count" %in% colnames(res)) {
    res %<>% dplyr::mutate(count = NA)
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
    res %<>% dplyr::mutate(year = year_res)
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

#' @importFrom httr2 url_build
#' @importFrom httr2 request
#' @importFrom httr2 req_retry
#' @importFrom httr2 req_perform
#' @importFrom httr2 resp_is_error
#' @importFrom httr2 resp_status
#' @importFrom httr2 resp_status_desc
#' @importFrom tibble tibble
#' @importFrom httr2 resp_body_json
#' @importFrom jsonlite fromJSON


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

  url <- httr2::url_build(uri)

  httr2::request(url) %>%
    httr2::req_retry(max_tries = options()[["opengender.retries"]]) %>%
    httr2::req_perform() -> resp

  if (httr2::resp_is_error(resp)) {
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
    rv  <- tibble::tibble(complete = FALSE)
  } else {
    httr2::resp_body_json(resp) %>%
      jsonlite::fromJSON() -> resp_body.ls

    if (resp.body.ls[["gender"]] == "male") {
      pr_f <- 1 - resp.body.ls[["probability"]]
    } else {
      pr_f <- resp.body.ls[["probability"]]
    }

    rv  <-
      tibble::tibble(pr_f = pr_f,
             given_count = resp.body.ls[["count"]],
             complete = TRUE)

  }

  rv
}



# Public: Dictionary Manipulation --------------------------------------------------------


#' Title
#'
#' @return tibble of dictionaries for matching
#' @export
#'
#' @examples [TODO]
list_dict <- function() {
  .pkgenv[["dicts"]][c("name", "desc", "type")]
}


#' Title
#'
#' @param name name of dictionary
#' @return tibble containing selected directionary
#' @export
#'
#' @examples [TODO]
show_dict <- function(name) {

  dict_entry <- og_dict_fetch_entry(name)
  if (length(dict_entry)>0) {
      load_dict(name)
  }
  rv <- get(og_dict_genname(name), envir=.pkgenv)
  rv
}

#' Title
#'
#' @param name name parameter
#' @param force Force reload
#'
#' @return logical success
#' @export
#'
#' @examples [TODO]
load_dict <- function(name , force = FALSE) {
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
#' @param data tibble
#' @param name dictionary name
#' @param save permanently save
#'
#' @return logical success
#' @export
#'
#' @examples [TODO]
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
#' @param cleancache logical
#' @param cleandata logical
#'
#' @return none
#' @export
#'
#' @examples [TODO]
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

#' add_gender_predictions
#'
#' @param col_map matching columns to impute
#' @param dicts list of dictionaries
#' @param save_api_results save api results
#' @param fuzzy_match employ fuzzy matching
#' @param year_adjust year adjustments
#'
#' @return tibble with imputed gender as probability
#' @export
#' @importFrom dplyr select
#' @importFrom dplyr rename
#' @importFrom dplyr mutate
#' @importFrom dplyr left_join
#' @importFrom dplyr anti_join
#' @importFrom dplyr inner_join
#' @importFrom dplyr coalesce
#' @importFrom fuzzyjoin stringdist_join
#' @importFrom tidyr nest

#'
#' @examples [TODO]

add_gender_predictions <- function(x, col_map = c(given="given", year="", country=""),
                          dicts = c("kantro"),
                          save_api_results = TRUE,
                          fuzzy_match = TRUE,
                          year_adjust = TRUE) {

  all_ind <- c("given","year","country")
  cmp <- col_map[col_map!=""]
  cmp <- cmp[names(cmp) %in% all_ind]

  if (!"given" %in% names(cmp)) {
    stop("must supply a column of given names")
  }

  if (!all(names(col_map) %in% all_ind)) {
    warning("additional columns in col_map",
            paste(names(col_map)[!name(cpl_map) %in% all_ind])
    )
  }

  dicts.tbl <- og_dict_combine(unique(dicts))

  # create reduced normalized input table

  cmp_r <- setNames(names(cmp),cmp)
  cmp_nm <- names(cmp)
  cmp_nm_r <- names(cmp_r)
  cmp_g <- cmp[["given"]]

  x_norm.df <- x %>%
    dplyr::select( {{cmp}} )

  # extract names before normalizing
  x_nms_match.df <- x_norm.df %>%
    dplyr::select(given_input=given) %>%
    dplyr::distinct() %>%
    dplyr::mutate(
      given_match=og_clean_given(given_input))

  x_norm.df %<>% mutate(gender="O") # temporary dummy for normalization

  x_norm.df %<>%
    og_dict_normalize() %>%
    dplyr::select(all_ind)

  ### fuzzy_match normalization

  # two-phase fuzzy join -- fuzziness should extend to name only

  if (fuzzy_match) {

    unmatched_givens.df <- x_norm.df %>%
      dplyr::anti_join(dicts.tbl %>% select(given),
                       by=c("given"))

    fuzzy_match.df <- unmatched_givens.df  %>%
      dplyr::select(given_clean=given) %>%
      fuzzyjoin::stringdist_inner_join(
      dicts.tbl %>%
        dplyr::select(given) ,
      by = c(given_clean="given") ,
      distance_col = "fuzzy_dist",
      method = "cosine",
      max_dist = options()[["opengender.fuzzymax"]]
    )  %>%
    dplyr::rename(given_fuzzy=given) %>%
    dplyr::group_by(given_clean) %>%
    dplyr::slice_min(order_by = fuzzy_dist, n = 1) %>%
    dplyr::slice_head(n = 1)  %>%
    dplyr::ungroup()

    x_norm.df %<>%
      dplyr::left_join(fuzzy_match.df ,
                       by=c(given="given_clean")) %>%
      dplyr::mutate(given_fuzzy=dplyr::coalesce(given_fuzzy,given)) %>%
      dplyr::select(given=given_fuzzy,year,country,fuzzy_dist)

    x_nms_match.df %<>%
      dplyr::left_join(fuzzy_match.df,
                       by=c(given_match="given_clean")) %>%
      dplyr::mutate(given_fuzzy=dplyr::coalesce(given_fuzzy,given_match)) %>%
      dplyr::select(given_input, given_match=given_fuzzy,fuzzy_dist)

  } else {
    x_norm.df %<>%
      mutate(given=given_clean,
             fuzzy_dist=NA_real_)
  }


  ### match to static dictionaries

  # full match

  match_cur.df <-
    x_norm.df %>%
    dplyr::inner_join(dicts.tbl,
                     by = all_ind
                     )

  match_cum.df <- match_cur.df

  # TODO: incomplete match to country

  if(FALSE) {
  unmatched.df <-
    dplyr::anti_join( x_norm.df, match_cum.df, by=all_ind)

    # join to unmatched
   match_cur.df <- unmatched.df %>%
     dplyr::inner_join(dicts.tbl,
                       by = all_ind
     )

   match_cum.df %<>% dplyr::bind_rows(match_cur.df)
  }

  # TODO: year interpolation

  # TODO: API retrieval of remainder


  ### combine results and rejoin to input

  match_cum.df %<>%
    dplyr::mutate(og_pr_F=pr_F) %>%
    tidyr::nest(og_details= c(pr_F,pr_M,n,fuzzy_dist)) %>%
    dplyr::select( {{cmp_nm}}, og_details, og_pr_F)

  byspec <- "given_input"; names(byspec) <- cmp_g
  rejoined.df <-
    dplyr::left_join(x, x_nms_match.df,
                     by= byspec)

  rejoined.df %<>% dplyr::left_join(match_cum.df,
                           by = c(given_match="given"))  %>%
  dplyr::select(!given_match)

  rejoined.df
}

# Public: Estimation --------------------------------------------------------

#' Title
#'
#' @param x (grouped) data frame with instrumented by impute
#' @param simplify return values
#'
#' @return mean value of each
#' @export
#'
#' @examples [TODO]
gender_mean <- function(x, simplify = TRUE) {

}

#' Title
#'
#' @param x data frame with instrumented by impute
#' @param simplify return values
#'
#' @return standard error value of each
#' @export
#'
#' @examples [TODO]
gender_se <- function(x,  simplify = TRUE) {

}

#' Title
#'
#' @param x  data frame with instrumented by impute
#' @param cl  confidence limit for interval
#' @param simplify return values
#'
#' @return mean tibble of upper lower values
#' @export
#'
#' @examples [TODO]
gender_ci <- function(x, cl = .05,  simplify =TRUE) {

}

#' Title
#'
#' @param x data frame with instrumented by impute
#' @param n number of imputations to draw
#'
#' @return list of imputation of gender
#' @export
#'
#' @examples [TODO]
gender_imp <- function(x, n=1) {

}

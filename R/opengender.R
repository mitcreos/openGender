#' @importFrom magrittr "%<>%"
#' @importFrom magrittr "%>%"
#' @import rlang

# Package: Constants --------------------------------------------------------
OG_DICT_EXT <- "_dict"
OG_NORM_EXT <- "_norm"
OG_DESC_EXT <- "_desc"
OG_DESC_FILE_EXT <- ".rds"
OG_NORM_FILE_EXT <- ".rds"
OG_DICT_FILE_EXT <- ".rds"
OG_DICT_NOYEAR <- 3000
OG_DICT_ANYYEAR <- 10000
OG_DICT_NOCOUNTRY <- "00"
OG_DICT_ANYCOUNTRY <- "99"
OG_DICT_NON <- 0
OG_GENDER_LEVELS <- c("F","M","O")
OG_ORGNAMETYPES <- c("canonical","full","short","other")

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
    opengender.dict.minsize = 10,
    opengender.bootreps = 1000
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
#' @importFrom stringr str_extract
#' @importFrom tibble tibble
#' @importFrom tibble as_tibble
#' @importFrom dplyr pull
#' @importFrom dplyr bind_rows
#' @importFrom dplyr rows_patch
#' @importFrom dplyr mutate
og_init_dictlist<-function() {
  core.df <- tibble::tribble(
    ~name, ~desc, ~version, ~type, ~custom_fun,  ~uri, ~domain,
    "wgen2",   "world gender dictionary", "2", "external", "wgen2", "https://dataverse.harvard.edu/api/access/datafile/4750352", "gender",
    "ssa","cumulative social security admin","2024","external","ssa","https://www.ssa.gov/oact/babynames/names.zip", "gender",
    "kantro",  "kantrowitz  NLTK dictionary", "1", "internal", "", "https://raw.githubusercontent.com/nltk/nltk_data/gh-pages/packages/corpora/names.zip", "gender",
    "ror",  "research organization registry", "1.58-2024-12-11", "external", "ror", "https://zenodo.org/records/14429114/files/v1.58-2024-12-11-ror-data.zip", "organization",
    "iso3166",  "iso country codes", "v2", "internal", "iso3166", "https://en.wikipedia.org/wiki/List_of_ISO_3166_country_codes", "organization",
    "genderize",  "genderize", "1",  "api", "genderize", "https:://api.genderize.io", "gender",
  ) %>%
    dplyr::mutate( loaded=FALSE)

  load_dir <- options("opengender.datadir")[[1]]
  loaded <- dir(path=load_dir, pattern =paste0('^.*', OG_DICT_EXT, OG_DICT_FILE_EXT))
  loaded_names <- stringr::str_replace(loaded,
                                       pattern =paste0('^(','.*',')', OG_DICT_EXT, OG_DICT_FILE_EXT), "\\1")
  added_dicts <- setdiff(loaded_names, core.df %>% dplyr::pull(name))

  added_dicts.df <- tibble::tibble(name=added_dicts, type="added", loaded=FALSE)
  res <- dplyr::bind_rows(core.df, added_dicts.df)

  desc_files <- dir(path=load_dir, pattern = paste0('^.*', OG_DESC_EXT, OG_DESC_FILE_EXT) )
  desc.df <- purrr::map(desc_files, function(x) {
      v<-readRDS(file.path(load_dir, x)) %>% tibble::as_tibble()
      nm <- stringr::str_replace(x,
          pattern = paste0('^(','.*',')', OG_DESC_EXT, OG_DESC_FILE_EXT), "\\1")
      v %>% dplyr::mutate(name=nm)
    }) %>%  purrr::list_rbind()

  if (nrow(desc.df)>0) {
    res %<>% dplyr::rows_patch(desc.df %>%
               dplyr::select(intersect(colnames(res),colnames(desc.df))),
                                      by="name",
               unmatched = "ignore")
  }
  res
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
    tmpd <- file.path(tmpd,"opengender")
    if (!dir.exists(tmpd)) {
      dir.create(tmpd, recursive = TRUE)
    }
  }
  tmpd
}

#' @importFrom rappdirs user_data_dir
og_find_datadir <- function() {
  tmpd <- rappdirs::user_data_dir(appname = "opengender")
  if (!dir.exists(tmpd) && !dir.create(tmpd, recursive = TRUE)) {
    tmpd <- tmpdir(check = TRUE)
    tmpd <- file.path(tmpd,"opengender")
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

og_dict_normalize <- function(x, min_count_default=1) {

  dict_domain =   attr(x,"domain")
  if (is.null(dict_domain)) {
    stop("dictionary domain not specified")
  }

  if (dict_domain == "gender") {
    v_req <- c("given","gender")
    v_opt <- c("country","year","n")
  } else if (dict_domain=="organization") {
    v_req <- c("name","id","type")
    v_opt <- c("country","year","n")
  } else {
    stop("unknown domain: ", dict_domain)
  }

  # column checks

  if (!all(v_req %in% colnames(x))) {
    stop("missing required variables -- check ",
         paste(v_req[!(v_req %in% colnames(x))],","))
  }

  if (length(setdiff(colnames(x),c(v_req,v_opt)))) {
    warning("dropping additional variables")
  }

  data_norm <- x

  if (!is.null(attr(data_norm,"min_obs_threshhold"))) {
    min_count_default <- attr(data_norm,"min_obs_threshold")
  }

  data_norm %<>% dplyr::select(all_of(v_req),any_of(v_opt))

  # fill missing cols

  if (!"year" %in% colnames(x) ) {
    data_norm %<>% dplyr::mutate( year = OG_DICT_ANYYEAR)
  } else {
    data_norm %<>% dplyr::mutate(year=og_clean_year(year))
  }

  if (!"country" %in% colnames(x)) {
    data_norm %<>% dplyr::mutate(country = OG_DICT_ANYCOUNTRY)
  }

  # clean columns: given, country, year, gender, N
  data_norm %<>% dplyr::mutate(country=og_clean_country(country))
  if ("given" %in% colnames(x)) {
    data_norm %<>% dplyr::mutate(given=og_clean_given(given))
  }
  if ("gender" %in% colnames(x)) {
      data_norm %<>% dplyr::mutate(gender=og_clean_gender(gender))
  }
  if ("orgtype" %in% colnames(x)) {
    data_norm %<>% dplyr::mutate(type=og_clean_orgtype(type))
  }

  if (dict_domain == "gender") {

    if (!"n" %in% colnames(x) ) {
      # temporarily set to 1 for reaggregation -- set to special value at end
      data_norm %<>% dplyr::mutate(n = 1 )
    }

    if ("n" %in% colnames(x) ) {
      data_norm %>%
        dplyr::group_by(given) %>%
        dplyr::summarize(total =sum(n)) ->
        grand_totals.df

      data_norm %<>% dplyr::anti_join(grand_totals.df %>%
                                        dplyr::filter(total < min_count_default), by=c(n="total"))
    }

    #TODO: OG_GENDER_LEVELS
    data_norm %<>% dplyr::bind_rows(
      tibble::tibble(given="[DUMMY]", gender=c("F","O","M"),
                     year = OG_DICT_NOYEAR , country = OG_DICT_NOCOUNTRY,
                     n = 0)
      # ensure dictionary has full range of gender codes
    )

    # aggregate multiple observations with same keys
    # to support matches when country, year, is not supplied

    data_norm %<>%
      na.omit() %>%
      dplyr::group_by(given,gender, year,country ) %>%
      dplyr::summarize(n = sum(n), .groups="drop")

    agg_all <- NULL
    agg_geo <- NULL
    agg_time <- NULL

    ## add aggregates
    # if either year or country column
    if (("year" %in% colnames(x)) ||
        ("country" %in% colnames(x)))  {

      data_norm %>%
        dplyr::group_by(given,gender) %>%
        dplyr::summarize(n=sum(n), .groups="drop") %>%
        dplyr::mutate(year=OG_DICT_ANYYEAR,
                      country=OG_DICT_ANYCOUNTRY) -> agg_all
    }

    if (("country" %in% colnames(x)))  {
      data_norm %>%
        dplyr::group_by(given,gender,year) %>%
        dplyr::summarize(n=sum(n), .groups="drop") %>%
        dplyr::mutate(country=OG_DICT_ANYCOUNTRY) -> agg_geo
    }

    if (("year" %in% colnames(x)))  {
      data_norm %>%
        dplyr::group_by(given,gender,country) %>%
        dplyr::summarize(n=sum(n), .groups="drop") %>%
        dplyr::mutate(year=OG_DICT_ANYYEAR) -> agg_time
    }

    #TODO: OG_GENDER_LEVELS
    data_norm <-
      dplyr::bind_rows(data_norm,agg_all, agg_geo, agg_time) %>%
      dplyr::distinct() %>%
      dplyr::arrange(given) %>%
      tidyr::pivot_wider(names_from="gender",
                         names_prefix="",
                         values_from="n",
                         values_fill=0) %>%
      dplyr::filter(given!="[DUMMY]") %>%
      dplyr::mutate(n= F + M + O,
                    across(c(F,M,O) , ~ .x /n , .names = "pr_{.col}" )
      ) %>%
      dplyr::select(given,year,country,pr_F,pr_M,pr_O,n)


    # fill in missing N
    if (!"n" %in% colnames(x) ) {
      data_norm %<>%
        dplyr::mutate(n = OG_DICT_NON )
    }

  }

  data_norm
}

og_list_dict_internal<-function() {
  .pkgenv[["dicts"]]
}

og_dict_load_internal <- function(name, entry) {
  dict_file <- og_dict_genfilepath(name)
  if (file.exists(dict_file)) {
    og_dict_import(name=name)
  } else {
    data(list=as.character(name), package = "opengender")
    ds <- eval(as.symbol(name))
    attr(ds,"domain") <- entry[["domain"]]
    attr(ds,"version") <- entry[["version"]]
    comment(ds)<-entry[["desc"]]
    if (entry[["custom_fun"]]!="") {
      ds <- do.call(paste0("og_dict_process_", entry[[1, "custom_fun"]]),
                    args = list(x=ds))
    }
    og_dict_import( x = ds , name = name )
  }
}

#' @importFrom dplyr bind_rows
#' @importFrom tibble tibble

og_dict_import <- function(x, name, renormalize = FALSE) {
  dict_file <- og_dict_genfilepath(name)
  norm_file <- og_dict_gennormfilepath(name)
  desc_file <- og_dict_gendescfilepath(name)

  if (!file.exists(norm_file)) {
    renormalize <- TRUE
  }
    # load data if not sent in
  if(missing(x) || is.null(x)) {
    x <- readRDS(dict_file)
  } else {
    renormalize <- TRUE
    saveRDS(x, file = dict_file)
  }

  if(attr(x,"domain")=="gender") {
    attr(x,"levels") == OG_GENDER_LEVELS
  }

  # normalize
  if (renormalize) {
    ds_norm <- og_dict_normalize(x)
    comment(ds_norm)<-comment(x)
    attr(ds_norm,"domain") <- attr(x,"domain")
    attr(ds_norm,"version") <- attr(x,"version")
    attr(ds_norm,"levels") <- attr(x,"levels")

    saveRDS(ds_norm, file=norm_file)
  } else {
    ds_norm <- readRDS(norm_file)
  }

  # insert in environment
  assign(og_dict_gennormname(name), ds_norm  ,  envir = .pkgenv)

  #update metatdata
  tmpcat <- .pkgenv[["dicts"]]
  up.df <- tibble::tibble(name=name,desc=comment(ds_norm),type="added",loaded=TRUE,
                          domain = attr(ds_norm,"domain"), version=attr(ds_norm,"version"))

  if (name %in% tmpcat[["name"]]) {
    tmpcat %<>%
      dplyr::rows_patch(
        up.df,
        by="name"
      )
  } else {
    tmpcat %<>%
      dplyr::bind_rows(
        up.df
      )
  }

  #update description file
  saveRDS(up.df, file=desc_file)

  .pkgenv[["dicts"]] <- tmpcat
}

og_dict_load_added <- function(name, entry) {
  og_dict_import(name=name)
}

og_dict_load_external <- function(name, entry) {
  dict_file <- og_dict_genfilepath(name)
  if (file.exists(dict_file)) {
    og_dict_import(name=name)
  } else {

    tmp_file <- tempfile()
    download.file(entry[[1,"uri"]], tmp_file)

    if (entry[["custom_fun"]]=="") {
      ds <- readRDS(tmp_file)
      comment(ds) <- entry[[1,"uri"]]
    } else {
      ds <- do.call(paste0("og_dict_process_", entry[[1, "custom_fun"]]),
              args = list(src=tmp_file))
    }
    og_dict_import( x = ds , name = name )
  }
}

og_dict_gendictname<- function(name = "") {
  paste0(name, OG_DICT_EXT)
}

og_dict_gennormname<- function(name = "") {
  paste0(name, OG_NORM_EXT)
}

og_dict_gendescname<- function(name = "") {
  paste0(name, OG_DESC_EXT)
}

og_dict_genfilepath<-function(name) {
  load_dir <- options("opengender.datadir")[[1]]
  rv <- file.path(load_dir,paste0(og_dict_gendictname(name), OG_DICT_FILE_EXT))
  rv
}

og_dict_gennormfilepath<-function(name) {
  load_dir <- options("opengender.datadir")[[1]]
  rv <- file.path(load_dir,paste0(og_dict_gennormname(name), OG_NORM_FILE_EXT))
  rv
}

og_dict_gendescfilepath<-function(name) {
  load_dir <- options("opengender.datadir")[[1]]
  rv <- file.path(load_dir,paste0(og_dict_gendescname(name), OG_DESC_FILE_EXT))
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
              missing_n_weight = options("opengender.dict.minsize")[[1]],
              dict_domain = "gender"
              ) {
  #TODO: OG_GENDER_LEVELS

  udicts <- unique(dicts)

  dc.df <- purrr::map(udicts, show_dict) %>% list_rbind()

  if (dict_domain=="gender") {
    dc.df %<>% dplyr::mutate(n=dplyr::case_match(n, OG_DICT_NON ~ missing_n_weight, .default=n))

    res <- dc.df %>%
      dplyr::mutate(n_F = pr_F * n, n_M = pr_M *n,
                    n_O = pr_O*n) %>%
      dplyr::group_by(given,year,country) %>%
      dplyr::summarise(n=sum(n),pr_F = sum(n_F)/n, pr_M=sum(n_M)/n,
                       pr_O=sum(n_O)/n) %>%
      ungroup() %>%
      filter(n>=missing_n_weight)
  } else {
    res <- dc.df
  }

   res
 }

#' @importFrom dplyr filter
og_dict_fetch_entry<-function(name) {
  val_ <- name

  dict_entry <-
    og_list_dict_internal() %>% dplyr::filter(`name` == {{ val_ }} )

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
      c("f","woman","women","female") ~ OG_GENDER_LEVELS[[1]],
      c("m","man","men","male") ~ OG_GENDER_LEVELS[[2]],
      NA ~ NA,
      .default = OG_GENDER_LEVELS[[3]]
    ) %>%
    factor(levels=OG_GENDER_LEVELS)
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
  codes <- c(opengender::iso3166[["country_2d"]],OG_DICT_NOCOUNTRY,OG_DICT_ANYCOUNTRY)
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


#' @importFrom tidyr replace_na
#' @importFrom dplyr case_match
og_clean_orgtype <- function(x) {
  codes <- OG_ORGNAMETYPES
  rv <- as.character(x)
  mismatch <- setdiff(unique(rv),codes)
  if (length(mismatch)>0) {
    warning("country codes not matched to org types", paste(mismatch, sep=","))
  }
  rv %>%
    dplyr::case_match(
      mismatch ~ tail(codes,n=1),
      .default = x
    ) %>%
    tidyr::replace_na(tail(codes,n=1))
}

# Internals: dictionary-specific source extraction  --------------------------------------------------------

#' @importFrom dplyr select
#' @importFrom dplyr mutate
#' @importFrom dplyr case_match
#' @importFrom dplyr na_if
#' @importFrom readr read_tsv
#' @importFrom readr col_character
#' @importFrom readr col_integer
#' @importFrom readr col_double
og_dict_process_wgen2 <- function(src) {

  # Coding notes:
  #   After expert inspection of data, applied coding rules:
  #   - assigned column types
  #   - recoded old country codes and use of fips codes insteda of iso
  #   - normalized undocumented use of '?' as NA
  #   - renamed variables for alignment with ingest
  #   - stray non-integer counts
  #   - a huge proportion of names are one-offs -- filtered as unreliable

  min_obs <- 4

  raw.df <- readr::read_tsv(src, col_types= c(
    name = readr::col_character(),
    code = readr::col_character(),
    gender = readr::col_character(),
    wgt = readr::col_double(),
    nobs = readr::col_integer(),
    src = readr::col_character()
  ))

  raw.df %<>%
    dplyr::select(given=name,country=code,n=nobs,gender,) %>%
    dplyr::mutate(country = dplyr::case_match(country,
                                              "BU" ~ "MM",
                                              "CB" ~ "KH",
                                              "KS" ~ "KR",
                                              "??" ~ OG_DICT_NOCOUNTRY,
                                              .default = country),
                  gender = dplyr::na_if(gender,"?"),
                  n=as.integer(n))

  attr(raw.df,"min_obs_threshhold") <- min_obs
  attr(raw.df,"version") <- "1"
  attr(raw.df,"domain") <- "gender"

  comment(raw.df) <- "world gender dictionary"
  raw.df
}

#' @importFrom dplyr select
#' @importFrom dplyr mutate

og_dict_process_iso3166 <- function(x) {

  # Coding notes:
  #   After expert inspection of data, applied coding rules:
  #   -

  raw.df <- x %>%
    dplyr::select(id=country_num,name=country_name, country=country_2d) %>%
    dplyr::mutate(type=OG_ORGNAMETYPES[[1]])
  raw.df
}

#' @importFrom dplyr select
#' @importFrom dplyr mutate
#' @importFrom dplyr na_if
#' @importFrom purrr map
#' @importFrom purrr list_rbind
#' @importFrom stringr str_extract
#' @importFrom utils unzip
#' @importFrom readr read_csv
#' @importFrom readr col_character
#' @importFrom readr col_integer
og_dict_process_ssa <- function(src) {

  # Coding notes:
  #   After expert inspection of data, applied coding rules:
  #   -

  min_obs <- 10

  filesls.df <-
    utils::unzip(zipfile=src, list=TRUE, unzip=getOption("unzip"))


  files.ls<- filesls.df %>%
    dplyr::filter(stringr::str_starts(Name,"yob")) %>%
    dplyr::pull(Name)

  read_ssa_single <-function(fn) {
    fy<- stringr::str_extract(fn, 'yob(\\d+)\\.txt', group=1)
    df<-readr::read_csv(unz(src,filename=fn),
                        col_names= c("given","gender","n"),
                        col_types= c(
                          given = readr::col_character(),
                          gender = readr::col_character(),
                          n = readr::col_integer()
                        )) %>% dplyr::mutate(year=fy)
    df
  }

  raw.df <- purrr::map(files.ls, read_ssa_single) %>%
    purrr::list_rbind() %>%
    dplyr::mutate(country="US")

  attr(raw.df,"min_obs_threshhold") <- min_obs
  attr(raw.df,"version") <- "1"
  attr(raw.df,"domain") <- "gender"

  comment(raw.df) <- "US social security admin baby names"
  raw.df
}

#' @importFrom dplyr select
#' @importFrom dplyr filter
#' @importFrom dplyr mutate
#' @importFrom dplyr arrange
#' @importFrom dplyr distinct
#' @importFrom stringr str_ends
#' @importFrom stringr str_squish
#' @importFrom tidyr separate_longer_delim
#' @importFrom utils unzip
#' @importFrom readr read_csv
#' @importFrom readr col_integer
#' @importFrom readr cols_only

og_dict_process_ror <- function(src) {

  # Coding notes:
  #   After expert inspection of data, applied coding rules:
  #   -

  min_obs <- 1

  filesls.df <-
    utils::unzip(zipfile=src, list=TRUE, unzip=getOption("unzip"))
  filesuffix <- "-ror-data.csv"


  rorfn <- filesls.df %>%
    dplyr::filter(stringr::str_ends(Name,filesuffix)) %>%
    dplyr::pull(Name)


  raw.df <- readr::read_csv(unz(src,filename=rorfn),
                            col_types=readr::cols_only(
                              id = readr::col_character(),
                              name = readr::col_character(),
                              aliases = readr::col_character(),
                              labels = readr::col_character(),
                              acronyms = readr::col_character(),
                              country.country_code = readr::col_character()
                            ))

  # unpack multiple names

  names.df <-
    raw.df %>%
    dplyr::select(id,country=country.country_code,name=name) %>%
    dplyr::filter(!is.na(name)) %>%
    tidyr::separate_longer_delim(delim=";", cols=c(name)) %>%
    dplyr::filter(!is.na(name)) %>%
    dplyr::mutate(type="canonical")

  aliases.df <-
    raw.df %>%
    dplyr::select(id,country=country.country_code,name=aliases) %>%
    dplyr::filter(!is.na(name)) %>%
    tidyr::separate_longer_delim(delim=";", cols=c(name)) %>%
    dplyr::filter(!is.na(name))   %>%
    dplyr::mutate(type="full")

  labels.df <-
    raw.df %>%
    dplyr::select(id,country=country.country_code,name=labels) %>%
    dplyr::filter(!is.na(name)) %>%
    tidyr::separate_longer_delim(delim=";", cols=c(name)) %>%
    dplyr::filter(!is.na(name)) %>%
    dplyr::mutate(name=stringr::str_remove(name,"^(\\s*\\w*: )")) %>%
    dplyr::mutate(type="full")


  acro.df <-
    raw.df %>%
    dplyr::select(id,country=country.country_code,name=acronyms) %>%
    dplyr::filter(!is.na(name)) %>%
    tidyr::separate_longer_delim(delim=";", cols=c(name)) %>%
    dplyr::filter(!is.na(name))  %>%
    dplyr::mutate(type="short")

  rv <-
    dplyr::bind_rows(names.df, aliases.df, acro.df, labels.df) %>%
    dplyr::filter(!is.na(name)) %>%
    dplyr::mutate(name=stringr::str_squish(name)) %>%
    dplyr::distinct() %>%
    dplyr::mutate(type=factor(type)) %>%
    dplyr::mutate(country=og_clean_country(country)) %>%
    dplyr::arrange(id,name)

  # clean country_code

  # instrument dictionary
  attr(rv,"min_obs_threshhold") <- min_obs
  version <- stringr::str_extract(rorfn,paste0("(.*)",filesuffix), group=1)
  attr(rv,"version") <- version
  attr(rv,"domain") <- "organization"

  comment(rv) <- "registry of research organizations name file"
  rv
}

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
      year_res <- OG_DICT_NOYEAR
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

# Internals: estimation functions  --------------------------------------------------------

#' @importFrom purrr map
#' @importFrom tibble tibble
#' @importFrom tidyr unnest
#' @importFrom dplyr mutate
#' @importFrom dplyr pull
#' @importFrom dplyr summarize
#' @importFrom rsample bootstraps
#' @importFrom rsample analysis
#' @importFrom rsample int_bca

og_mn_boot <- function(x, rep=options()[["opengender.bootreps"]]) {
  if (!inherits(x,"data.frame")) {
    x <- tibble::tibble(value=x)
  }

  est_fun <- function(dat) {
    dat  %>%
      dplyr::summarize(estimate=mean(value,na.rm=TRUE)) %>%
      dplyr::pull(estimate)
  }

  se_fun <- function(dat) {
    tmp <- dat  %>%
      dplyr::summarize(sd=sd(value,na.rm=TRUE),
                       adjn=sum(!is.na(value)) ) %>%
      dplyr::mutate(estimate = sqrt(sd/adjn) ) %>%
      dplyr::pull(estimate)
  }

  term_fun <- function(split, termname="termx", ...) {
    est_split <- split %>%
      rsample::analysis() %>%
      est_fun

    se_split <- split %>%
      rsample::analysis() %>%
      se_fun

    tibble::tibble(
      term = termname,
      estimate = est_split,
      std.error = se_split
    )
  }
  x_rs <- x %>%
    rsample::bootstraps( times = rep, apparent = TRUE) %>%
    dplyr::mutate(models =
                    purrr::map(splits, function(x)term_fun(x,termname="mean")))

  boot_se <-
    x_rs %>%
    dplyr::select(models) %>%
    tidyr::unnest(models) %>%
    dplyr::summarize(estimate=mean(std.error,na.rm=TRUE)) %>%
    dplyr::pull()

  bca_res <- rsample::int_bca(x_rs, models, .fn=term_fun, termname="mean")
  tibble::tibble(term = c("imputed","lower","upper","se"),
                 estimate= c(bca_res$.estimate,
                 ymin=bca_res$.lower,
                 ymax=bca_res$.upper,
                 yse=boot_se))
}

# Public: Dictionary Manipulation --------------------------------------------------------


#' Title
#'
#' @return tibble of dictionaries for matching
#' @export
#'
#' @examples [TODO]
#' @importFrom dplyr select

list_dicts <- function() {
  res<- og_list_dict_internal() %>% dplyr::select(name,desc,type,loaded,domain)
  res
}

#' Title
#'
#' @param name name of dictionary
#' @return tibble containing selected directionary
#' @export
#'
#' @examples [TODO]
show_dict <- function(name) {

  load_dict(name)
  rv <- get(og_dict_gennormname(name), envir=.pkgenv)
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

  if (!force && exists(og_dict_gennormname(name), envir = .pkgenv)) {
    return(TRUE)
  }

  rv <- do.call(paste0("og_dict_load_", dict_entry[[1, "type"]]),
                args = list(name=name, entry = dict_entry))
  return(rv)
}

#' manage_local_dicts
#'
#' @param data tibble
#' @param name dictionary name
#' @param delete  permanently delete
#' @param description description
#' @param force force replacing existing added dictionary
#' @param min_obs mininum observations per cell to use when normalizing

#'
#' @return logical success
#' @export
#'
#' @examples [TODO]
manage_local_dicts <- function(x, name = "local_1", description="a local dictionary",
                              delete=FALSE, force=FALSE, min_obs = options("opengender.dict.minsize")[[1]],
                              domain  = "gender", version = "1", levels=NULL) {

  if (delete) {
    if (!is.null(x) || !force) {
      stop("use x=NULL, force=TRUE, delete=TRUE for deletions")
    }
   file.remove(og_dict_gennormfilepath(name))
   file.remove(og_dict_genfilepath(name))
   file.remove(og_dict_gendescfilepath(name))

  }

  dict_entry <- og_dict_fetch_entry(name)
  if (length(dict_entry)!=0) {
    if (dict_entry[["type"]]!="added") {
      stop("Cannot update non-local dictionaries")
    } else if (!force) {
      stop("Dictionary exists, use force=TRUE to replace")
    }
  }

  comment(x) <- description
  attr(x,"min_obs_threshhold") <- min_obs
  attr(x,"domain") <- domain
  attr(x,"version") <-  version
  if (!is.null(levels) && domain != "gender") {
    attr(x,"levels")= levels
  }

  og_dict_import(
    x = x ,
    name = name
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
                        cleannorm = TRUE,
                        cleandata = FALSE,
                        cleandesc = FALSE ) {

  if (cleandata) {
    file.remove(dir(options()[["opengender.datadir"]],
                    full.names=TRUE,
                    pattern = paste0('.*', OG_DICT_EXT, OG_DICT_FILE_EXT)))
    cleannorm <- TRUE
  }
  if (cleannorm) {
    file.remove(dir(options()[["opengender.datadir"]],
                    full.names=TRUE,
                    pattern = paste0('.*', OG_NORM_EXT, OG_NORM_FILE_EXT)))
    loaded_dicts <- .pkgenv %>% ls() %>% grep("_norm",.,value=TRUE)
    rm(list=loaded_dicts,envir=.pkgenv)
    .pkgenv[["dicts"]] <- og_init_dictlist()
    cleandesc<- TRUE
  }
  if (cleandesc) {
    file.remove(dir(options()[["opengender.datadir"]],
                    full.names=TRUE,
                    pattern = paste0('.*', OG_DESC_EXT, OG_DESC_FILE_EXT)))
  }
  if (cleancache) {
    .pkgenv[["cacheobj"]]$reset()
  }
}

# Public: matching and analysis  --------------------------------------------------------

#' add_gender_predictions
#'
#' @param x input data frame
#' @param col_map matching columns to impute
#' @param dicts list of dictionaries
#' @param save_api_results save api results
#' @param fuzzy_match employ fuzzy matching
#' @param year_adjust year adjustments
#'
#' @return tibble with imputed gender as probability
#' @export


#'
#' @examples [TODO]

add_gender_predictions <- function(x, col_map = c(given="given", year="year", country="country"),
                          dicts = c("kantro"),
                          save_api_results = TRUE,
                          fuzzy_match = TRUE,
                          year_adjust = TRUE) {

    add_category_predictions(x=x, col_map = col_map,
                             dicts =dicts, save_api_results=save_api_results,
                             fuzzy_match = fuzzy_match, year_adjust=year_adjust,
                             dict_domain = "gender",
                             domain_levels = OG_GENDER_LEVELS,
                             all_keys = c("given","year","country"),
                             output_vars = c("og_pr_F","og_gender_details")
                             )

}

#' @importFrom dplyr select
#' @importFrom dplyr rename
#' @importFrom dplyr filter
#' @importFrom dplyr mutate
#' @importFrom dplyr left_join
#' @importFrom dplyr anti_join
#' @importFrom dplyr any_of
#' @importFrom dplyr all_of
#' @importFrom dplyr inner_join
#' @importFrom dplyr coalesce
#' @importFrom dplyr join_by
#' @importFrom dplyr bind_cols
#' @importFrom dplyr bind_rows
#' @importFrom tibble as_tibble
#' @importFrom tibble tibble
#' @importFrom fuzzyjoin stringdist_join
#' @importFrom tidyr nest
#' @importFrom tidyr unnest

add_category_predictions <- function(x,
                                   col_map,
                                   dicts,
                                   save_api_results,
                                   fuzzy_match,
                                   year_adjust,
                                   dict_domain,
                                   domain_levels,
                                   all_keys,
                                   output_vars
                                  ) {

  cmp <- col_map[col_map!=""]
  cmp <- cmp[names(cmp) %in% all_keys]

  x %<>% dplyr::select(!dplyr::any_of(output_vars))

  primary_key <- all_keys[1]

  if (!primary_key %in% names(cmp)) {
    stop("must supply a primary key", primary_key)
  }

  if (!all(names(col_map) %in% all_keys)) {
    warning("additional columns in col_map",
            paste(names(col_map)[!name(col_map) %in% all_keys])
    )
  }

  dicts.tbl <- og_dict_combine(unique(dicts))

  # create reduced normalized input table

  cmp_r <- setNames(names(cmp),cmp)
  cmp_nm <- names(cmp)
  cmp_nm_r <- names(cmp_r)
  cmp_primary <- cmp[[primary_key]]

  x_norm.df <- x %>%
    dplyr::select( {{cmp}} )

  # extract names before normalizing
  x_nms_match.df <- x_norm.df %>%
    dplyr::select(primary_input = {{ primary_key }}) %>%
    dplyr::distinct()

  ### GENDER MATCHING PHASE
  x_nms_match.df   %<>%
    dplyr::mutate(
      primary_match=og_clean_given(primary_input))

  x_norm.df %<>% mutate(gender="O") # temporary dummy for normalization

  attr(x_norm.df,"domain") <- dict_domain
  x_norm.df %<>%
    og_dict_normalize() %>%
    dplyr::select(all_keys)

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
                       by=c(primary_match="given_clean")) %>%
      dplyr::mutate(given_fuzzy=dplyr::coalesce(given_fuzzy,primary_match)) %>%
      dplyr::select(primary_input, primary_match=given_fuzzy)

  } else {
    x_norm.df %<>%
      mutate(given=given_clean,
             fuzzy_dist=NA_real_)
  }


  ### match to static dictionaries

  # full match
  # Note:
  #  - incomplete match to country - accomplished through pre-processing to
  #         missing country in og_dict_normalize
  #  - fuzzy match to names -- accomplished by augmenting x_norm.df with fuzzy-matched
  #     names, above, before joining to dictionary

  match_cur.df <-
    x_norm.df %>%
    dplyr::inner_join(dicts.tbl,
                     by = all_keys
                     )

  match_cum.df <- match_cur.df

  if(year_adjust) {
    ind_ny <- setdiff(all_keys,"year")

    year_interp<- function( y, y_l, y_u, df_l, df_u) {
      if (is.na(y_l) && is.na(y_u)) {
        rv <- tibble::tibble()
      } else if(is.na(y_l)) {
        rv <- df_u
      } else if(is.na(y_u)) {
        rv <- df_l
      } else if (y_l == y_u) {
        rv <- df_l
      } else {
        dft <- dplyr::bind_rows(df_l,df_u)
        rv <- dft[1,] + (((y-y_l)/(y_u-y_l)) *(dft[2,]-dft[1,]))
        rv <- rv %>% dplyr::mutate(n=as.integer(round(n)))
      }
      return(rv)
    }

    unmatched.df <-
      dplyr::anti_join( x_norm.df, match_cum.df, by=all_keys)

    # join to unmatched
    match_cur.df <-
      dplyr::left_join(unmatched.df,
              dicts.tbl %>% dplyr::filter(year< OG_DICT_ANYYEAR),
              by = dplyr::join_by( !!!{ind_ny}
                            , closest(year>=year))) %>%
      dplyr::rename(year=year.x, year_l=year.y) %>%
      tidyr::nest(x_l=c(-starts_with("year"),-{{ind_ny}})) %>%
      dplyr::bind_cols(
        dplyr::left_join(unmatched.df,
                  dicts.tbl  %>% dplyr::filter(year< OG_DICT_ANYYEAR),
                  by = dplyr::join_by(!!!{ind_ny},closest(year<=year))) %>%
        dplyr::rename(year=year.x, year_u=year.y) %>%
        tidyr::nest(x_u=c(-starts_with("year"),-{{ind_ny}})) %>%
        dplyr::select(year_u, x_u)
      ) %>%
      dplyr::filter(!is.na(year_l) | !is.na(year_u))

    if (nrow(match_cur.df)>0) {
       match_cur.df %<>%
        dplyr::rowwise() %>%
        dplyr::mutate(x_int = list(year_interp(y=year,y_l=year_l,y_u=year_u,df_l=x_l,df_u=x_u))) %>%
        dplyr::ungroup() %>%
        dplyr::select(-year_l,-year_u, -x_l, -x_u) %>%
        tidyr::unnest(x_int)

      match_cum.df %<>% dplyr::bind_rows(match_cur.df)
    }
  }

  # TODO: API retrieval of remainder

   if(FALSE) {
    unmatched.df <-
      dplyr::anti_join( x_norm.df, match_cum.df, by=all_keys)

    # join to unmatched
    match_cur.df <- unmatched.df %>%
      dplyr::inner_join(dicts.tbl,
                        by = all_keys
      )

    match_cum.df %<>% dplyr::bind_rows(match_cur.df)
  }

    #TODO: OG_GENDER_LEVELS

  rnt <- c("og_summary","og_details")
  names(rnt) <- output_vars
  match_cum.df %<>%
    dplyr::mutate(og_summary = pr_F) %>%
    tidyr::nest(og_details= c(pr_F, pr_M, pr_O, n, fuzzy_dist)) %>%
    dplyr::rename(dplyr::all_of(rnt)) %>%
    dplyr::select( {{cmp_nm}}, dplyr::all_of(output_vars))

  ### REJOIN PHASE
  byspec <- "primary_input"; names(byspec) <- cmp_primary
  rejoined.df <-
    dplyr::left_join(x, x_nms_match.df,
                     by= byspec)

  rejoined.df %<>% dplyr::left_join(match_cum.df,
                           by = c(primary_match= {{primary_key}}))  %>%
  dplyr::select(!primary_match) %>% as_tibble()

  rejoined.df
}

#' add_dict_matches
#'
#' @param x input data frame
#' @param col_map  column to match against
#' @param dicts list of dictionaries
#' @param matchTypes  match whole string, start, or anywhere in string
#' @param matchCaseVariations match title and all caps versions of dictionary keys
#' @param matchFirstN match against partial dictionary keys of at least first N tokens
#' @return tibble with list of measures in a column
#' @export


#'
#' @examples [TODO]
#'
#' @importFrom dplyr select
#' @importFrom dplyr rename
#' @importFrom dplyr filter
#' @importFrom dplyr mutate
#' @importFrom dplyr relocate
#' @importFrom dplyr pull
#' @importFrom dplyr left_join
#' @importFrom dplyr right_join
#' @importFrom dplyr bind_cols
#' @importFrom dplyr bind_rows
#' @importFrom dplyr rowwise
#' @importFrom dplyr summarise
#' @importFrom dplyr arrange
#' @importFrom dplyr ungroup
#' @importFrom tibble tibble
#' @importFrom tidyr nest
#' @importFrom rlang arg_match
#' @importFrom purrr map
#' @importFrom purrr list_rbind
#' @importFrom stringr str_to_upper
#' @importFrom stringr str_to_title
#' @importFrom stringr str_split
#' @importFrom stringr str_count
#' @importFrom glue glue_collapse

add_dict_matches <- function(x, col_map = c(text="text"),
                             dicts = c("kantro","iso3166"),
                             matchTypes = c("exact", "contains","starts"),
                             matchCaseVariations = TRUE,
                             matchFirstN = Inf,
                             addToDF = TRUE
) {

  rlang::arg_match(matchTypes, multiple = TRUE)

  # extract names from dictionaries

  extract_name_col <- function(dict, addCaseVariations) {
    tmp.df <- show_dict(dict)
    dict_domain <- attr(tmp.df,"domain")

    if (dict_domain=="gender") {
      res <- tmp.df %>%
        dplyr::select(name=given, id = given)
    } else {
      res <- tmp.df %>%
        dplyr::select(name, id = id)
    }
    res %<>% dplyr::mutate(dict = dict)

    res
  }

  dicts.df <-
    purrr::map(unique(dicts),  ~ extract_name_col( .x , matchCaseVariations)) %>%
    purrr::list_rbind()

  # add dictionary variations
  if (matchCaseVariations) {
    dicts.df %<>% dplyr::bind_rows(
      dicts.df  %>% dplyr::mutate(name=stringr::str_to_upper(name)),
      dicts.df %>% dplyr::mutate(name=stringr::str_to_title(name))
    )
  }

  if (matchFirstN<Inf) {
    dicts.df %<>% dplyr::bind_rows(
      dicts.df %>%
        dplyr::mutate(name_split =
                        stringr::str_split(name, " ",
                                           n=(matchFirstN+1))) %>%
        rowwise() %>%
        dplyr::mutate(name_short =
                        glue::glue_collapse(head(name_split, n=matchFirstN),
                                            sep=" ")) %>%
        dplyr::ungroup() %>%
        dplyr::select(name=name_short, id, dict)
    )
  }

  tvar <- col_map[["text"]]
  src.df <-  x %>%
    dplyr::select( text = {{tvar}} ) %>%
    dplyr::mutate( row = dplyr::row_number() )

  # build ngram list for search
  if (any(c("contains","starts") %in% matchTypes)) {
    # analysis for tokenization

    max_tokens <-
      dicts.df %>% dplyr::summarise(
        max_spaces = max(na.rm=TRUE,
                         stringr::str_count(name," "))+1) %>%
      dplyr::pull()

    min_tokens <-
      dicts.df %>% dplyr::summarise(
        max_spaces = min(na.rm=TRUE,
                         stringr::str_count(name," "))+1) %>%
      dplyr::pull()

    tokens.df <- src.df %>%
      tidytext::unnest_ngrams( input="text", output="ngram",
                               format = "text", to_lower=FALSE, drop=TRUE,
                               n = max_tokens , n_min = min_tokens)
  }

  cum_matches.df <- tibble::tibble()

  if ("exact" %in% matchTypes) {
    # find exact matches within text
    matches.df <- src.df %>%
      dplyr::rename( name = text ) %>%
      dplyr::left_join(dicts.df, by="name",
                       relationship="many-to-many") %>%
      dplyr::filter(!is.na(id)) %>%
      dplyr::distinct(row,id,dict)

    cum_matches.df %<>% dplyr::bind_rows(matches.df)
  }

  if (any(c("contains","starts") %in% matchTypes)) {
    # find exact matches within text
    matches.df <- tokens.df %>%
      dplyr::left_join(dicts.df, by=c(ngram="name"),
                       relationship="many-to-many") %>%
      dplyr::filter(!is.na(id))

    if (! "contains" %in% matchTypes) {
      matches.df %<>%
        dplyr::left_join(src.df, by="row") %>%
        dplyr::mutate(starts = stringr::str_starts( text,
                                                    pattern = stringr::coll(ngram) )) %>%
        dplyr::filter(starts) %>%
        dplyr::select(!starts, !text)
    }

    matches.df %<>%
      dplyr::distinct(row,id,dict)

    cum_matches.df %<>% dplyr::bind_rows(matches.df)
  }

  # normalize output
  norm <- cum_matches.df %>%
    tidyr::nest( .by = row , .key = "matches") %>%
    dplyr::right_join(src.df %>% dplyr::select(row), by="row") %>%
    dplyr::rowwise() %>%
    dplyr::mutate(first = list(purrr::pluck(matches,1,1)))  %>%
    dplyr::mutate(first=ifelse(is.null(first),NA,first)) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(first=unlist(first)) %>%
    dplyr::arrange(row) %>%
    dplyr::select(!row) %>%
    dplyr::relocate(og_match_first=first, og_match_details = matches)

  if (addToDF) {
    dplyr::bind_cols(x,norm)
  } else {
    norm
  }
}

#' gender_mean
#'
#' @param x (grouped) data frame with instrumented by impute
#' @param simplify return values
#'
#' @return mean value of each
#' @export
#'
#' @examples [TODO]


gender_mean <- function(x,  simplify_output = "scalar") {
  gender_estimate(x, simplify_output=simplify_output,
                  estimates="mean")
}

#' Title
#'
#' @param x data frame with instrumented by impute
#' @param simplify  simplify return values
#'
#' @return standard error value of each
#' @export
#'
#' @examples [TODO]
#' @importFrom tidyr pivot_longer
#' @importFrom tidyr pivot_wider
#' @importFrom purrr list_rbind
#' @importFrom tibble tibble
#' @importFrom dplyr bind_cols
#' @importFrom dplyr starts_with
#' @importFrom dplyr everything
#' @importFrom dplyr mutate
#' @importFrom dplyr relocate
#' @importFrom dplyr rename

gender_estimate <- function(x,  simplify_output = "tidy",
                           estimates=c("mean","sd","unc"),
                           ci_limit = .05) {

  termlist <- c("per_F","per_M","per_O")
  output_types <- c("tidy","row","scalar")
  estimate_types <- c("mean","sd","unc")

  # TODO: OG_GENDER_LEVELS integrate

  if(length(setdiff(estimates,estimate_types))>0) {
    warning("unsupported estimate types:", paste(setdiff(estimates,estimate_types),sep=",") )
  }

  if ((length(estimates) >1) && (simplify_output=="scalar")) {
    warning("multiple estimates selected for scalar output, only first is returned")
  }

  if(!simplify_output %in% output_types) {
    warning("unsupported output type -- using tidy")
    simplify_output <- "tidy"
  }

  #normalize  from either pr_F or og_details  as vector, list, or tibble
  if(is.data.frame(x)) {
    if (ncol(x)==1) {
      x.df <- x[[1]]
    }
  } else {
    x.df <- x
  }

  if(inherits(x.df,"list")) {
    x.df <- purrr::list_rbind(x.df)
  } else if(is.numeric(x.df)) {
    x.df <- tibble::tibble( pr_F = x.df )
    x.df %<>%
      dplyr::mutate(pr_M=1-pr_F,pr_O=0*pr_F,n=NA_integer_)
  }

  x.df %<>% dplyr::select(pr_M,pr_F,n) %>%
    dplyr::mutate(pr_O=1-pr_M-pr_F) %>%
    dplyr::relocate(pr_F,pr_M,pr_O,n)

  res_cum <- NULL

  if("mean" %in% estimates) {
    x.df %>%
      dplyr::summarize(
        dplyr::across(dplyr::starts_with("pr_"),
                      ~ mean(.x, na.rm = TRUE)
        )
      ) %>%
      tidyr::pivot_longer(dplyr::everything()) %>%
      dplyr::rename(term=name , mean =value) -> res_cur

    res_cum %<>% dplyr::bind_cols(res_cur)
  }

  if("sd" %in% estimates) {
    x.df %>%
      dplyr::summarize(
        dplyr::across(dplyr::starts_with("pr_"),
                      ~ sd(.x, na.rm = TRUE)
        )
      ) %>%
      tidyr::pivot_longer(dplyr::everything()) %>%
      dplyr::rename(term=name , sd =value) -> res_cur

    res_cum %<>% dplyr::bind_cols(res_cur %>% dplyr::select(!term))
  }

  if("unc" %in% estimates) {
    x.df %>%
      dplyr::summarize(
        dplyr::across(dplyr::starts_with("pr_"),
                      ~list (opengender:::og_mn_boot(.x))
        )
      ) %>%
      tidyr::pivot_longer(dplyr::everything()) -> res_cur

    res_cur %<>%
      dplyr::rowwise() %>%
      dplyr::mutate(value_t =
                      tidyr::pivot_wider(
                        value,names_from=term,values_from=estimate)) %>%
      select(term=name,value=value_t) %>%
      tidyr::unnest(value)

    res_cum %<>% dplyr::left_join(res_cur,by="term")
  }

  if (simplify_output=="tidy") {
    res_final <- res_cum
  } else if (simplify_output=="scalar") {
    res_final <- res_cum[[1,2]]
  } else {
    res_final <-
      res_cum %>%
      dplyr::mutate(term=stringr::str_replace(term,"pr_","prop_")) %>%
      tidyr::pivot_wider(values_from=!term,names_from=term)
  }
  res_final
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
gender_sample <- function(x, simplify=FALSE,  n=1) {

}

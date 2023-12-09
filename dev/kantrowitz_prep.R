###
### Kantrowitz Corpus Extraction
###
###
### See:
### https://www.nltk.org/nltk_data/
### https://www.cs.cmu.edu/afs/cs/project/ai-repository/ai/areas/nlp/corpora/names/
### https://www.nltk.org/book/ch06.html

library(tidyverse)
library(magrittr,include.only = "%<>%")

dev_prep_kantro<-function () {

  src_uri <- "https://raw.githubusercontent.com/nltk/nltk_data/gh-pages/packages/corpora/names.zip"
  target_file <- "kantro.zip"
  target_dir <- "./dev/data"
  target_path <- file.path(target_dir,target_file)
  male_file <- "names/male.txt"
  female_file <- "names/female.txt"

  download.file(src_uri, target_path)
  opath = getwd()
  setwd(target_dir)
  unzip(target_file, files = c(male_file,female_file))
  setwd(opath)

  kantro <-  tibble( given=readr::read_lines(file.path(target_dir,male_file)),pr_f=0)
  kantro %<>% bind_rows( tibble( given=readr::read_lines(file.path(target_dir,female_file)),pr_f=1) )
  usethis::use_data(kantro)
  kantro
}

dev_prep_kantro()

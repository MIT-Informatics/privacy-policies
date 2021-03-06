### destination DB
library("RSQLite")
library("DBI")
library("dbplyr")

### DBI Helper function

## Note: dplyr:db_create_index appears to do the same thing, leaving this for
##      documentation reasons
# createIndex <- function(con, tbl, vars) {
#   indexStatement <- function(tbl,vars) {
#     paste0("CREATE INDEX ",tbl,"_",paste0(collapse="_",vars),
#            " ON ",tbl,"(",  paste0(collapse=",",vars),");")
#   }
#   dbExecute(con,indexState(tbl,vars))
# }


###

setup_db <- function(force=FALSE) {
  maindb.file <- "main.sqlite"
  if (!force & !file.exists(maindb.file)) {
    stop("DB does not exist", maindb.file)
  }
  con <- DBI::dbConnect(RSQLite::SQLite(), maindb.file)
  return(con)
}

setup_privaseer_tables <- function(con) {
  tl <- DBI::dbListTables(con)

  if (! all(c("privaseer_par","privaseer_tok","privaseer_file") %in% tl)) {
    # create tables and indices
    DBI::dbCreateTable(con, "privaseer_par", tibble(file="aaa",par=1,text="aaa"))
    dplyr::db_create_index(con,"privaseer_par","file")

    DBI::dbCreateTable(con, "privaseer_tok", tibble(file="aaa",par=1,token="aaa"))
    dplyr::db_create_index(con,"privaseer_tok","token")
    dplyr::db_create_index(con,"privaseer_tok","file")
    dplyr::db_create_index(con,"privaseer_tok",c("file","token"))


    DBI::dbCreateTable(con, "privaseer_file", tibble(file="aaa"))
    dplyr::db_create_index(con,"privaseer_file","file")

  }
  list(
    file = tbl(con,"privaseer_file"),
    par = tbl(con,"privaseer_par"),
    tok = tbl(con,"privaseer_tok")
  )
}

library(utils)
library(rvest)
library(textclean)
library(tidytext)
library(SnowballC)

ingest_privaseer_data <- function() {
  con <- setup_db(force=TRUE)
  ptbls <- setup_privaseer_tables(con)

  ### Privaseer demo extraction

  if(!file.exists("policy_files")) {
    privaseerTarPath <- "privaseer_demo_set_policies_v1.0.tar.gz"
    utils::untar(privaseerTarPath, exdir="policy_files")
  }
  policyFiles.ls <- dir("policy_files/",recursive = TRUE, pattern=".*\\.html$")

  ### iterate through file -- parsing


  textElements <- c("p","li","table","pre","blockquote")
  lstop <- stop_words %>%
    filter(lexicon=="snowball") %>%
    select(word) %>% unlist() %>% as.character()

  for (i in policyFiles.ls)  {
    if ((ptbls$file %>% filter(file==i) %>% collect() %>% nrow()) > 0) {
      print (paste("Found",i))
      next
    } else {
      print (paste("Processing",i))
    }

    tm <-   read_html(fs::path("policy_files/",i))
    tx <-  tm %>%   html_elements(c("body"))
    local({
      txm2 <- tx %>% html_elements("main")
      if (length(txm2)>0) {
        tx <-  txm2
      }
    })

    tx %<>%
      html_elements(textElements) %>%
      html_text2() %>%
      unique() %>%
      str_squish() %>%
      unique()

    tx %<>%
      replace_white() %>%
      replace_non_ascii() %>%
      replace_contraction() %>%
      str_squish() %>%
      unique() %>%
      as.tibble() %>%
      rename(text=value) %>%
      mutate(par=row_number(),file=i)

    ### Stemming and tokenizing


    tok<- tx %>%
      unnest_tokens(`word`, `text`,
                    token="words",
                    stopwords = lstop ,
                    to_lower=TRUE) %>%
      filter(!is.na(word)) %>%
      mutate(stemmed=wordStem(word)) %>%
      filter(str_length(stemmed)>1) %>%
      select(-word)

    tok_ngram <- tok %>%
      group_by(file,par) %>%
      unnest_tokens(`token`,`stemmed`,
                    token="skip_ngrams",
                    to_lower=TRUE,
                    n=2,
                    k=1)

    # update 3 tables

    dbAppendTable(con,
                  as.character(ptbls$file$ops$x),
                  tibble(file=i))
    dbAppendTable(con,
                  as.character(ptbls$tok$ops$x),
                  tok_ngram)
    dbAppendTable(con,
                  as.character(ptbls$par$ops$x),
                  tx)
  }
  dbDisconnect(con)
}


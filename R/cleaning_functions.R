#' Find the index with the max argument from a structured set of data.
#'
#' The data columns should be in the following order `tweet_id_str`,
#'   `job_id`, `created_at`, `none`, `med` and `high`.
#'
#' @param data A data frame of the order above.
#' @return A data frame with a new argmax column called `prediction`.
find_argmax <- function(data){
  data$prediction <- apply(data[4:6], MARGIN=1, FUN = which.max)
  data$prediction <- data$prediction - 1

  data
}

#' Preprocess the twitter data being by finding the counts for each job.
#'
#' This function just serves a small preprocessing step that will save me from
#'   writing the same thing over and over and over again. It essentially prunes
#'   out those who may overtweet more than their analysis alotment, which in
#'   this case, is set to one.
#'
#' @importFrom magrittr "%>%"
#' @param data The data frame to count the events.
#' @return A data frame with a count column per job_id.
preprocess <- function(data){
  data %>%
    dplyr::distinct(from_user, month, year, .keep_all = T)
}

#' This function maps values to the predictions for the purpose of creating an
#'   emotional index. Of course, this could be done with the current values but
#'   with a bit more manipulation on the denominator side of things. This just
#'   saves a step plus adds a bit more functionality with the manipulation of
#'   things.
#'
#' @importFrom magrittr "%>%"
#' @param data The data frame. Must contain a column called `msa_geoid`.
#' @param msa_data The msa data frame. Probably from the `haterzmapper` package
#'   and specifically \ref{`hatermapper::topcities`}.
#' @param col_name The name of the column to be manipulated.
#' @param remove_middle This option removes the middle class if set to true.
#' @return A data frame with cleaned predictions.
transform_prediction <- function(data, msa_data, col_name, remove_middle = F){
  col_name <- rlang::enquo(col_name)
  emotion_name <- paste0('val_', dplyr::quo_name(col_name))

  new_data <- data %>%
    dplyr::left_join(msa_data) %>%
    dplyr::mutate(!! emotion_name := prediction/2)

  if(remove_middle){
    new_data <- new_data %>%
      dplyr::filter(prediction != 1)
  }
  new_data
}

#' Creates a summary of the predictions that make it easier to understand what
#'   is going on. It does so by providing the number of rows per grouping along
#'   with a transformation of the prediction variable in which the preiction val
#'   is divided by two. This creates a mapping from classification to hlaf the
#'   the size of classification {2 = 1, 1 = 0.5, 0 = 0}.
#'
#' @importFrom magrittr "%>%"
#' @param data Dataframe
#' @param summary_var A column in `data` to perform the summary on
#' @param ... The grouping variables.
summarize_emotion <- function(data, summary_var, ...){
  summary_var <- dplyr::enquo(summary_var)
  summary_var_name <- paste0('sum_', dplyr::quo_name(summary_var))
  n_var_name <- paste0('n_', dplyr::quo_name(summary_var))
  group_vars <- dplyr::enquos(...)


  data %>%
    dplyr::group_by(!!! group_vars) %>%
    dplyr::summarize(!! n_var_name := dplyr::n(), !! summary_var_name := sum(!! summary_var))
}

#' Pick select msa variables primarily from the `msa_count` data.
#'
#' This provides easier functionality for quickly selecting variables from data.
#'
#' @importFrom magrittr "%>%"
#' @param data MSA count data. Most likely `msa_count` data frame.
#' @param var MSA variable that you want to compare against the population var.
#' @return A data frame with the selected variable and log of those variables.
pick_msavars <- function(data, var){
  var <- rlang::enquo(var)
  data %>% dplyr::select(msa_geoid, B01003_001, !! var) %>%
    dplyr::filter(!! var > 0 & B01003_001 > 0) %>%
    dplyr::mutate(logx = log(B01003_001), logy := log(!! var))
}

#' Clean up the tweet files upon reading.
#'
#' This function reads in the file mmanipulates the column types, removes
#'   duplicaes and creates a couple new variables.
#'
#' @param file The file to be read. This file must contain a `created_at`,
#'   `from_user`, and `tweet_id_str` column in order to function.
#' @importFrom magrittr "%>%"
#' @return Cleaned tibble with non_duplicated rows.
clean_tweeter <- function(file){
  readr::read_csv(file,
                  col_types = readr::cols(
                    tweet_id_str = readr::col_character(),
                    from_user = readr::col_character())) %>%
    dplyr::distinct(tweet_id_str, .keep_all = T) %>%
    dplyr::mutate(month = lubridate::month(created_at),
                  year = lubridate::year(created_at))
}

#' This function preprocesses prediction files. It uses the non-pred files
#'   along with some of the other helper functions in this notebook to achieve
#'   this.
#'
#' @importFrom magrittr "%>%"
#' @param file Path to the prediction file.
#' @param join_data A data from of the non-prediction files.
#' @param colnames A vector of column names.
#' @param ... See `transform_prediction`.
#' @return A cleaned tibble for predictions.
clean_predtweets <- function(file, join_data, colnames=cnames, ...){
  readr::read_csv(file, col_names = colnames,
                  col_types = readr::cols(tweet_id_str = readr::col_character())
  ) %>%
    find_argmax() %>%
    dplyr::left_join(join_data) %>%
    dplyr::distinct(tweet_id_str, .keep_all = T) %>%
    preprocess() %>%
    transform_prediction(msa_jobs, anger, ...)
}

#' Summarize the anger index per msa_geoid.
#'
#' @importFrom magrittr "%>%"
#' @param data Data frame to be passed.
#' @return Summarized data frame.
create_summary_data <- function(data){
  data %>%
    summarize_emotion(val_anger, msa_geoid) %>%
    dplyr::mutate(index_anger = sum_val_anger/n_val_anger)
}

#' This combines the general and keyword summary data so that comparisons from
#' This combines the general and keyword summary data so that comparisons from
#'   baseline can be made.
#'
#' @importFrom magrittr "%>%"
#' @param data The summary data frame.
#' @param gen_summary_data The general summary data frame.
#' @return A tibble with an index_diff column.
create_comparison <- function(data, gen_summary_data = gen_ang_summary){
  dplyr::select(gen_summary_data, msa_geoid, index_anger) %>%
    dplyr::rename(gen_index = index_anger) %>%
    dplyr::left_join(data) %>%
    dplyr::mutate(index_diff = index_anger - gen_index) %>%
    na.omit()
}

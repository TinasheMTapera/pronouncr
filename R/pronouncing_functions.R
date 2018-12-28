# pronunciation functions

syllable_count <- function(phones) {
  # Count the number of syllables in a string of phones

  phones %>%
    stresses() %>%
    stringr::str_split(., pattern = "") %>%
    unlist() %>%
    length() %>%
    return()
}

phones_for_word <- function(my_word) {
  # Get the phones for a given word

  paste0("\\b", my_word, "(\\(\\d\\))?\\s") %>%
    stringr::str_detect(pronouncr::cmu_dict, .) %>%
    pronouncr::cmu_dict[.] %>%
    parse_cmu_dict() %>%
    dplyr::pull(phones) %>%
    return()
}

stresses <- function(phones) {
  # Get the vowel stresses for a given string of phones

  stringr::str_extract_all(phones, "[[:digit:]]+") %>%
    unlist() %>%
    stringr::str_c(., collapse = "") %>%
    return()

}

stresses_for_word <- function(word) {
  # Get a list of possible stress patterns for a given word

  phones <- phones_for_word(tolower(word))

  if(length(phones) > 1) {

    phones %>%
      as.list() %>%
      purrr::map(., stresses) %>%
      unlist() %>%
      return()
  } else {

    phones %>%
      stresses() %>%
      return()
  }

}

rhyming_part <- function(phones) {
  # Get the "rhyming part" of a string with CMUdict phones;
  # "Rhyming part" here means everything from the vowel in the stressed
  # syllable nearest the end of the word up to the end of the word.

  phones_list <- phones %>%
    stringr::str_split(string = ., pattern = " ") %>%
    unlist()

  indexes <-  stringr::str_which(phones_list, "1|2")

  last_index <- indexes[ length(indexes) ]

  phones_list[ last_index:length(phones_list) ] %>%
    stringr::str_c(collapse = " ") %>%
    return()

}

search_phones <- function(pattern, exact = TRUE, return_type = "both") {
  # Get words whose pronunciation matches a pattern of phones.
  # This function Searches the CMU dictionary for pronunciations matching a
  # given a string of phones.

  pattern <- ifelse(exact == TRUE,
                    paste0("[^A-Z0-9]\\s", pattern, "\\b$"),
                    paste0("\\b", pattern, "\\b"))

  search_results <- pattern %>%
    stringr::str_detect(pronouncr::cmu_dict, .) %>%
    pronouncr::cmu_dict[.] %>%
    parse_cmu_dict()

  if(return_type == "both") {

    return(search_results)

  } else {

    search_results %>%
      dplyr::pull(return_type) %>%
      return()

  }

}

search_stresses <- function(pattern, exact = TRUE, return_type = "both") {
  # Get words whose stress pattern matches a regular expression.
  # This function searches only for matches in the
  # stress patterns of each pronunciation in the dictionary.

  pad <- "([[:space:]]+[[:upper:]]+)+"

  pattern <- ifelse(exact == TRUE,
                    stringr::str_split(pattern,pattern = "") %>%
                      unlist() %>%
                      stringr::str_c(.,collapse = pad) %>%
                      paste0("^[^0-9]+", ., "[^0-9]+$"),
                    stringr::str_split(pattern,pattern = "") %>%
                      unlist() %>%
                      stringr::str_c(.,collapse = pad))

  search_results = stringr::str_detect(pronouncr::cmu_dict, pattern) %>%
    pronouncr::cmu_dict[.] %>%
    parse_cmu_dict()

  if(return_type == "both") {

      return(search_results)

  } else {

    search_results %>%
      dplyr::pull(return_type) %>%
      return()

  }

}

rhymes <- function(word, return_type = "both") {
  # Get words rhyming with a given word.

  rhyming_phones <- phones_for_word(word) %>%
    rhyming_part()

  phone_matches <- search_phones(rhyming_phones,
                                 exact = FALSE,
                                 return_type = "phones") %>%
    .[ stringr::str_which(., paste0(rhyming_phones, "$")) ]

  search_results <- purrr::map_dfr(as.list(phone_matches), search_phones) %>%
    dplyr::filter(word != !!word)

  if(return_type == "both") {

    return(search_results)

  } else {

    search_results %>%
      dplyr::pull(return_type) %>%
      return()

  }

}

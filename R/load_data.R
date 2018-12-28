# Load the CMU Pronouncing Dictionary files
#
#

get_dictionary <- function() {


  pronouncr::cmu_dict %>%
    dplyr::as_tibble() %>%
    tidyr::separate(col = value,
             sep = " ",
             into = c("word", "phones"),
             extra = "merge") %>%
    dplyr::mutate(word = stringr::str_replace(string = word,
                                       pattern = "\\(\\d+\\)",
                                       replacement = "")) %>%
    return()
}

get_symbols <- function() {

  return(pronouncr::cmu_symbols)

}

get_phones <- function() {

  pronouncr::cmu_phones %>%
    dplyr::as_tibble() %>%
    tidyr::separate(col = value,
             sep = "\t",
             into = c("phone", "type"),
             extra = "merge") %>%
    return()

}

get_vp <- function() {

  pronouncr::cmu_vp %>%
    dplyr::as_tibble() %>%
    tidyr::separate(col = value,
             sep = " ",
             into = c("vp", "phones"),
             extra = "merge") %>%
    return()

}

parse_cmu_dict <- function(cmu_dict_vec) {

  # Parse lines of the CMU dictionary and return
  # a tidy dataframe version of the lines

  cmu_dict_vec %>%
    dplyr::as_tibble() %>%
    tidyr::separate(col = value,
                    sep = " ",
                    into = c("word", "phones"),
                    extra = "merge") %>%
    dplyr::mutate(word = stringr::str_replace(string = word,
                                              pattern = "\\(\\d+\\)",
                                              replacement = "")) %>%
    return()
}

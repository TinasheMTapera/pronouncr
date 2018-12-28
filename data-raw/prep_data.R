# R package preparation:
# This script loads the CMU-dictionary data files.
# These are essentially text files describing how
# words are pronounced using modern American-English linguistic rules.
# See https://github.com/cmusphinx/cmudict for more

# Load the pronouncing dictionary
cmu_dict <- read_lines("inst/extdata/cmudict.dict")

# Load the phones
cmu_phones <- read_lines("inst/extdata/cmudict.phones")

# Load the symbols
cmu_symbols <- read_lines("inst/extdata/cmudict.symbols")

# Load the vp's
cmu_vp <- read_lines("inst/extdata/cmudict.vp")

usethis::use_data(cmu_dict, overwrite = TRUE)
usethis::use_data(cmu_phones, overwrite = TRUE)
usethis::use_data(cmu_symbols, overwrite = TRUE)
usethis::use_data(cmu_vp, overwrite = TRUE)

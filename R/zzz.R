
# Start up message when loading the library, implemented with
# `packageStartupMessage()` so it can be easily suppressed.

#' @import utils

.onAttach <- function(...) {
  packageStartupMessage(paste0(
    "Thanks for using EssentialTnSeq v", packageVersion("EssentialTnSeq"), "!\n",
    "If you encounter any bugs or problems, please submit an issue at the\n",
    "Github page: https://github.com/hancockinformatics/EssentialTnSeq/issues\n"
  ))
}

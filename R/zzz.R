
.onAttach <- function(...) {
  packageStartupMessage(paste0(
    "\nThanks for using EssentialTnSeq!\n",
    "If you encounter any bugs or problems, please submit an issue at the\n",
    "Github page: https://github.com/hancockinformatics/EssentialTnSeq/issues\n"
  ))
}

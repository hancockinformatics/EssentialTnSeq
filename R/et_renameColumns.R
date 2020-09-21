#' et_renameColumns
#'
#' @param input_list Named list of data frames for a given condition, with each
#'   data frame corresponding to a different replicate.
#' @param condition Condition name (string) for the given list.
#'
#' @return The same list of data frames, but with columns renamed to be specific
#'   to the condition.
#'
#' @export
#'
#' @description Renames columns of all data frames in a list based on the name
#'   of the list element containing those data frames. Designed to use the
#'   output from \code{EssentialTnSeq::et_readFiles()}. Joins the tables for a
#'   given condition into one data frame containing information from all
#'   replicates for that condition.
#'
#' @references None.
#'
#' @seealso \url{https://github.com/hancockinformatics/EssentialTnSeq}
#'
#' @examples
#' \dontrun{
#'   et_renameColumns(
#'     input_list = treatment1_list,
#'     condition = "treatment1"
#'   )
#' }
#'
et_renameColumns <- function(input_list, condition) {

  step1 <- input_list

  for (i in 1:length(step1)) {
    colnames(step1[[i]])[2] <- paste(
      condition,
      names(step1)[i],
      colnames(step1[[i]])[2],
      sep = "_"
    )
  }

  step2 <-
    plyr::join_all(step1, by = "locus_tag", type = "full") %>% as_tibble()

  return(step2)
}

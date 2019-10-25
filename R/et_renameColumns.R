#' et_renameColumns
#'
#' @param input_list List of data frames for a given condition, with each data
#'   frame corresponding to a different replicate.
#' @param cond_name Condition for the given list.
#'
#' @return The same list of data frames, but with columns renamed to be specific
#'   to the condition.
#' @export
#'
#' @description Renames columns of all data frames in a list based on the name
#'   of the list element containing those data frames.
#'
#' @references None.
#'
#' @seealso \url{https://github.com/travis-m-blimkie/EssentialTnSeq}
#'
et_renameColumns <- function(input_list, cond_name) {

  for (i in 1:length(input_list)) {

    colnames(input_list[[i]])[2] <- paste0(cond_name,
                                           "_",
                                           names(input_list)[i],
                                           "_",
                                           colnames(input_list[[i]])[2])

  }

  return(input_list)

}

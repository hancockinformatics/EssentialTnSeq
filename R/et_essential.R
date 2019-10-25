#' et_essential
#'
#' @param tool Tool used to generate the results; one of Gumbel or Tradis.
#' @param input_df Data frame containing essentiality call for all replicates
#'   for a given condition.
#' @param cutoff Threshold for determining essentiality based on the number of
#'   replicates.
#'
#' @return Data frame of essential genes for the condition.
#' @export
#'
#' @import dplyr
#'
#' @references None.
#'
#' @seealso \url{https://github.com/travis-m-blimkie/EssentialTnSeq}
#'
et_essential <- function(tool, input_df, cutoff) {

  # Stop and print error if tool specified incorrectly
  if (tool %in% c("Gumbel", "Tradis") == FALSE) {
    stop('Please enter either "Gumbel" or "Tradis" for tool.')
  }


  if (tool == "Gumbel") {

    # Code for Gumbel
    ess_df <- input_df %>%
      mutate(sum_counts_E = rowSums(. == "E"),
             ess_stat = case_when(sum_counts_E >= cutoff ~ "ess", TRUE ~ "non"))

  } else if (tool == "Tradis") {

    # Code for Tradis
    ess_df <- input_df %>%
      mutate(sum_counts_0 = rowSums(. == 0),
             ess_stat = case_when(sum_counts_0 >= cutoff ~ "ess", TRUE ~ "non"))

  }

  # Filter out non-essential genes
  output_df <- ess_df %>%
    filter(ess_stat == "ess")


  return(output_df)

}

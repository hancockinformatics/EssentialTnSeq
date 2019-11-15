#' et_essential
#'
#' @param tool String; one of "Gumbel" or "Tradis".
#' @param input_df Data frame containing essentiality call for all replicates
#'   for a given condition.
#' @param cutoff Threshold for determining essentiality based on the number of
#'   replicates. Recommend setting to one less than the number of replicates.
#'
#' @return Filtered data frame of essential genes for the condition.
#'
#' @export
#'
#' @import dplyr
#'
#' @description Determines essentiality for a set of genes from Gumbel or
#'   Tradis. For Gumbel, a gene is considered essential if it was assigned a
#'   call of "E". For Tradis, essential genes are those with 0 read counts. The
#'   filtered data frame which is returned contains the additional columns used
#'   in defining essentiality.
#'
#' @references None.
#'
#' @seealso \url{https://github.com/travis-m-blimkie/EssentialTnSeq}
#'
#' @examples
#' \dontrun{
#'   et_essential("Gumbel", treatment1_df, cutoff = 2)
#' }
#'
et_essential <- function(tool, input_df, cutoff) {


  # Make tool name lower case so we know what to expect.
  tool <- str_to_lower(tool)


  # Stop and print error if tool specified incorrectly
  if (tool %in% c("gumbel", "tradis") == FALSE) {
    stop('Please enter either "Gumbel" or "Tradis" for tool.')
  }


  if (tool == "gumbel") {
    ess_df <- input_df %>%
      mutate(sum_counts_E = rowSums(. == "E"),
             ess_stat = case_when(sum_counts_E >= cutoff ~ "ess", TRUE ~ "non"))

  } else if (tool == "tradis") {
    ess_df <- input_df %>%
      mutate(sum_counts_0 = rowSums(. == 0),
             ess_stat = case_when(sum_counts_0 >= cutoff ~ "ess", TRUE ~ "non"))
  }

  # Filter out non-essential genes
  output_df <- ess_df %>% filter(ess_stat == "ess")

  return(output_df)
}

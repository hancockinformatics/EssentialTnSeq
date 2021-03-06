#' et_essential
#'
#' @param tool String; one of "Gumbel" or "Tradis".
#' @param input_df Data frame containing essentiality call for all replicates
#'   for a given condition.
#' @param cutoff Threshold for determining essentiality based on the number of
#'   replicates. Recommend setting to one less than the number of replicates.
#' @param filter If \code{TRUE} (default), the result returned to the user is
#'   filtered to only contains the genes determined essential. If \code{FALSE},
#'   the unfiltered data frame is returned instead.
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
#' @seealso \url{https://github.com/hancockinformatics/EssentialTnSeq}
#'
#' @examples
#' \dontrun{
#'   et_essential("Gumbel", treatment1_df, cutoff = 2, filter = TRUE)
#' }
#'
et_essential <- function(tool, input_df, cutoff, filter = TRUE) {


  # Make tool name lower case so we know what to expect.
  tool <- tolower(tool)


  # Stop and print error if tool specified incorrectly
  if (tool %in% c("gumbel", "tradis") == FALSE) {
    stop('Please enter either "Gumbel" or "Tradis" for tool.')
  }


  if (tool == "gumbel") {
    ess_df <- input_df %>%
      mutate(
        sum_counts_E = rowSums(. == "E"),
        ess_stat = case_when(sum_counts_E >= cutoff ~ "ess", TRUE ~ "non")
      ) %>%
      as_tibble()

  } else if (tool == "tradis") {
    ess_df <- input_df %>%
      mutate(
        sum_counts_0 = rowSums(. == 0),
        ess_stat = case_when(sum_counts_0 >= cutoff ~ "ess", TRUE ~ "non")
      ) %>%
      as_tibble()
  }

  # Filter out non-essential genes
  df_filtered <- filter(ess_df, ess_stat == "ess")

  # Return filtered or unfiltered data frame based on user argument
  if (filter) {
    return(df_filtered)
  } else{
    return(ess_df)
  }

}

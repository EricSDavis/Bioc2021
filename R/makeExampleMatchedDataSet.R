#' Function for generating an example matchRanges dataset
#'
#' This function will generate an example dataset as input
#' for `matchRanges()`.
#'
#' @param type Character designating which type of dataset to make.
#'   options are one of 'data.frame', 'data.table', 'DataFrame',
#'   'GRanges', or 'GInteractions'.
#'
#' @return Returns an example dataset for
#'   input to `matchRanges()`.
#'
#' @examples
#' ## Make examples for matchRanges()
#' makeExampleMatchedDataSet()
#' makeExampleMatchedDataSet(type = 'data.frame') |> head()
#' makeExampleMatchedDataSet(type = 'data.table')
#' makeExampleMatchedDataSet(type = 'DataFrame')
#' makeExampleMatchedDataSet(type = 'GRanges')
#'
#' @rdname makeExampleMatchedDataSet
#' @rawNamespace import(data.table, except = c(between, shift, first, second, indices))
#' @import IRanges
#' @import GenomicRanges
#' @import S4Vectors
#' @export
makeExampleMatchedDataSet <- function(type = "GRanges") {

  ## Parse type argument
  type <- match.arg(type, choices = c('data.frame',
                                      'data.table',
                                      'DataFrame',
                                      'GRanges'))

  ## Define colors
  colors <- c("#e19995", "#adaf64", "#4fbe9b", "#6eb3d9", "#d098d7")

  ## Generate example DataFrame data -----------------------------------------------------

  ## Create data.frame for points
  set.seed(5)
  df <- data.frame(color = factor(c(sample(colors, 16, replace = TRUE),
                                    sample(colors, 120, replace = TRUE))),
                   size = c(abs(rnorm(16, 0.5, 0.25))+0.35,
                            abs(rnorm(120, 0.5, 0.25))+0.35),
                   set = c(rep('focal', 16),
                           rep('pool', 120)))

  ## Reorder factor level by colors
  levels(df$color) <- colors

  ## Generate example data.frame/data.table/DataFrame
  if (identical(type, 'data.frame')) out <- df
  if (identical(type, 'data.table')) out <- as.data.table(df)
  if (identical(type, 'DataFrame')) out <- DataFrame(df)


  ## Generate example GRanges data -------------------------------------------------------

  if (identical(type, 'GRanges')) {

    set.seed(5)
    pool <- GRanges(seqnames = "chr1",
                    ranges = IRanges(start = sample(1:800, 120, replace = TRUE),
                                     width = sample(25:200, 120, replace = TRUE)),
                    feature1 = FALSE,
                    color = sample(1:5, 120, replace = TRUE))
    focal <- GRanges(seqnames = "chr1",
                     ranges = IRanges(start = sample(1:800, 16, replace = TRUE),
                                      width = sample(25:200, 16, replace = TRUE)),
                     feature1 = TRUE,
                     color = sample(1:5, 16, replace = TRUE))

    ## Add width to metadata
    pool$length <- width(pool)
    focal$length <- width(focal)

    ## Combine
    out <- c(focal, pool)
  }

  return(out)
}


#' A function to calculate contrasts
#' @param linearmodel A linear model.
#' @K a vector specifying the contrast
#' @keywords contrast
#' @export
#' @examples
#' contrast(lm(dv ~ iv1*iv2, data = data), c(0,0,0,1))

contrast <- function(linearmodel, K) {
  summary(glht(aov(linearmodel), matrix(K,1)))
}


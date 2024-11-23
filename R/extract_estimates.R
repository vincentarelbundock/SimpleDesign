extract_estimates_one = function(model,
                                 label = NULL,
                                 vcov = NULL,
                                 alpha = 0.05,
                                 null = 0,
                                 term = NULL) {
  checkmate::assert_string(label)
  checkmate::assert_number(alpha, lower = 0, upper = 1)
  checkmate::assert_number(null)
  out = marginaleffects::hypotheses(model,
    vcov = vcov,
    conf_level = 1 - alpha,
    hypothesis = null)
  out = as.data.frame(out)
  out[["estimator"]] = label
  cols = c("estimator", "term", "estimate", "p.value", "conf.low", "conf.high")
  cols = intersect(cols, colnames(out))
  out = out[, cols]
  if (!is.null(term)) {
    if (!any(term %in% out$term)) {
      stop("No matching `term` found.", call. = FALSE)
    }
    out = out[which(out$term %in% term), ]
  }
  return(out)
}


#' Tidy Results from a List of Estimators
#'
#' This function applies extracts a "tidy" output from a fitted model, or from a named list of fitted models.
#' The information is extracted using the `hyotheses()` function from the `marginaleffects` package.
#'
#' @param ... A named list of fitted model objects. Each model is passed to `marginaleffects::hypotheses()`.
#' @param vcov An optional variance-covariance matrix or a function to compute it. Passed to `extract_estimates`.
#' @param alpha A numeric value between 0 and 1 specifying the significance level for confidence intervals (default is 0.05).
#' @param null A numeric value representing the null hypothesis for the parameter estimates (default is 0).
#' @param term An optional character vector of terms to filter the output. If specified, only rows with matching terms are returned.
#'
#' @return A data frame combining the tidied results from all models in the list.
#'   Columns include:
#' \describe{
#'   \item{estimator}{The label for the estimator (corresponding to the list names).}
#'   \item{term}{The name of the parameter (e.g., model term).}
#'   \item{estimate}{The estimated value of the parameter.}
#'   \item{p.value}{The p-value for the hypothesis test of the parameter.}
#'   \item{conf.low}{The lower bound of the confidence interval.}
#'   \item{conf.high}{The upper bound of the confidence interval.}
#' }
#'
#' @examples
#' library(SimpleDesign)
#' extract_estimates(
#'   `I` = lm(mpg ~ wt, data = mtcars),
#'   `II` = lm(mpg ~ wt + disp, data = mtcars),
#'   alpha = 0.01,
#'   term = "wt"
#' )
#'
#' @export
extract_estimates = function(...,
                             vcov = NULL,
                             alpha = 0.05,
                             null = 0,
                             term = NULL) {
  models = list(...)
  if (is.null(names(models))) {
    names(models) = seq_along(models)
  }
  out = models
  for (n in names(out)) {
    if (!inherits(out[[n]], "data.frame")) {
      out[[n]] = extract_estimates_one(out[[n]],
        label = n,
        vcov = vcov,
        alpha = alpha,
        null = null,
        term = term)
    }
  }
  out = data.table::rbindlist(out, fill = TRUE)
  out = data.table::setDF(out)
  return(out)
}

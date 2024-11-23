#' Extract and Tidy Estimator Results
#'
#' This function tidies the results from a statistical model, extracting key metrics
#' such as the estimate, confidence intervals, and p-values. It also allows specifying
#' a label for the estimator and filtering by terms.
#'
#' @param model A fitted model object. This object is passed to the `marginaleffects::hypotheses` function.
#' @param label A character string specifying a label for the estimator. This label is added to the output as a column.
#' @param vcov An optional variance-covariance matrix or a function to compute it. Passed to `marginaleffects::hypotheses`.
#' @param alpha A numeric value between 0 and 1 specifying the significance level for confidence intervals (default is 0.05).
#' @param null A numeric value representing the null hypothesis for the parameter estimates (default is 0).
#' @param term An optional character vector of terms to filter the output. If specified, only rows with matching terms are returned.
#'
#' @return A data frame containing tidied results with columns:
#' \describe{
#'   \item{estimator}{The label for the estimator, as specified by the `label` parameter.}
#'   \item{term}{The name of the parameter (e.g., model term).}
#'   \item{estimate}{The estimated value of the parameter.}
#'   \item{p.value}{The p-value for the hypothesis test of the parameter.}
#'   \item{conf.low}{The lower bound of the confidence interval.}
#'   \item{conf.high}{The upper bound of the confidence interval.}
#' }
#'
#' @examples
#' library(SimpleDesign)
#' model <- lm(mpg ~ wt, data = mtcars)
#' tidy_estimator(model, label = "OLS", alpha = 0.05, null = 0)
#'
#' @export
tidy_estimator = function(model,
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
#' This function applies `tidy_estimator` to a named list of models, extracting and tidying
#' results for each model and combining them into a single data frame.
#'
#' @param model_list A named list of fitted model objects. Each model is passed to `tidy_estimator`.
#' @param vcov An optional variance-covariance matrix or a function to compute it. Passed to `tidy_estimator`.
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
#' model1 <- lm(mpg ~ wt, data = mtcars)
#' model2 <- lm(mpg ~ disp, data = mtcars)
#' models <- list(OLS_wt = model1, OLS_disp = model2)
#' tidy_estimator_list(models, alpha = 0.05, null = 0)
#'
#' @export
tidy_estimator_list = function(model_list,
                               vcov = NULL,
                               alpha = 0.05,
                               null = 0,
                               term = NULL) {
  checkmate::assert_list(model_list, names = "unique")

  out = model_list
  for (n in names(out)) {
    out[[n]] = tidy_estimator(out[[n]],
      label = n,
      vcov = vcov,
      alpha = alpha,
      null = null,
      term = term)
  }
  out = data.table::rbindlist(out, fill = TRUE)
  out = data.table::setDF(out)
  return(out)
}

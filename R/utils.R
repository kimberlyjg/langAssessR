#' @import ggplot2
#' @importFrom stats dnorm glm plogis rnorm runif predict binomial quantile sd
#' @importFrom utils globalVariables
NULL

# Global variables for ggplot2
utils::globalVariables(c(
  "site", "estimate", "lcl", "ucl", "model",
  "start", "weight", "window_id", "delta",
  "topic", "lift", "group", "subgroup", "auc",
  "mean_pred", "obs_rate", "threshold", "net_benefit",
  "policy", "entity", "count", "balance", "fnr", "fpr",
  ".data", "y"
))

# Re-export ggplot2's special operator
`%+replace%` <- ggplot2::`%+replace%`

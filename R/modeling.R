#' Fit a model (classification or regression)
#'
#' @param x data.frame/matrix of features (rows = samples)
#' @param y vector; for classification use 0/1 or two-level factor; for regression numeric
#' @param engine character: "glm" (default) or "glmnet" (optional; requires glmnet)
#' @param ... engine-specific args (e.g., alpha, lambda for glmnet)
#' @return an object of class "langAssessR_model" with slots: type, engine, model, feats, levels
#' @export
la_fit <- function(x, y, engine = c("glm", "glmnet"), ...) {
  engine <- match.arg(engine)
  x <- as.data.frame(x, check.names = FALSE)

  # auto-detect task type
  type <- if ((is.factor(y) && length(levels(y)) == 2) || all(y %in% c(0,1))) "classification" else "regression"
  if (is.factor(y) && type == "classification") {
    lev <- levels(y)
    y <- as.integer(y == lev[2])  # positive = second level
  } else {
    lev <- NULL
  }

  # fit
  if (engine == "glm") {
    df <- cbind.data.frame(y = y, x)
    if (type == "classification") {
      mod <- stats::glm(y ~ ., data = df, family = stats::binomial())
    } else {
      mod <- stats::glm(y ~ ., data = df)
    }
  } else {
    # glmnet (optional)
    if (!requireNamespace("glmnet", quietly = TRUE)) {
      stop("engine='glmnet' requires the 'glmnet' package. Please install it or use engine='glm'.")
    }
    m <- as.matrix(x)
    if (type == "classification") {
      mod <- glmnet::cv.glmnet(m, y, family = "binomial", ...)
    } else {
      mod <- glmnet::cv.glmnet(m, y, family = "gaussian", ...)
    }
  }

  out <- list(
    type   = type,
    engine = engine,
    model  = mod,
    feats  = names(x),
    levels = lev
  )
  class(out) <- c("langAssessR_model", class(out))
  out
}

#' Predict with a langAssessR model
#'
#' @param model object from la_fit()
#' @param newx data.frame/matrix with same feature columns as training (order flexible)
#' @param type "prob" (classification default) or "response" for regression; "label" returns 0/1 labels at threshold
#' @param threshold classification threshold for "label" predictions (default 0.5)
#' @return numeric vector (probabilities or predictions), or integer(0/1) if type="label"
#' @export
la_predict <- function(model, newx, type = NULL, threshold = 0.5) {
  stopifnot(inherits(model, "langAssessR_model"))
  newx <- as.data.frame(newx, check.names = FALSE)

  # reorder/complete columns
  miss <- setdiff(model$feats, names(newx))
  if (length(miss)) {
    # silently add zeros for missing features — baseline-friendly
    newx[miss] <- 0
  }
  newx <- newx[model$feats]

  if (is.null(type)) type <- if (model$type == "classification") "prob" else "response"

  if (model$engine == "glm") {
    if (model$type == "classification") {
      prob <- stats::predict(model$model, newdata = newx, type = "response")
      if (type == "prob") return(as.numeric(prob))
      if (type == "label") return(as.integer(prob >= threshold))
      stop("Unknown type; use 'prob' or 'label' for classification.")
    } else {
      return(as.numeric(stats::predict(model$model, newdata = newx, type = "response")))
    }
  } else { # glmnet
    m <- as.matrix(newx)
    if (model$type == "classification") {
      prob <- as.numeric(stats::predict(model$model, newx = m, s = "lambda.min", type = "response"))
      if (type == "prob") return(prob)
      if (type == "label") return(as.integer(prob >= threshold))
      stop("Unknown type; use 'prob' or 'label' for classification.")
    } else {
      return(as.numeric(stats::predict(model$model, newx = m, s = "lambda.min", type = "response")))
    }
  }
}

#' Fit "Mirror" and "Non-Mirror" models (wrappers for readability)
#'
#' @param x data.frame/matrix of features
#' @param y outcome vector
#' @param engine character: "glm" or "glmnet"
#' @param ... additional arguments passed to la_fit
#' @return langAssessR_model object
#' @export
la_fit_mirror <- function(x, y, engine = "glm", ...) la_fit(x, y, engine = engine, ...)

#' @rdname la_fit_mirror
#' @export
la_fit_nonmirror <- function(x, y, engine = "glm", ...) la_fit(x, y, engine = engine, ...)

#' Export a model card (YAML) for transparency and reproducibility
#'
#' @param model optional model object (stored invisibly)
#' @param path file path to write (default "modelcard.yml")
#' @param meta list with metadata fields (model_name, version, date, intended_use, data, method)
#' @param performance list with pooled_cv, leave_site_out (data.frame), etc.
#' @param contamination data.frame from check_contamination()
#' @param fairness list from audit_fairness()
#' @param limitations character vector
#' @param ethics list of warnings/notes
#' @return (invisibly) path written
#' @export
export_modelcard <- function(model = NULL,
                             path = "modelcard.yml",
                             meta = list(model_name = "langAssessR model",
                                         version = "0.1.0",
                                         date = format(Sys.Date(), "%Y-%m-%d"),
                                         intended_use = list(
                                           purpose = "Research on language-based assessment; not a diagnostic device.",
                                           users = c("Researchers", "Clinicians (evaluation)")
                                         ),
                                         data = list(sources = "transcripts", sites = "unknown", timeframe = "unknown",
                                                     de_identification = "redaction")),
                             performance = list(pooled_cv = list(metric = "AUROC", estimate = NA_real_, ci = c(NA_real_, NA_real_)),
                                                leave_site_out = NULL),
                             contamination = NULL,
                             fairness = NULL,
                             limitations = c("Depends on label quality", "Text-only; nonverbal cues omitted"),
                             ethics = list(deployment_warnings = c("Not for crisis detection",
                                                                   "Not for standalone clinical decisions"))) {

  # assemble a simple YAML-like list
  card <- list(
    model = meta$model_name,
    version = meta$version,
    date = meta$date,
    intended_use = meta$intended_use,
    data = meta$data,
    method = meta$method %||% list(features = "unknown", learner = "unknown", validation = "unknown", seeds = NULL),
    performance = performance,
    contamination_check = contamination,
    fairness = fairness,
    limitations = limitations,
    ethics = ethics
  )

  use_yaml <- requireNamespace("yaml", quietly = TRUE)
  if (use_yaml) {
    txt <- yaml::as.yaml(card, line.sep = "\n")
    writeLines(txt, con = path)
  } else {
    # minimal writer fallback
    con <- file(path, open = "wt"); on.exit(close(con))
    writeLines(paste0("model_name: \"", card$model, "\""), con)
    writeLines(paste0("version: \"", card$version, "\""), con)
    writeLines(paste0("date: \"", card$date, "\""), con)
    writeLines("intended_use:", con)
    writeLines(paste0("  purpose: \"", card$intended_use$purpose, "\""), con)
    writeLines(paste0("  users: [", paste0("\"", card$intended_use$users, "\"", collapse = ", "), "]"), con)
    writeLines("limitations:", con)
    for (l in limitations) writeLines(paste0("  - \"", l, "\""), con)
    # advise user to install yaml for a richer card
    writeLines("# (Install {yaml} for full, structured export)", con)
  }
  invisible(path)
}

`%||%` <- function(a, b) if (!is.null(a)) a else b

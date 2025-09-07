#' Extract simple text features
#'
#' Provides a lightweight baseline feature set from raw text:
#' - character length
#' - word count
#' - type-token ratio (unique words / total words)
#' - mean word length
#'
#' @param df data.frame with at least `id` and `text` columns
#' @param engine "classic" (default) or "embeddings" (future extension)
#' @param ... reserved for engine-specific args
#' @return data.frame with features, keyed by id
#' @export
la_features <- function(df, engine = c("classic","embeddings"), ...) {
  engine <- match.arg(engine)
  stopifnot(all(c("id","text") %in% names(df)))

  if (engine == "classic") {
    txt <- as.character(df$text)
    chars <- nchar(txt)
    words <- strsplit(txt, "\\s+")
    wc <- vapply(words, length, numeric(1))
    uniq <- vapply(words, function(w) length(unique(w)), numeric(1))
    ttr <- ifelse(wc > 0, uniq / wc, 0)
    meanlen <- vapply(words, function(w) mean(nchar(w), na.rm=TRUE), numeric(1))

    feats <- data.frame(
      id = df$id,
      char_len = chars,
      word_count = wc,
      type_token_ratio = round(ttr, 4),
      mean_word_len = round(meanlen, 3),
      stringsAsFactors = FALSE
    )
    return(feats)
  } else {
    stop("engine='embeddings' not yet implemented; see future roadmap.")
  }
}

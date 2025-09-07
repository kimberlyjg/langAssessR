#' Simulate synthetic language-based assessment data
#'
#' @param n Number of participants
#' @param n_sites Number of sites (for leave-site-out CV)
#' @param seed Random seed
#' @return A list of tidy data frames: participants, transcripts, labels,
#'         site_estimates, fairness, calibration, decision curves,
#'         survival, attention, topics, de-identification, contamination.
#' @export
simulate_lang_data <- function(n = 300, n_sites = 3, seed = 123) {
  set.seed(seed)

  id   <- sprintf("P%03d", 1:n)
  site <- sample(paste0("Site", seq_len(n_sites)), n, TRUE)
  sex  <- sample(c("Female","Male"), n, TRUE, prob = c(0.55,0.45))
  race <- sample(c("GroupA","GroupB","GroupC"), n, TRUE, prob = c(0.6,0.3,0.1))
  age  <- sample(18:75, n, TRUE)
  age_group <- cut(age, c(17,29,44,64,100), c("18-29","30-44","45-64","65+"))

  # latent risk & targets
  latent <- plogis(rnorm(n))
  y_bin  <- as.integer(latent > 0.65)
  y_ext  <- scales::rescale(latent + rnorm(n, 0, 0.15), 0:1)

  # structured vs narrative scores
  target_struct <- scales::rescale(latent + rnorm(n, 0, 0.1), 0:1)
  target_narr   <- scales::rescale(0.6*latent + rnorm(n, 0, 0.2), 0:1)

  participants <- data.frame(id, site, sex, race, age, age_group, y_bin, y_ext)

  transcripts <- list(
    structured = data.frame(id, text = paste("structured text", rnorm(n))),
    narrative  = data.frame(id, text = paste("narrative text", rnorm(n)))
  )

  labels <- data.frame(id, target_struct, target_narr, y_ext, y_bin)

  # mock site performance
  site_estimates <- data.frame(
    site = paste0("Site", seq_len(n_sites)),
    estimate = runif(n_sites, 0.72, 0.82),
    lcl = runif(n_sites, 0.68, 0.72),
    ucl = runif(n_sites, 0.82, 0.88)
  )

  # subgroup fairness
  fairness_auc <- data.frame(
    subgroup = c("Female","Male","GroupA","GroupB","GroupC","18-29","30-44","45-64","65+"),
    auc = runif(9, 0.70, 0.85),
    lcl = runif(9, 0.65, 0.70),
    ucl = runif(9, 0.85, 0.90)
  )

  # calibration bins
  bins <- seq(0.05,0.95,0.1)
  calibration <- expand.grid(subgroup=c("Female","Male"), prob_bin=bins)
  calibration$mean_pred <- bins
  calibration$obs_rate  <- pmin(pmax(bins + rnorm(nrow(calibration),0,0.05),0),1)

  # decision curve
  thr <- seq(0.01,0.8,0.01)
  decision_curves <- rbind(
    data.frame(threshold=thr, net_benefit=0.02+0.08*exp(-6*(thr-0.3)^2),
               lcl=NA,ucl=NA, policy="Model"),
    data.frame(threshold=thr, net_benefit=thr-0.5, lcl=NA,ucl=NA, policy="Treat All"),
    data.frame(threshold=thr, net_benefit=0, lcl=NA,ucl=NA, policy="Treat None")
  )

  # attention weights
  attention <- data.frame(start=1:60,
                          weight=scales::rescale(dnorm(1:60,35,7),0:1))

  # topic lift
  topic_lift <- data.frame(topic=paste0("T",1:8),
                           group=rep(c("TP","FP"),each=8),
                           lift=runif(16,0.6,1.4))

  # de-ID counts
  deid_counts <- data.frame(entity=c("PERSON","ORG","GPE","LOC"),
                            count=c(340,120,210,55))

  # contamination summary
  contamination <- data.frame(
    metric=c("In-sample AUROC","External AUROC"),
    mirror_est=c(0.90,0.69),
    nonmirror_est=c(0.83,0.72),
    delta=c(0.07,-0.03),
    risk_flag=c("High","Low")
  )

  list(participants=participants,
       transcripts=transcripts,
       labels=labels,
       site_estimates=site_estimates,
       fairness_auc=fairness_auc,
       calibration=calibration,
       decision_curves=decision_curves,
       attention=attention,
       topic_lift=topic_lift,
       deid_counts=deid_counts,
       contamination=contamination)
}

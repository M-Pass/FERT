#### FER-T 1.0 Scoring, version 2.0 (November 2026) ####
#
# No packages are required: this script runs on base R (version 3.5 or later).
#
# It estimates each participant's ability (theta) with the 2PL model and the
# item parameters of the FERT validation study (Passarelli et al., 2018), under a
# standard normal prior. The estimate is the posterior mean of theta (EAP), the
# same quantity reported by the previous version of this script. The previous
# version needed the packages 'BEST' (no longer on CRAN) and 'rstan' and sampled
# the posterior by MCMC; this version computes the posterior exactly on a fine
# grid, so it is faster and gives the same result every time.
#
# HOW TO USE
# 1. Open this file in R or RStudio and run it all (in RStudio: Source).
# 2. When prompted, choose the .csv file(s) written by PsychoPy in the "data"
#    folder. On Windows you can select several files at once.
# 3. The results are printed on screen. With more than one file, they are also
#    saved as "FERT_scores.csv" in the folder of the first file.

#### Item parameters (do not edit) ####

Item <- c("ARANG2", "ARDIS3", "ARFEA2", "ARHAP2", "ARSAD3", "ARSUR2", 
"EBANG1", "EBDIS2", "EBFEA3", "EBHAP3", "EBSAD3", "EBSUR3", 
"FFANG3", "FFDIS2", "FFFEA2", "FFHAP3", "FFSAD1", "FFSUR2", 
"FGANG2", "FGDIS2", "FGFEA2", "FGHAP3", "FGSAD1", "FGSUR3", 
"LDANG3", "LDDIS3", "LDFEA3", "LDHAP2", "LDSAD3", "LDSUR2", 
"MGANG1", "MGDIS2", "MGFEA3", "MGHAP3", "MGSAD2", "MGSUR1")

Alpha <- c(0.4651, 0.3005, 0.7169, 0.8780, 0.5474, 0.6123, 0.6064,
 0.2136, 0.7794, 1.4335, 0.5459, 0.4452, 1.6947, 0.4810, 0.5903,
 0.8826, 0.5491, 0.8933, 0.8345, 0.8804, 0.2824, 0.6313, 0.6104,
 0.4571, 0.7794, 0.5261, 0.8495, 0.6296, 0.4887, 0.6038, 1.4067,
 0.5214, 1.0957, 0.7295, 0.6006, 0.3117)

Beta <- c(-3.3289, -3.9263, -3.8320, -4.1036, -1.4685, -3.7313, -1.1982,
 0.3763, 0.3876, -4.7480, -4.7722, -6.2939, -3.3452, -6.4326, 0.4887,
 -5.8279, -1.7127, -3.8558, -2.8657, -4.3872, 0.1057, -6.1967, -3.4558,
 -3.0521, -2.8014, -4.6856, -0.3597, -3.4142, 0.2852, -3.6963, -0.9332,
 -3.5789, -1.4791, -5.4119, 0.1957, -6.5352)

Itempars <- data.frame(Item, Alpha, Beta, stringsAsFactors = FALSE)

Emotions <- c("ANG", "DIS", "FEA", "HAP", "SAD", "SUR")  # response keys 1 to 6

#### Functions ####

# Last valid response key (1-6) in each cell; NA when there is no response
last_key <- function(x) {
  s <- gsub("[^1-6]", "", as.character(x))
  out <- rep(NA_integer_, length(s))
  ok <- !is.na(s) & nchar(s) > 0
  out[ok] <- as.integer(substring(s[ok], nchar(s[ok])))
  out
}

# Read one PsychoPy output file and return one row per item
read_fert <- function(file) {
  raw <- read.csv(file, stringsAsFactors = FALSE)
  needed <- c("Stimulus", "key_resp_2.keys")
  if (!all(needed %in% names(raw))) {
    stop("'", basename(file), "' does not look like a FERT output file ",
         "(columns 'Stimulus' and 'key_resp_2.keys' not found).")
  }
  stim <- as.character(raw$Stimulus)
  trial <- !is.na(stim) & grepl("\\.png$", stim)
  item <- sub("\\.png$", "", basename(stim[trial]))
  key <- last_key(raw$key_resp_2.keys[trial])
  answer <- Emotions[key]
  data.frame(Item = item,
             Answer = answer,
             Correct = !is.na(answer) & substr(item, 3, 5) == answer,
             stringsAsFactors = FALSE)
}

# Posterior of theta on a grid: 2PL, eta = Alpha * (theta - Beta), prior N(0, 1)
score_fert <- function(file, plot = TRUE) {
  d <- read_fert(file)
  if (anyDuplicated(d$Item)) {
    stop("'", basename(file), "' contains repeated items: was it a single session?")
  }
  unknown <- setdiff(d$Item, Itempars$Item)
  if (length(unknown) > 0) {
    stop("Unknown items in '", basename(file), "': ", paste(unknown, collapse = ", "))
  }
  if (nrow(d) < nrow(Itempars)) {
    warning("'", basename(file), "' contains ", nrow(d), " of ", nrow(Itempars),
            " items: theta is estimated from the items present.")
  }
  d <- merge(d, Itempars, by = "Item")

  grid <- seq(-8, 8, by = 0.001)
  eta <- outer(grid, d$Beta, "-") * rep(d$Alpha, each = length(grid))
  loglik <- plogis(eta, log.p = TRUE) %*% d$Correct +
    plogis(-eta, log.p = TRUE) %*% (1 - d$Correct)
  logpost <- dnorm(grid, log = TRUE) + as.vector(loglik)
  w <- exp(logpost - max(logpost))
  w <- w / sum(w)

  eap <- sum(grid * w)
  psd <- sqrt(sum((grid - eap)^2 * w))
  # 95% highest density interval (the interval shown by the old posterior plot)
  ord <- order(w, decreasing = TRUE)
  inside <- ord[seq_len(which(cumsum(w[ord]) >= 0.95)[1])]
  hdi <- range(grid[inside])

  if (plot) {
    dens <- w / 0.001
    plot(grid, dens, type = "n", xlim = c(eap - 4 * psd, eap + 4 * psd),
         xlab = expression(theta), ylab = "Posterior density",
         main = paste0("FERT: ", basename(file)))
    sel <- grid >= hdi[1] & grid <= hdi[2]
    polygon(c(hdi[1], grid[sel], hdi[2]), c(0, dens[sel], 0),
            col = "grey85", border = NA)
    lines(grid, dens, lwd = 2)
    abline(v = eap, lty = 2)
    legend("topright", bty = "n",
           legend = c(paste0("Mean = ", format(round(eap, 2), nsmall = 2)),
                      paste0("95% HDI: ", format(round(hdi[1], 2), nsmall = 2),
                             " to ", format(round(hdi[2], 2), nsmall = 2))))
  }

  data.frame(File = basename(file),
             Items = nrow(d),
             Correct = sum(d$Correct),
             No_response = sum(is.na(d$Answer)),
             Theta = round(eap, 3),
             Posterior_SD = round(psd, 3),
             HDI95_low = round(hdi[1], 3),
             HDI95_high = round(hdi[2], 3),
             stringsAsFactors = FALSE)
}

#### Run ####

if (!exists("FERT_NO_RUN")) {
  files <- if (.Platform$OS.type == "windows") {
    choose.files(caption = "Choose the FERT .csv file(s) written by PsychoPy",
                 filters = matrix(c("CSV files (*.csv)", "*.csv"), 1))
  } else {
    file.choose()
  }
  results <- do.call(rbind, lapply(files, score_fert, plot = length(files) == 1))
  print(results, row.names = FALSE)
  if (nrow(results) == 1) {
    print(paste("Estimated ability: ", round(results$Theta, 2)))
  } else {
    out <- file.path(dirname(files[1]), "FERT_scores.csv")
    write.csv(results, out, row.names = FALSE)
    message("Results saved to ", out)
  }
}

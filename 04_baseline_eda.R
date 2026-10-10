# 04_baseline_eda.R
# Checkpoint I additions (base R only): label check, one EDA figure, first baseline.
#   Input : pipeline/projects_clean.csv   (from 02_clean_and_label.R)
#   Output: printed numbers, fig_overrun_by_year.pdf
# Usage: Rscript pipeline/04_baseline_eda.R [clean.csv] [figure.pdf]

args <- commandArgs(trailingOnly = TRUE)
clean_path <- if (length(args) >= 1) args[1] else "pipeline/projects_clean.csv"
fig_path   <- if (length(args) >= 2) args[2] else "pipeline/fig_overrun_by_year.pdf"
set.seed(42)
d <- read.csv(clean_path, stringsAsFactors = FALSE, na.strings = c("", "NA"))
cat("R version:", R.version.string, "\n")

# ---------- A. near-complete subset: how much of 'overrun by spending' is mechanical? ----------
nc <- d[d$near_complete %in% TRUE & !is.na(d$label_spent), ]
norev <- nc[abs(nc$cost_anticipated - nc$cost_original) <= 1e-9, ]
cat("\n[A] near-complete:", nrow(nc), "| label_spent = 1:", sum(nc$label_spent),
    "| of these also label_anticipated = 1:", sum(nc$label_spent == 1 & nc$label_anticipated == 1), "\n")
cat("    no revision (anticipated == original):", nrow(norev),
    "| spent more than original (= more than the official estimate):", sum(norev$label_spent),
    sprintf("(%.1f%%)", 100 * mean(norev$label_spent)), "\n")

# ---------- B. figure: overrun share by approval year ----------
h <- d[!is.na(d$label_anticipated) & !is.na(d$approval_year), ]
h$yr <- ifelse(h$approval_year <= 2012, 2012, h$approval_year)        # pool sparse early years
tab <- aggregate(label_anticipated ~ yr, data = h, FUN = mean)
n   <- aggregate(label_anticipated ~ yr, data = h, FUN = length)
pdf(fig_path, width = 3.5, height = 2.3, pointsize = 8)
par(mar = c(3.2, 3.4, 0.6, 0.4), mgp = c(2.1, 0.6, 0))
lab <- substr(tab$yr, 3, 4); lab[tab$yr == 2012] <- ""
bp <- barplot(100 * tab$label_anticipated, names.arg = lab,
              col = "grey60", border = NA, ylim = c(0, 80), las = 1,
              xlab = "Approval year", ylab = "Overrun by estimate (%)")
text(bp, 100 * tab$label_anticipated + 4, n$label_anticipated, cex = 0.75)
mtext(expression(phantom() <= 12), side = 1, at = bp[1], line = 0.6, cex = 1)
dev.off()
print(cbind(tab, n = n$label_anticipated))

# ---------- C. baseline: majority class and logistic regression ----------
m <- d[!is.na(d$label_anticipated) & !is.na(d$approval_year), ]
m$y <- m$label_anticipated
m$log_cost <- log(m$cost_original)
m$dur_missing <- as.integer(is.na(m$planned_duration))

prep <- function(tr, te) {                     # everything is fitted on the training part only
  med <- median(tr$planned_duration, na.rm = TRUE)
  for (z in c("tr", "te")) {
    x <- get(z)
    x$log_dur <- log(ifelse(is.na(x$planned_duration), med, x$planned_duration))
    assign(z, x)
  }
  keep_state <- names(which(table(tr$state) >= 10))           # rare states -> "other"
  keep_sector <- names(which(table(tr$sector) >= 10))
  for (z in c("tr", "te")) {
    x <- get(z)
    x$state_g  <- factor(ifelse(x$state %in% keep_state, x$state, "other"), levels = c(keep_state, "other"))
    x$sector_g <- factor(ifelse(x$sector %in% keep_sector, x$sector, "other"), levels = c(keep_sector, "other"))
    assign(z, x)
  }
  list(tr = tr, te = te)
}
auc <- function(y, p) { r <- rank(p); n1 <- sum(y == 1); n0 <- sum(y == 0)
  (sum(r[y == 1]) - n1 * (n1 + 1) / 2) / (n1 * n0) }
ap <- function(y, p) { o <- order(-p); y <- y[o]; mean(cumsum(y)[y == 1] / which(y == 1)) }
scores <- function(y, p, thr = 0.5) {
  yhat <- as.integer(p >= thr); tp <- sum(yhat == 1 & y == 1)
  prec <- if (sum(yhat) > 0) tp / sum(yhat) else NA; rec <- tp / sum(y == 1)
  c(acc = mean(yhat == y), prec = prec, rec = rec, f1 = 2 * prec * rec / (prec + rec),
    roc_auc = auc(y, p), pr_auc = ap(y, p))
}
run <- function(tr, te, label) {
  s <- prep(tr, te); tr <- s$tr; te <- s$te
  w <- ifelse(tr$y == 1, nrow(tr) / (2 * sum(tr$y == 1)), nrow(tr) / (2 * sum(tr$y == 0)))  # class weights
  fit <- suppressWarnings(glm(y ~ log_cost + log_dur + dur_missing + approval_year + sector_g + state_g,
                              data = tr, family = binomial, weights = w))
  p <- suppressWarnings(predict(fit, te, type = "response"))
  base <- c(acc = mean(te$y == 0), prec = NA, rec = 0, f1 = NA, roc_auc = 0.5, pr_auc = mean(te$y))
  cat("\n[C]", label, "| train", nrow(tr), "(overrun", sprintf("%.1f%%", 100 * mean(tr$y)), ") | test", nrow(te),
      "(overrun", sprintf("%.1f%%", 100 * mean(te$y)), ")\n")
  print(round(rbind(majority = base, logistic = scores(te$y, p)), 3))
}
# random stratified 80/20 split
idx1 <- which(m$y == 1); idx0 <- which(m$y == 0)
te_i <- c(sample(idx1, round(0.2 * length(idx1))), sample(idx0, round(0.2 * length(idx0))))
run(m[-te_i, ], m[te_i, ], "random stratified split")
# time-based split: approved up to 2020 -> train, 2021-2024 -> test
run(m[m$approval_year <= 2020, ], m[m$approval_year >= 2021, ], "time split (train <= 2020, test >= 2021)")

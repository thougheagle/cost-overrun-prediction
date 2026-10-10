# 03_audit.R
# Label audit: every number quoted in the Checkpoint I report (base R only).
#   Input: pipeline/projects_clean.csv   (from 02_clean_and_label.R)

args <- commandArgs(trailingOnly = TRUE)
clean_path <- if (length(args) >= 1) args[1] else "pipeline/projects_clean.csv"
d <- read.csv(clean_path, stringsAsFactors = FALSE, na.strings = c("", "NA"))

pct <- function(x) sprintf("%.1f%%", 100 * x)
have <- !is.na(d$cost_original) & !is.na(d$cost_anticipated)
h <- d[have, ]
cat("projects total:", nrow(d), "| with original and anticipated cost:", nrow(h), "\n")

# [1] overrun by official estimate; [2] anticipated == original
pos   <- sum(h$label_anticipated == 1)
equal <- sum(abs(h$cost_anticipated - h$cost_original) <= 1e-9)
under <- sum(h$cost_anticipated < h$cost_original - 1e-9)
cat("\n[1] anticipated > original:", pos, "of", nrow(h), "=", pct(pos / nrow(h)), "\n")
cat("    anticipated < original (underrun):", under, "| equal:", equal, "\n")
cat("[2] anticipated == original:", equal, "of", nrow(h), "=", pct(equal / nrow(h)), "\n")
cat("    formally revised cost printed:", sum(!is.na(d$cost_revised)), "of", nrow(d),
    "=", pct(sum(!is.na(d$cost_revised)) / nrow(d)), "\n")

# [3] original commissioning date 2022 or earlier, but still no overrun
old <- h[!is.na(h$doc_original_year) & h$doc_original_year <= 2022, ]
cat("[3] original date <= 2022:", nrow(old), "projects; still 'no overrun':",
    sum(old$label_anticipated == 0), "\n")

# [4] zero expenditure
z <- d[!is.na(d$expenditure) & d$expenditure == 0, ]
cat("[4] zero expenditure:", nrow(z), "| approved <= 2019:",
    sum(z$approval_year <= 2019, na.rm = TRUE), "\n")

# [5] stage effect: share of money spent
hh <- h[!is.na(h$expenditure), ]
hh$spent <- hh$expenditure / hh$cost_anticipated
hh$over_est   <- hh$label_anticipated == 1
hh$over_spent <- hh$expenditure > hh$cost_original
grp <- function(name, m) {
  g <- hh[m, ]
  cat("   ", name, ": n =", nrow(g), "| overrun by estimate", pct(mean(g$over_est)),
      "| by spending", pct(mean(g$over_spent)), "\n")
}
cat("[5] stage effect (spent = expenditure / anticipated cost)\n")
grp(">=95% spent (near-complete)", hh$spent >= 0.95)
grp(">=90% spent", hh$spent >= 0.90)
grp("<50% spent", hh$spent < 0.50)
grp("50-90% spent", hh$spent >= 0.50 & hh$spent < 0.90)
cat("    label_spent defined on", sum(!is.na(d$label_spent)), "projects; overrun share",
    pct(mean(d$label_spent, na.rm = TRUE)), "\n")

# [6] columns
cat("\n[6] state:", length(unique(na.omit(d$state))), "values | sector:",
    length(unique(na.omit(d$sector))), "values\n")
print(head(sort(table(d$sector), decreasing = TRUE), 6))
cat("    missing approval:", sum(is.na(d$approval)), "| missing original date:",
    sum(is.na(d$doc_original)), "| planned_duration missing:", sum(is.na(d$planned_duration)), "\n")

# [7] overrun in percent among overrun projects (for the three-class idea)
ov <- h$overrun_pct[h$label_anticipated == 1]
cat("\n[7] overrun % among the", length(ov), "overrun projects\n")
print(round(quantile(ov, c(0, .25, .5, .75, .9, 1)), 1))
cat("    overruns below 0.1% of original cost:", sum(ov < 0.1), "\n")

# [8] baseline
cat("\n[8] baseline 'always no overrun': accuracy", pct(1 - pos / nrow(h)),
    "on the full panel;", pct(1 - mean(d$label_spent, na.rm = TRUE)),
    "on the near-complete subset (majority there is overrun)\n")

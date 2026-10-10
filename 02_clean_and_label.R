# 02_clean_and_label.R
# Step 2 of the pipeline (base R only, no packages needed).
#   Input : pipeline/projects_raw.csv   (written once by parse_annex18_fixed.py)
#   Output: pipeline/projects_clean.csv (typed columns, two labels, approval-stage features)
# Run from the project folder (open roject.Rproj in RStudio), or:
#   Rscript pipeline/02_clean_and_label.R [raw.csv] [clean.csv]

args <- commandArgs(trailingOnly = TRUE)
raw_path   <- if (length(args) >= 1) args[1] else "pipeline/projects_raw.csv"
clean_path <- if (length(args) >= 2) args[2] else "pipeline/projects_clean.csv"

d <- read.csv(raw_path, stringsAsFactors = FALSE, na.strings = c("", "NA"))
cat("rows read:", nrow(d), "\n")

# ---------- 1. types ----------
# "M/YYYY" -> month and year (NA if missing or not a valid month/year, e.g. "0/0")
parse_my <- function(x) {
  ok <- !is.na(x) & grepl("^[0-9]{1,2}/[0-9]{4}$", x)
  m <- rep(NA_integer_, length(x)); y <- m
  m[ok] <- as.integer(sub("/.*", "", x[ok]))
  y[ok] <- as.integer(sub(".*/", "", x[ok]))
  bad <- ok & (m < 1 | m > 12)
  m[bad] <- NA_integer_; y[bad] <- NA_integer_
  list(m = m, y = y)
}

ap <- parse_my(d$approval)
do <- parse_my(d$doc_original)
d$approval_year <- ap$y
d$approval_month <- ap$m
d$doc_original_year <- do$y
d$doc_original_month <- do$m

for (cc in c("cost_original", "cost_revised", "cost_anticipated", "expenditure",
             "cost_overrun_cr", "time_overrun_m")) {
  d[[cc]] <- suppressWarnings(as.numeric(d[[cc]]))
}

# ---------- 2. validation: anticipated - original must equal the reported overrun ----------
have <- !is.na(d$cost_original) & !is.na(d$cost_anticipated)
d$overrun_cr_computed <- d$cost_anticipated - d$cost_original
cmp <- have & !is.na(d$cost_overrun_cr)
ok  <- cmp & abs(d$overrun_cr_computed - d$cost_overrun_cr) <= 0.011
cat("\n=== Validation: anticipated - original vs. reported overrun ===\n")
cat("projects with original and anticipated cost:", sum(have), "of", nrow(d), "\n")
cat("projects with a printed overrun:", sum(cmp), "| computed == printed:", sum(ok),
    "| differ:", sum(cmp & !ok), "\n")

# ---------- 3. approval-stage features ----------
d$planned_duration <- (d$doc_original_year - d$approval_year) * 12 +
                      (d$doc_original_month - d$approval_month)
d$planned_duration[!is.na(d$planned_duration) & d$planned_duration <= 0] <- NA
d$cost_per_month <- d$cost_original / d$planned_duration

# ---------- 4. labels ----------
d$overrun_pct <- (d$cost_anticipated - d$cost_original) / d$cost_original * 100
d$label_anticipated <- ifelse(have, as.numeric(d$cost_anticipated > d$cost_original + 1e-9), NA)
d$spent_share <- d$expenditure / d$cost_anticipated
d$near_complete <- !is.na(d$spent_share) & d$spent_share >= 0.95
d$label_spent <- ifelse(d$near_complete & !is.na(d$expenditure) & !is.na(d$cost_original),
                        as.numeric(d$expenditure > d$cost_original), NA)

write.csv(d, clean_path, row.names = FALSE, na = "")
cat("\nsaved", clean_path, ":", nrow(d), "rows,", ncol(d), "columns\n")

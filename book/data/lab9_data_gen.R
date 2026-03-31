## lab9_data_gen.R
## Generates lab9_dif_data.csv for Week 11 DIF lab
## Run once; save the output CSV and distribute to students
## 
## Design:
##   N = 400 (200 reference, 200 focal)
##   15 items
##   Items with DIF:
##     Item 3  — Class B uniform DIF, favors reference (Δ ≈ +1.2)
##     Item 7  — Class C uniform DIF, favors reference (Δ ≈ +1.9)
##     Item 10 — Class C uniform DIF, favors focal (Δ ≈ −1.6)
##     Item 12 — Non-uniform DIF (crossing interaction)
##     Item 13 — Class B uniform DIF, favors focal (Δ ≈ −1.1)

set.seed(2024)

N <- 400
n_ref <- 200
n_foc <- 200

# True ability: focal group mean slightly lower (impact, not just bias)
theta_ref <- rnorm(n_ref, mean = 0.0, sd = 1.0)
theta_foc <- rnorm(n_foc, mean = -0.3, sd = 1.0)
theta <- c(theta_ref, theta_foc)
group <- c(rep(0, n_ref), rep(1, n_foc))

# Item parameters (1PL / Rasch-like difficulty values)
b <- c(-0.8, -0.5, 0.0, 0.2, -0.3,
       0.5,  0.1, -0.2, 0.4, -0.6,
       0.3,  0.0, -0.1, 0.7, -0.4)

# Generate item responses — base probabilities from Rasch model
p_correct <- function(theta, b) 1 / (1 + exp(-(theta - b)))

items <- matrix(NA, nrow = N, ncol = 15)
for (j in 1:15) {
  p <- p_correct(theta, b[j])
  items[, j] <- rbinom(N, 1, p)
}

# ── Introduce DIF ─────────────────────────────────────────────────────────────

## Item 3: Class B uniform DIF, favors reference (reference easier by ~0.4 logits)
b3_focal <- b[3] + 0.45        # harder for focal
p3_foc   <- p_correct(theta_foc, b3_focal)
items[(n_ref + 1):N, 3] <- rbinom(n_foc, 1, p3_foc)

## Item 7: Class C uniform DIF, favors reference (reference easier by ~0.7 logits)
b7_focal <- b[7] + 0.72
p7_foc   <- p_correct(theta_foc, b7_focal)
items[(n_ref + 1):N, 7] <- rbinom(n_foc, 1, p7_foc)

## Item 10: Class C uniform DIF, favors focal (focal easier by ~0.6 logits)
b10_ref <- b[10] + 0.62
p10_ref <- p_correct(theta_ref, b10_ref)
items[1:n_ref, 10] <- rbinom(n_ref, 1, p10_ref)

## Item 12: Non-uniform DIF — interaction with ability
## Focal group: item gets easier at low ability, harder at high ability
## Achieved by a crossing logistic curve
theta_foc_vals <- theta[(n_ref + 1):N]
# non-uniform: add a term that reverses direction
p12_foc <- plogis(1.2 * theta_foc_vals - 0.5 * theta_foc_vals^2 - 0.2)
p12_foc <- pmax(0.05, pmin(0.95, p12_foc))
items[(n_ref + 1):N, 12] <- rbinom(n_foc, 1, p12_foc)

## Item 13: Class B uniform DIF, favors focal (focal easier by ~0.38 logits)
b13_ref <- b[13] + 0.38
p13_ref <- p_correct(theta_ref, b13_ref)
items[1:n_ref, 13] <- rbinom(n_ref, 1, p13_ref)

# ── Assemble and save ──────────────────────────────────────────────────────────

dat <- data.frame(
  id    = 1:N,
  group = group,
  as.data.frame(items)
)
colnames(dat)[3:17] <- paste0("item", 1:15)

write.csv(dat, "lab9_dif_data.csv", row.names = FALSE)
cat("Dataset written: lab9_dif_data.csv\n")
cat("N =", nrow(dat), "| Groups:", table(dat$group), "\n")
cat("Item means (proportion correct):\n")
print(round(colMeans(dat[, 3:17]), 3))

# ── Quick verification with difR ───────────────────────────────────────────────
# Uncomment and run to confirm DIF items are recovering as expected
# library(difR)
# items_only <- dat[, 3:17]
# total      <- rowSums(items_only)
# mh         <- difMH(items_only, dat$group, focal.name = 1, match = total, correct = TRUE)
# print(mh)

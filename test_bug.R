mat <- matrix(runif(30, 0, 10), nrow = 3)
colnames(mat) <- c("0min", "30min", "1h", "2h", "3h", "60min", "90min", "120min", "10h", "T01")
rownames(mat) <- c("G1", "G2", "G3")

print("colnames matched by grep('0min|T01'):")
print(colnames(mat)[grep("0min|T01", colnames(mat))])

mat_aligned <- mat
baseline_cols <- grep("0min|T01", colnames(mat_aligned))

print("Baseline cols indices:")
print(baseline_cols)

# simulate what happens for z-score minus t0
mat_scaled <- t(scale(t(mat_aligned)))
mat_scaled[!is.finite(mat_scaled)] <- 0

# what if we use the correct regex?
correct_baseline_cols <- grep("^0min$|^T01$", colnames(mat_aligned))
print("Correct baseline cols:")
print(correct_baseline_cols)

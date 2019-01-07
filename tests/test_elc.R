library(DCGen)

expert1 <- input_norm(c(10, 2))
expert2 <- input_gamma(c(0.1, 0.01))
# Aggregated expert
experts <- aggregate_elicitations(list(expert1, expert2))
experts


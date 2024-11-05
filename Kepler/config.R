experiment_config <- list(
  count = 1,
  transforms = c("sigmoid","sin_deg","exp_dbl","p0","troot","p3","p2"),
  eps = 0.05,
  D = 5,
  L = 5,
  B = c(1,1),
  P = c(5,5),
  N_init = 1000,
  N_final = 3000,
  Q = c(15,30),
  probs = list(c(0.22,0.22,0.06,0.06,0.22,0.22),c(0.3,0.3,0.1,0.1,0.1,0.1),c(0.4,0.4,0.1,0.1,0,0))
)


# experiment_config <- list(
#   count = 50,
#   transforms = c("sigmoid","sin_deg","exp_dbl","p0","troot","p3","p2"),
#   eps = 0.05,
#   D = 5,
#   L = 5,
#   B = c(1,5),
#   P = c(500,50),
#   N_init = 1000,
#   N_final = 3000,
#   Q = c(15,30),
#   probs = list(c(0.22,0.22,0.06,0.06,0.22,0.22),c(0.3,0.3,0.1,0.1,0.1,0.1),c(0.4,0.4,0.1,0.1,0,0))
# )

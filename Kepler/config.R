# experiment_config <- list(
#   count = 3,
#   transforms = c("sigmoid","sin_deg","exp_dbl","troot","p3","p2"),
#   eps = 0.05,
#   D = 5,
#   L = 5,
#   B = c(1,1),
#   P = c(5,5),
#   N_init = 1000,
#   N_final = 3000,
#   Q = c(15,30),
#   probs = list(c(0.22,0.22,0.06,0.06,0.22,0.22),c(0.3,0.3,0.1,0.1,0.1,0.1),c(0.4,0.4,0.1,0.1,0,0))
# )


experiment_config <- list(
  count = 30,
  transforms = c("sigmoid","sin_deg","exp_dbl","troot","p0","p3"),
  eps = 0.05,
  D = 10,
  L = 5,
  B = c(1,4),
  P = c(200,50),
  N_init = 1000,
  N_final = 3000,
  Q = c(15,30),
  probs = list(c(0.22,0.22,0.06,0.06,0.22,0.22),c(0.3,0.3,0.1,0.1,0.1,0.1),c(0.4,0.4,0.1,0.1,0,0))
)

# experiment_config <- list(
#   count = 1,
#   transforms = c("sigmoid","sin_deg","exp_dbl","troot","p0","p3"),
#   eps = 0.05,
#   D = 10,
#   L = 5,
#   B = c(1,4),
#   P = c(2,5),
#   N_init = 100,
#   N_final = 300,
#   Q = c(15,30),
#   probs = list(c(0.22,0.22,0.06,0.06,0.22,0.22),c(0.3,0.3,0.1,0.1,0.1,0.1),c(0.4,0.4,0.1,0.1,0,0))
# )




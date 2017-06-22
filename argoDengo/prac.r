

library(argo)
all_data <- load_data()
santillana_etal <- list()
our_argo <- list()

for(dtname in c("GC10","GC09","GT")){
  exogen <- all_data[[dtname]]
  yx_merged <- merge(all_data$CDC, exogen, join = "right")
  y <- yx_merged$CDC.data
  santillana_etal[[dtname]] <-
    argo(y, exogen = exogen,
         alpha = 1, N_lag = NULL, use_all_previous = TRUE,  N_training = 104)

  our_argo[[dtname]] <-
    argo(logit(y / 100), exogen = log((exogen + 0.5) / 100),
         alpha = 1, N_lag=1:52, use_all_previous = FALSE, N_training = 104)
}
ar3 <- argo(all_data$CDC, alpha = NA, use_all_previous = FALSE, N_lag=1:3,
            N_training = 104)

ar3_gft <- argo(all_data$CDC[zoo::index(all_data$GFT)], exogen = all_data$GFT,
                alpha = NA, use_all_previous = FALSE, N_lag=1:3,
                N_training = 104)


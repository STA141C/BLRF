blrf <- function(formula, data, newdata, s, gamma, r, workers = 1){

  n <- nrow(data)
  b = ceiling(n^gamma)

  plan(multiprocess, workers = workers)

  future_map(seq_len(s),

             ~{
                I <- sample(n, b, replace = FALSE)


                map_dbl(seq_len(r),

                    ~{
                      freqs <- rmultinom(1, n, rep(1, b))
                      t <- tree(formula, I, weights = freqs)
                      predict(t, newdata)
                    }
                    ) %>%
                  mean()??

             }
  )




}

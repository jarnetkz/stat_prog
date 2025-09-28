# (7)  Step 7 (Yiheng) — next.word() per handout
next.word <- function(key, M, M1, w = rep(1, ncol(M) - 1L)) {
  mlag <- ncol(M) - 1L
  key  <- as.integer(key)
  if (length(key) > mlag) key <- tail(key, mlag)       
  Lmax <- min(length(key), mlag)                      
  
  tok <- integer(0)                                    
  pr  <- numeric(0)                                    
  
  for (L in Lmax:1L) {
    kL <- tail(key, L)
    mc <- mlag - L + 1L
    ii  <- colSums(!(t(M[, mc:mlag, drop = FALSE]) == kL))
    hit <- which(ii == 0 & is.finite(ii))
    if (length(hit)) {
      u <- M[hit, mlag + 1L]
      u <- u[!is.na(u)]
      if (length(u)) {
        pr  <- c(pr,  rep(w[L] / length(u), length(u)))
        tok <- c(tok, u)
      }
    }
  }

  if (!length(tok)) {
    pool <- unique(M1[!is.na(M1)])
    return(sample(pool, size = 1L))
  }
  
  mix <- tapply(pr, tok, sum)
  tokens <- as.integer(names(mix))
  probs  <- as.numeric(mix)
  probs  <- probs / sum(probs)
  sample(tokens, size = 1L, prob = probs)
}

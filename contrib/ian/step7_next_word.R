# (7)  Step 7 (Yiheng) — next.word()
#' @param key integer token vector (current context)
#' @param M   (n-mlag) x (mlag+1) window matrix; last column = next token
#' @param M1  full token vector of the text
#' @param w   weights for backoff orders; length = mlag (default: all 1)
#' @return    integer token id of the next word
next.word <- function(key, M, M1, w = rep(1, ncol(M) - 1L)) {
  mlag <- ncol(M) - 1L  # model order
  key  <- as.integer(key) # ensure integer token ids
  if (length(key) > mlag) key <- tail(key, mlag)       
  Lmax <- min(length(key), mlag)                      
  
  tok <- integer(0) # collected candidate next tokens                                   
  pr  <- numeric(0) # corresponding (unnormalized) weights                                    
  
  for (L in Lmax:1L) {
    kL <- tail(key, L) # suffix of length L to match
    mc <- mlag - L + 1L # start column in M that aligns this suffix
    #rows where all L columns equal kL produce ii == 0
    ii  <- colSums(!(t(M[, mc:mlag, drop = FALSE]) == kL))
    hit <- which(ii == 0 & is.finite(ii))
    if (length(hit)) {
      u <- M[hit, mlag + 1L] # next-token column is the last one
      u <- u[!is.na(u)] # drop missing next tokens
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

setwd("/Users/jarnetkz/postgraduate/statistical_prog/stat_prog")
v_ip <- scan("shakespeare.txt",what="character",skip=83,nlines=196043-83,
          fileEncoding="UTF-8") ## Loads file
# v_ip <-a
# v_ip<- scan("shakespeare_mini.txt",what="character",skip=4,nlines=196043-83,
          # fileEncoding="UTF-8")

## Get the index that has "[" , "]"
idx_opnb = grep("[", v_ip, fixed=TRUE) 
idx_clsb = grep("]", v_ip, fixed=TRUE) 

to_remove <- logical(length(v_ip)) ## Creates a logic vector of length of v_ip

## Prepare the index we want to remove
for (i in seq_along(idx_opnb)) {
  start <- idx_opnb[i] ## Start at [ bracket
  end <- idx_clsb[idx_clsb > start & idx_clsb <= start + 100] ## Search within 100 of this for a ] bracket
  
  ## Choose first instance of ] bracket after [ bracket
  if (length(end) > 0) { 
    end <- end[1]
  } else {
    end <- start
  }
  
  ## Change our logic vector to show TRUE for all values between our start and stop variables
  to_remove[start:end] <- TRUE
}

## Removes all words from v_ip that are in the brackets
clean_data <- v_ip[!to_remove]
upper_data <- toupper(clean_data)

# filter out UPPERCASE words (except I , A)
filtered_expt_AI = clean_data[(clean_data != toupper(clean_data)) | 
                             (("I" == upper_data) | ("A" == upper_data))]

# remove character - _ out of filtered_expt_AI
filtered_vec = gsub("[_-]", "", filtered_expt_AI)

## Function to split punctuation from words
split_punct <- function(filtered_vec, spcl_char) {
  spcl_pattern = paste0("[", paste(spcl_char, collapse=""), "]") ## Concatenate a string of punctuation togethe with no space
  i_spcl_char <- grep(spcl_pattern, filtered_vec) 
  new_vec <- rep("",length(filtered_vec) + length(i_spcl_char)) ## Create character empty string
  i_new_char <- i_spcl_char + seq_along(i_spcl_char) ## Compute new location after splitting
  new_vec[i_new_char] <- substr(filtered_vec[i_spcl_char], nchar(filtered_vec[i_spcl_char]), nchar(filtered_vec[i_spcl_char]))
  new_vec[-i_new_char] <- gsub(spcl_pattern, "", filtered_vec)
  return(new_vec)
}

## Choose our punctuation
spcl_char_vec <- c(",", ".", ";", "!", ":", "?")
v_output <- split_punct(filtered_vec, spcl_char_vec)

## Final cleaned vector for further work
v_lower <- tolower(v_output)

b_all <- unique(v_lower) ## Vector of all unique tokens in v_lower
idx   <- match(v_lower, b_all) ## Indices of each token
freq  <- tabulate(idx, nbins = length(b_all)) ## Frequency count of all tokens in our vector
ord   <- order(-freq, seq_along(freq)) ## Sort by popularity      
k     <- 1000L
b     <- b_all[ ord[ seq_len(min(k, length(ord))) ] ] ## Select first 1000 words from our ordered unique words

 
.PUNCT <- c(",", ".", ";", "!", ":", "?")
b <- unique(c(b, .PUNCT)) ## Adds punctuation to our word tokens

## 6: tokens & context matrix
M1 <- match(v_lower, b)                        
storage.mode(M1) <- "integer"
mlag <- 4L
n <- length(M1); nr <- n - mlag; stopifnot(nr > 0L) ## Stop condition

## Generate a matrix of NAs
M <- matrix(NA_integer_, nrow = nr, ncol = mlag + 1L)

## Generate our sliding window vector
for (j in 0:mlag) M[, j + 1L] <- M1[(1L + j):(nr + j)]


## 7: next.word
# key: integer token ids; M: (n-mlag) x (mlag+1) (last col = next token)
# M1: full-text tokens (integers) over same vocab; w: mixture weights
next.word <- function(key, M, M1, w = rep(1, ncol(M) - 1L)) {
  mlag <- ncol(M) - 1L
  key  <- as.integer(key) # ensure integers
  ## Take the last mlag words of the key if this is larger than mlag
  if (length(key) > mlag) key <- tail(key, mlag) 
  Lmax <- min(length(key), mlag) # max usable order

  cand <- integer(0)                                    
  prob <- numeric(0)                                    

  for (L in Lmax:1L) {                                  # back off: L = Lmax..1
    if (L <= 0L) next
    kL <- tail(key, L)                                  # last L tokens
    mc <- mlag - L + 1L                                 # starting column

    block <- M[, mc:mlag, drop = FALSE]                 # context block (nrow x L)
    if (!is.matrix(block)) block <- cbind(block)        # ensure matrix
    eq <- block == rep(kL, each = nrow(block))          # compare with suffix
    eq[is.na(eq)] <- FALSE                              # NA never matches
    hit <- which(rowSums(eq) == ncol(block))           

    if (length(hit)) {
      uL <- M[hit, mlag + 1L]                           # corresponding next tokens
      uL <- uL[!is.na(uL)]
      if (length(uL)) {
        cand <- c(cand, uL)
        prob <- c(prob, rep(w[L] / length(uL), length(uL))) # equal share within order
      }
    }
  }

  # 0-gram fallback: global frequency
  if (!length(cand)) {
    pool <- as.integer(M1[!is.na(M1)])
    if (!length(pool)) return(NA_integer_)
    tab <- tabulate(pool, nbins = max(M1, na.rm = TRUE))
    return(sample.int(length(tab), 1L, prob = tab))
  }

  prob <- prob / sum(prob)                              # normalize
  sample(cand, 1L, prob = prob)                         # sample a next token
}

## part 8 9 from yiheng
.PUNCT <- c(",", ".", ";", "!", ":", "?")

generate_sentence <- function(b, M, M1,
                              w = rev(seq_len(ncol(M) - 1L))^2,  
                              seed_word = NULL,
                              max_len = 60L,
                              min_len = 8L,
                              tries_if_punct = 6L) {

  pick_seed <- function() {
    pool <- M1[!is.na(M1)]
    pool <- pool[!(b[pool] %in% .PUNCT)]
    if (!length(pool)) return(NA_integer_)
    tab <- tabulate(pool, nbins = length(b))
    sample.int(length(b), 1L, prob = tab)
  }
  seed <- if (is.null(seed_word)) {
    pick_seed()
  } else {
    id <- match(tolower(seed_word), b)
    if (is.na(id) || b[id] %in% .PUNCT) pick_seed() else id
  }

  out <- seed; key <- seed
  for (i in seq_len(max_len)) {
    nx <- NA_integer_
    for (t in seq_len(tries_if_punct)) {
      nx <- next.word(key, M, M1, w)
      if (is.na(nx)) break
      if (length(out) < min_len && b[nx] == ".") next
      break
    }
    if (is.na(nx)) break
    out <- c(out, nx)
    if (b[nx] == ".") break
    key <- c(key, nx)
  }

  s <- paste(b[out], collapse = " ")
  s <- gsub(" +([,.;!:?])", "\\1", s) 
  sub(" +$", "", s)
}
##over
## test code
set.seed(42)
cat("Sample A:", generate_sentence(b, M, M1, w), "\n")
cat("Sample B (seed='romeo'):", generate_sentence(b, M, M1, w, seed_word="romeo"), "\n")
cat("Sample C (seed='king') :", generate_sentence(b, M, M1, w, seed_word="king"), "\n")

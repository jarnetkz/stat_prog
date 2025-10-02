## This code generates a small language model by taking Shakespeare's works and 
## cleaning them. It then tokenises them, builds a simple 4-gram model (though 
## it could be updated to work for any n-gram relatively easily) and then generates 
## a new sentence in the style of Shakespeare, from a seed word, or random word if no
## seed word is given. 

## We begin the code by cleaning the data, removing stage directions, uppercase names,
## some punctuation. We then tokenise everything and generate a sliding window vector 
## based off the top 1000 most popular words. The next.word function predicts the 
## most likely following word by collecting candidate words based off sections of the 
## passed key and assigning probabilities. After obtaining all candidate words, it chooses one
## of the options randomly. If no candidate word is found, then it returns a random one 
## from the original text, based on overall frequencies. Finally, this process is repeated using 
## the generate sentence function until a full stop is reached, unless the first sentece
## is under 6 words.

setwd("/Users/jarnetkz/postgraduate/statistical_prog/stat_prog")
v_ip <- scan("shakespeare.txt",what="character",skip=83,nlines=196043-83,
          fileEncoding="UTF-8") ## Loads file

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

# generate_sentence:
# Generate a readable sentence from a tokenised Shakespeare corpus using a
# simple back-off n-gram sampler.
# Starting from a given seed word (or a high-frequency non-punctuation token
# if none is provided), generate a sentence by repeatedly sampling the next
# word from the back-off n-gram model.
# The process stops at the first period '.' encountered.


# Design rationale:
# To avoid extremely short sentences (e.g. “Romeo dead.”), we add a very
# simple safeguard: a minimum length (min_len). Before this threshold,
# if '.' is sampled we re-sample a few times (max 4). This still respects
# the rule “stop at a full stop,” but reduces premature termination.

# Parameters:
#   b         character(), vocabulary (length |b|).
#   M         integer matrix, context/next-token table as above.
#   M1        integer(), full token stream over b.
#   w         numeric(), mixture weights for back-off; same length as number
#             of context columns (mlag). Defaults to equal weights.
#   seed_word character(1) or NULL; if provided and valid it is used as seed.
#   min_len   integer(1) >= 1; before this length, '.' is re-sampled (up to 4 tries).

generate_sentence <- function(b, M, M1,
                              w = rep(1, ncol(M)-1L),
                              seed_word = NULL,
                              min_len = 6L) {
  .PUNCT <- c(",", ".", ";", "!", ":", "?")
  
# Choose a non-punctuation seed.
# We frequency-weight by counts in M1 (full corpus) to avoid extremely rare
# seeds that often lead to sparse contexts and poor continuations.
  pick_seed <- function() {
    pool <- M1[!is.na(M1)]
    pool <- pool[!(b[pool] %in% .PUNCT)]
    tab  <- tabulate(pool, nbins = length(b))
    # sample.int(k, 1, prob = p) draws an id in 1..k with probability p.
    sample.int(length(b), 1L, prob = tab)
  }
  
# Resolve the seed:
# if a seed_word is provided and found in b (and not punctuation), use it;
# otherwise, fall back to a frequency-weighted seed from M1.
  seed <- if (is.null(seed_word)) pick_seed() else {
    id <- match(tolower(seed_word), b)
    if (is.na(id) || b[id] %in% .PUNCT) pick_seed() else id
  }
  
  out <- seed; key <- seed
  
  repeat {
    nx <- next.word(key, M, M1, w)
    if (is.na(nx)) break # defensive exit
    
# Minimum-length safeguard:
# If the sentence is still shorter than min_len and we happen to sample '.',
# try re-sampling up to 4 times to avoid stopping too early.
# This keeps the "stop at '.'" rule intact while preventing trivial outputs.
    if (length(out) < min_len && b[nx] == ".") {
      tries <- 0L
      repeat {
        nx <- next.word(key, M, M1, w)
        if (is.na(nx) || b[nx] != "." || tries >= 4L) break
        tries <- tries + 1L
      }
      if (is.na(nx)) break
    }
    
# Append chosen token to the output.
    out <- c(out, nx)
    if (b[nx] == ".") break
    
# Update the rolling context:
# Append the new token and trim to the last mlag tokens so that the
# context shape matches the training layout of M.
    key <- c(key, nx)
    mlag <- ncol(M) - 1L
    if (length(key) > mlag) key <- tail(key, mlag)
    if (length(out) > 2000L) break # pathological runs guard 
  }
  
  s <- paste(b[out], collapse = " ")
  s <- gsub(" +([,.;!:?])", "\\1", s)
  s <- sub("^([a-z])", "\\U\\1", s, perl = TRUE) # Capitalise the first character for readability.
  s
}
## over

## Test code examples
# Example 1: single seed
set.seed(1)
sentence <- generate_sentence(b, M, M1, seed_word = "romeo", min_len = 6L)
cat(sentence, "\n")

# Example 2: multiple seeds
set.seed(2)
for (sd in c("romeo", "king", "love")) {
  cat(sprintf("seed='%s': %s\n", sd,
      generate_sentence(b, M, M1, seed_word = sd, min_len = 6L)))
}

# Example 3: automatic seed (seed_word = NULL)
set.seed(3)
cat("auto seed:",
    generate_sentence(b, M, M1, seed_word = NULL, min_len = 6L), "\n")


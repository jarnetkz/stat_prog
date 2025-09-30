setwd("/Users/jarnetkz/postgraduate/statistical_prog/stat_prog")
v_ip <- scan("shakespeare.txt",what="character",skip=83,nlines=196043-83,
          fileEncoding="UTF-8")
# v_ip <-a
# v_ip<- scan("shakespeare_mini.txt",what="character",skip=4,nlines=196043-83,
          # fileEncoding="UTF-8")

# Get the index that has "[" , "]"
idx_opnb = grep("[", v_ip, fixed=TRUE) 
idx_clsb = grep("]", v_ip, fixed=TRUE) 

to_remove <- logical(length(v_ip))
# Prepare the index we want to remove
for (i in seq_along(idx_opnb)) {
  start <- idx_opnb[i]
  end <- idx_clsb[idx_clsb > start & idx_clsb <= start + 100]
  
  if (length(end) > 0) {
    end <- end[1]
  } else {
    end <- start
  }
  
  to_remove[start:end] <- TRUE
}

clean_data <- v_ip[!to_remove]
upper_data <- toupper(clean_data)

# filter out UPPERCASE words (except I , A)
filtered_expt_AI = clean_data[(clean_data != toupper(clean_data)) | 
                             (("I" == upper_data) | ("A" == upper_data))]

# remove character - _ out of filtered_expt_AI
filtered_vec = gsub("[_-]", "", filtered_expt_AI)

split_punct <- function(filtered_vec, spcl_char) {
  spcl_pattern = paste0("[", paste(spcl_char, collapse=""), "]")
  i_spcl_char <- grep(spcl_pattern, filtered_vec)
  new_vec <- rep("",length(filtered_vec) + length(i_spcl_char)) # create character empty string
  i_new_char <- i_spcl_char + seq_along(i_spcl_char) # compute new location after splitting
  new_vec[i_new_char] <- substr(filtered_vec[i_spcl_char], nchar(filtered_vec[i_spcl_char]), nchar(filtered_vec[i_spcl_char]))
  new_vec[-i_new_char] <- gsub(spcl_pattern, "", filtered_vec)
  return(new_vec)
}
spcl_char_vec <- c(",", ".", ";", "!", ":", "?")
v_output <- split_punct(filtered_vec, spcl_char_vec)
v_lower <- tolower(v_output)


## Part 5, 6, 7

## 5
b_all <- unique(v_lower)
idx   <- match(v_lower, b_all)
freq  <- tabulate(idx, nbins = length(b_all))
ord   <- order(-freq, seq_along(freq))           
k     <- 1000L
b     <- b_all[ ord[ seq_len(min(k, length(ord))) ] ]

 
.PUNCT <- c(",", ".", ";", "!", ":", "?")
b <- unique(c(b, .PUNCT))

## 6: tokens & context matrix
M1 <- match(v_lower, b)                        
storage.mode(M1) <- "integer"
mlag <- 4L
n <- length(M1); nr <- n - mlag; stopifnot(nr > 0L)

M <- matrix(NA_integer_, nrow = nr, ncol = mlag + 1L)
for (j in 0:mlag) M[, j + 1L] <- M1[(1L + j):(nr + j)]


## 7: next.word (from yiheng)
# key: integer token ids; M: (n-mlag) x (mlag+1) (last col = next token)
# M1: full-text tokens (integers) over same vocab; w: mixture weights
next.word <- function(key, M, M1, w = rep(1, ncol(M) - 1L)) {
  mlag <- ncol(M) - 1L
  key  <- as.integer(key)
  if (length(key) > mlag) key <- tail(key, mlag)
  Lmax <- min(length(key), mlag)

  cand <- integer(0); prob <- numeric(0)

  for (L in Lmax:1L) {
    if (L <= 0L) next
    kL <- tail(key, L)
    mc <- mlag - L + 1L

    block <- M[, mc:mlag, drop = FALSE]            # nrow x L
    if (!is.matrix(block)) block <- cbind(block)
    eq <- block == rep(kL, each = nrow(block))   
    eq[is.na(eq)] <- FALSE                         # NA ≠ anything
    hit <- which(rowSums(eq) == ncol(block))       # rows matching all L cols

    if (length(hit)) {
      uL <- M[hit, mlag + 1L]                      # next-token column
      uL <- uL[!is.na(uL)]
      if (length(uL)) {
        cand <- c(cand, uL)
        prob <- c(prob, rep(w[L] / length(uL), length(uL)))
      }
    }
  }

  # 0-gram fallback
  if (!length(cand)) {
    pool <- as.integer(M1[!is.na(M1)])
    if (!length(pool)) return(NA_integer_)
    tab <- tabulate(pool, nbins = max(M1, na.rm = TRUE))
    return(sample.int(length(tab), 1L, prob = tab))
  }

  prob <- prob / sum(prob)
  sample(cand, 1L, prob = prob)
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




## Abbi's Addition
## May have over commented

b<- unique(v_lower) # Getting unique words in text
c<- match(v_lower, b)
freq<- tabulate(c) # Count how many times unique word appears
pop<- rank(-freq, na.last=TRUE) # Rank words by frequency. Ties have the same rank.
top<- which(pop <= 1000) # Indices of around the top 1000 words
top_1000_words<- v_lower[top] # Get the top actual words from the text



mlag<- 4 # As given in the assignment
m<- match(v_lower,top_1000_words) # Convert the text into token indices, but only for top 1000 words, the rest are NA

n<- length(m) 
M<- matrix(nrow=(n-mlag), ncol=mlag+1) # Generates matrix of required size

# Fill each column with shifted versions of tokens
for (j in 0:mlag) {
  M[, j+1] <- m[(1 + j):(n - mlag + j)]
}

M # Sliding vector

## NOT CHECKED, WIP
next.word<- function(key,M,M1,w=rep(1,ncol(M)-1)) {
  ## Deal with the length of the key, if longer/ shorter
  ## NOT SURE ABOUT THIS AT THE MOMENT
  mlag<- ncol(M) - 1
  if (length(key)>mlag) {
    key_used<- tail(key, mlag)
  } else{
    key_used<- key
  }
  
  mlag_for_key <- length(key_used)
  mc<- mlag - mlag_for_key + 1
  
  # Using hint from the assignment  
  ii <- colSums(!(t(M[,mc:mlag,drop=FALSE])==key_used))
  matching_rows <- which(ii[j] == 0 & is.finite(ii[j]))
  
  if(length(matching_rows)>0) {
    c<- M[matching_rows, mlag + 1]
    next_word<- sample(c, 1, replace=FALSE)
  } else {
    next_word<- sample(M1, 1)
  }
  
  return(next_word)
}

key<- c("From", "fairest", "creatures", ",", "sun")

cat(next.word(key,M,v_lower,w=rep(1,ncol(M)-1)))


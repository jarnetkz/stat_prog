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
  i_new_char <- i_spcl_char+1:length(i_spcl_char) # compute new location after splitting
  new_vec[i_new_char] <- substr(filtered_vec[i_spcl_char], nchar(filtered_vec[i_spcl_char]), nchar(filtered_vec[i_spcl_char]))
  new_vec[-i_new_char] <- gsub(spcl_pattern, "", filtered_vec)
  return(new_vec)
}
spcl_char_vec <- c(",", ".", ";", "!", ":")
v_output <- split_punct(filtered_vec, spcl_char_vec)
v_lower <- tolower(v_output)


## Part 5 and 6, 
## Abbi's Addition
## May have over commented

b<- unique(a) # Getting unique words in text
c<- match(a, b)
freq<- tabulate(c) # Count how many times unique word appears
pop<- rank(-freq, na.last=TRUE) # Rank words by frequency. Ties have the same rank.
top<- which(pop <= 1000) # Indices of around the top 1000 words
top_1000_words<- a[top] # Get the top actual words from the text

mlag<- 4 # As given in the assignment
m<- match(a,top_1000_words) # Convert the text into token indices, but only for top 1000 words, the rest are NA

n<- length(m) 
M<- matrix(nrow=(n-mlag), ncol=mlag+1) # Generates matrix of required size

# Fill each column with shifted versions of tokens
for (j in 0:mlag) {
  M[, j+1] <- m[(1 + j):(n - mlag + j)]
}

M # Sliding vector

## NOT CHECKED
next.word<- function(key,M,M1,w=rep(1,ncol(M)-1)) {
  ## Deal with the length of the key, if longer/ shorter
  ## NOT SURE ABOUT THIS AT THE MOMENT
  if (length(key)>mlag+1) {
    key_used<- tail(key, mlag+1)
  } else{
    key_used<- key
  }
  
  mlag_for_key<- length(key_used)
  
  # Using hint from the assignment  
  ii <- colSums(!(t(M[,mc:mlag,drop=FALSE])==key))
  matching_rows<- which(ii[j]==0 & is.finite(ii))
  
  if(length(matching_rows)>0) {
    c<- M[matching_rows, mlag + 2]
    next_word<- sample(c, 1, replace=FALSE)
  } else {
    next_word<- sample(M1, 1)
  }
  
  return(next_word)
}


setwd("/Users/jarnetkz/postgraduate/statistical_prog/stat_prog")
# a <- scan("shakespeare.txt",what="character",skip=83,nlines=196043-83,
#           fileEncoding="UTF-8")

v_ip<- scan("shakespeare_mini.txt",what="character",skip=4,nlines=196043-83,
          fileEncoding="UTF-8")

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

# define the function to separate the special character
output <- character(0)
split_punct <- function(words, spcl_lst) {
  str_pattern <- paste0("[", paste0(spcl_lst, collapse = ""),"]")
  
  for (i in seq_along(words)) {
    if (grepl(str_pattern, words[i])) {
      spcl_char <- regmatches(words[i], gregexpr(str_pattern, words[i]))
      output <- c(output, gsub(spcl_char, "", words[i]), spcl_char)
    } else {
      output <- c(output, words[i])
    }
  }
  return(unlist(output))
}

v_output <- split_punct(filtered_vec, c(",", ".", ";", "!", ":"))
v_lower <- tolower(v_output)


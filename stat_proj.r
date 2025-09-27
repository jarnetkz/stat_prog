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

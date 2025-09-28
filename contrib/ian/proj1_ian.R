#(3)
setwd("/Users/zhaoyiheng/Desktop/sp-proj1-shakespeare-2025") 
a <- scan("shakespeare.txt",
          what = "character",
          skip = 83,
          nlines = 196043 - 83,
          fileEncoding = "UTF-8")

#(4a) 删除舞台提示 [ ... ],在每个 '[' 之后向前看最多 look_ahead 个词,如果在窗口内找到第一个 ']'，则把从 '[' 到该 ']' 的整段删除。
remove_stage_dirs <- function(a, look_ahead = 100L) {
  n <- length(a)
  keep <- rep(TRUE, n)
  #用 grep 定位所有包含 '[' 的词的位置
  opens <- grep("[", a, fixed = TRUE)
  #遍历每个 '['，在接下来的 look_ahead 个词内用 grep 再找 ']'
  for (i in opens) {
    # 如果这个位置已经被更早的一段覆盖删除，就跳过
    if (!keep[i]) next
    window_end <- min(n, i + look_ahead)
    if (i + 1L > window_end) next
    # 在 a[(i+1):window_end] 这个窗口里找第一个包含 ']' 的词
    rel_close <- integer(0)
    rel_close <- grep("]", a[(i + 1L):window_end], fixed = TRUE)
    if (length(rel_close) > 0L) {
      close_pos <- i + rel_close[1L]  # 还原为全局下标
      keep[i:close_pos] <- FALSE
    }
  }
  # 返回删除后的文本
  a[keep]
}

# To Test, paste the below lines to the Console
a1 <- remove_stage_dirs(a)
head(a1, 100)
#(4b)删掉全大写,且不是I,A或纯数字
drop_caps_nums_and_simplify <- function(a) {
  is_upper <- a == toupper(a)            
  keep_IA  <- a %in% c("I", "A")       
  is_num   <- grepl("^[0-9]+$", a)      
  a <- a[ !( (is_upper & !keep_IA) | is_num ) ]
#(4c)用gsub删引号和连字符
  a <- gsub("\u201C|\u201D|\"", "", a)     
  a <- gsub("-", "", a, fixed = TRUE)      
  a[nchar(a) > 0]
}
#(4d) split_punct，把 , . ; ! : ? 从词里剥离，作为独立 token 放在该词后
split_punct <- function(words, punct = c(",", ".", ";", "!", ":", "?")) {
  out <- vector("list", length(words))
  for (i in seq_along(words)) {
    w <- words[i]; adds <- character(0)
    for (p in punct) {
      k <- nchar(w) - nchar(gsub(p, "", w, fixed = TRUE))  # 出现次数
      if (k > 0L) { adds <- c(adds, rep(p, k)); w <- gsub(p, "", w, fixed = TRUE) }
    }
    out[[i]] <- if (nzchar(w)) c(w, adds) else adds
  }
  unlist(out, use.names = FALSE)
}
#避免重复刷屏
if (!exists("a", inherits = FALSE)) {
  a <- scan("shakespeare.txt", what="character",
            skip = 83, nlines = 196043 - 83, fileEncoding = "UTF-8")
}

#4(d)，4(f)
a <- tolower(
  split_punct(
    drop_caps_nums_and_simplify(
      remove_stage_dirs(a)
    ),
    c(",", ".", ";", "!", ":", "?")
  )
)
# 5(a)
u <- unique(a)

# 5(b)
idx <- match(a, u)

# 5(c)
freq <- tabulate(idx, nbins = length(u))

# 5(d) 取≈1000个最常见词（用 rank）
k <- 1000L
r <- rank(-freq, ties.method = "first")
b <- u[r <= min(k, length(u))]

# 6) 构造“常用词 token 序列”的矩阵
mlag <- 4L   # 可以改成任何合理的整数

# 6(a) 整个文本的 token 向量（不在 b 里的词记为 NA）
M1 <- match(a, b) 

# 6(b) 生成 (n - mlag) x (mlag + 1) 的矩阵 M
n  <- length(M1)
nr <- n - mlag
if (nr <= 0L) stop("mlag 太大了，导致没有行。")

M <- matrix(NA_integer_, nrow = nr, ncol = mlag + 1L)
for (j in 0:mlag) {
  M[, j + 1L] <- M1[(1L + j):(nr + j)]   # 逐列用移位后的 M1 填充
}

# (7) 生成下一个 token
next.word <- function(key, M, M1, w = rep(1, ncol(M) - 1L)) {
  mlag <- ncol(M) - 1L
  key  <- as.integer(key)
  if (length(key) > mlag) key <- tail(key, mlag)       # 只用最后 mlag 个
  Lmax <- min(length(key), mlag)                        # 短 key 用降阶
  
  tok <- integer(0)                                     # 收集候选 token
  pr  <- numeric(0)                                     # 收集对应概率权重
  
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
  
  # 无任何匹配：最朴素回退（在语料中出现过的 token 里平均抽一个）
  if (!length(tok)) {
    pool <- unique(M1[!is.na(M1)])
    return(sample(pool, size = 1L))
  }
  
  # 合并相同 token 的权重并归一化后采样
  mix <- tapply(pr, tok, sum)
  tokens <- as.integer(names(mix))
  probs  <- as.numeric(mix)
  probs  <- probs / sum(probs)
  sample(tokens, size = 1L, prob = probs)
}
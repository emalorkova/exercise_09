# `HMM(N, M, A, B, pi)`
# * `N` set of hidden states
# * `M` set of emitted characters
# * `A` transition probability matrix
# * `B` emission probability matrix
# * `pi` initial probability distribution vector

seq <- "GGCACTGAA"
substr = unlist(strsplit(seq, split = ""))
probability_matrix <- matrix(NA, nrow = 2, ncol = length(substr))

for (i in 1:nchar(seq)) {
  if (i == 1) {
    char <- which(HMM1$M == substr[i])
    pH <- HMM1$B[1,char] + HMM1$pi[1]
    pL <- HMM1$B[2,char] + HMM1$pi[2]
    probability_matrix[1,1] <- pH
    probability_matrix[2,1] <- pL
  } else {
    char <- which(HMM1$M == substr[i])
    pH = HMM1$B[1,char] + max((probability_matrix[1, i-1] + HMM1$A[1,1]), (probability_matrix[2, i-1] + HMM1$A[2,1]))
    pL = HMM1$B[2,char] + max((probability_matrix[1, i-1] + HMM1$A[1,2]), (probability_matrix[2, i-1] + HMM1$A[2,2]))
    probability_matrix[1,i] <- pH
    probability_matrix[2,i] <- pL
  }
}

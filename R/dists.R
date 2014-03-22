#' Compute the Hamming distance
#'
#' b1 and b2 must be integer vectors with 0s and 1s only, or logical
#' vectors.
#'
#' @export
#' 
hamdist <- function(b1, b2) {
    if (length(b1) != length(b2)) {
        stop("Bit vector lengths are different.")
    }
    
    sum(xor(b1, b2))
}

#' Compute approximate cosine distance
#'
#' Use the LSH hashes of two vectors to approximate the cosine
#' distance in the original space.
#'
#' @export
#' 
lshdist <- function(b1, b2) {
    if (length(b1) != length(b2)) {
        stop("Bit vector lengths are different.")
    }
    
    nb <- length(b1)
    cos(hamdist(b1, b2) / nb * pi)
}

#' Compute the cosine distance
#'
#' @export
#' 
cosdist <- function(x1, x2) {
    n1 <- sqrt(sum(x1^2))
    n2 <- sqrt(sum(x2^2))
    d <- as.numeric(x1 %*% x2) / n1 / n2
    d
}

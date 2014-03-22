#' Construct a new LSH object
#'
#' @param d The dimension of the input vectors.
#' @param b The number of bits to use in the hash.
#' @param seed The seed to use to generate the random transformation
#' matrix T.
#'
#' @export
#' 
lsh <- function(d, b, seed) {
    orig_seed <- .Random.seed
    obj <- structure(list(), class = "lsh")
    set.seed(seed)
    obj$d <- d
    obj$b <- b
    obj$seed <- seed
    obj$T <- matrix(rnorm(b * d), b, d)
    .Random.seed <- orig_seed
    obj
}

#' @export
print.lsh <- function(hash_fn) {
    s <- sprintf("lsh(d=%d, b=%d, seed=%d)\n",
                 hash_fn$d, hash_fn$b, hash_fn$seed)
    cat(s)
    invisible(hash_fn)
}

#' Hash input vectors using an LSH object
#'
#' @export
#' 
hash <- function(...) UseMethod("hash")

#' @export
hash.numeric <- function(x, lsh_fn) {
    if (length(x) != lsh_fn$d) {
        stop("Dimension of x does not match LSH input dimension.")
    }

    hx <- lsh_fn$T %*% x
    hx <- as.integer(hx >= 0)
    hx
}

#' @export
hash.matrix <- function(X, lsh_fn) {
    if (nrow(X) != lsh_fn$d) {
        stop("Number of rows in X does not match LSH input dimension.")
    }

    hX <- lsh_fn$T %*% X
    hXdim <- dim(hX)
    hX <- matrix(as.integer(hX >= 0), hXdim[1], hXdim[2])
    hX
}


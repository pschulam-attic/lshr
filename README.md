lshr
====

Locality sensitive hashing in R.

Here's a simple example of hashing some random feature vectors.

```r
library(lshr)

X <- matrix(rnorm(40 * 1000), 40, 1000)  # 1000 40-dimensional feature vectors

hash_fn <- lsh(40, 15, 1)  # Create an LSH hash from 40 dimensions to 15 bits using 1 as a random seed.
hX <- hash(X, hash_fn)     # Hash each of columns.
```

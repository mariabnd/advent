#111111 meets these criteria (double 11, never decreases).
#223450 does not meet these criteria (decreasing pair of digits 50).
#123789 does not meet these criteria (no double).

input <- 136760 : 595730

# Extract digits
digits <- do.call(rbind, strsplit(as.character(input), ""))

# Remove first decreasing pairs of digits
for (i in rev(2 : 6)){
  temp <- digits[, i] < digits[, i - 1]
  digits <- digits[- which(temp), ]
}

# Locations where double digits are possible
opts <- combn(1 : 6, 2)
opts <- opts[, which(opts[2, ] == opts[1, ] + 1)]

index <- NA
for (i in 1 : dim(opts)[2]){
     index <- c(index, which(digits[, i] == digits[, i + 1]))
}
# Remove NA again
index <- index[- 1]
index <- unique(sort(index))

# Number of passwords meeting the criteria
length(index)

# Bonus - Which ones
output <- digits[index, ]
output <- sapply(1 : dim(output)[1], 
                 function(x) paste(output[x, ], collapse = ""))
output <- as.numeric(output)

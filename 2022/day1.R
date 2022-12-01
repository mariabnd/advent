#Assumes inpt is the string
#Inserted as
inpt <- "
<input goes here>
"

grab <- strsplit(x = inpt, split = "\\n")[[1]]
idx <- which(grab == "")

i <- 1
vals <- c()
while (i <= length(idx)){
 vals[i] <- sum(as.numeric(grab[c(1, (idx + 1)[-length(idx)])[i] : idx[i]]), na.rm = TRUE)
  i <- i + 1
}

# Most calories
max(vals)

# Top three
sum(sort(vals, decreasing = TRUE)[1 : 3])

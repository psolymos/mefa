`binmat` <-
function(matrix){
mat <- matrix(data = as.numeric(matrix > 0), nrow = nrow(matrix), ncol = ncol(matrix))
rownames(mat) <- rownames(matrix)
colnames(mat) <- colnames(matrix)
return(mat)
}


`fill.count` <-
function(table){
out <- table

for(col in 1:ncol(table))
	{
		for(row in 1:nrow(table)){
			if(is.na(table[row, col])) {
				row2 <- row - 1
				out[row, col] <- out[row2, col]}
		}
	}

return(out)
}


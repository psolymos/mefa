`ivgen` <-
function(x, cl, k){

typol <- cutree(cl, k = c(1:k))

write.table(typol, file="iv-typology.out", quote=FALSE, sep=" ", dec=".", row.names=FALSE, col.names=FALSE)
write.table(t(x), file="iv-data.out", quote=FALSE, sep=" ", dec=".", row.names=FALSE, col.names=FALSE)
write.table(t(x)[,0], file="iv-species.out", quote=FALSE, sep=" ", dec=".", row.names=TRUE, col.names=FALSE)

cat("The 3 output files were successfully written into your working directoty.\n",
    "Type these when running 'IndVal2.exe':\n",
    "  - name of the species-sample file:\t\tiv-data.out\n",
    "  - number of species in the dataset:\t\t",ncol(x),"\n",
    "  - number of samples in the dataset:\t\t",nrow(x),"\n",
    "  - name of the typology file:\t\t\tiv-typology.out\n",
    "  - number of partitions in the dataset:\t",k,"\n",
    "  - name of the species name file:\t\tiv-species.out\n\n",sep="")
}

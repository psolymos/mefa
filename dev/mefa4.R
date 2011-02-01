MEFA TODO LIST -- also a memo

*** mefa ***
- fulldrop issues
- xtab representation (sum/mean)

*** mefa4 ***
< methods to add to >
OK - dim
OK - dimnames, dimnames<-
OK - xtab, xtab<-
OK - samp, samp<-
OK - taxa, taxa<-
OK - [, [<-
OK - coercion methods between S3 and S4 classes (went to mefa)
OK - coercion between various classes and Mefa
OK - aggregate -- no sparse implementation, use aggregate(as.mefa), ...)
     but that is sloooow. An implementation is ready in my Dropbox folder
     it is implemented as groupSums and groupMeans methods
OK - show
OK - test directory with example and performance comparison code
OK - Xtab methods for [ and [<- (different siglists?), now returns dgCMatrix
     Xtab class removed, use dgCMatrix instead
OK - join 2 Xtab, Mefa objects, inner/outer/left/right join operations
     needs careful planning, but should be pretty straightforward
     left join version is implemented as mbind in mefa4
OK - add a vignette on performance comparisons, new classes and methods
OK - write documentation (Rd)
OK - sparseMatrixList virtual class added
- methods for sparseMatrixList [, [<-
- how to incorporate sparseMatrixList into Mefa?
     define new class, that will need [, [<-
- compatibility with 'mefa' class: xtab should be the segm list, segm should be a MefaDataFrame
- rename slot?
- print, summary (not sure what to do with these)
- plot, image (see image plot for Matrix classes) - it is not urgent, image(as.mefa(x))
? set a class union for XtabMatrix and Mefa -- currently no need for it
? which operatons should affect @call? -- update all the time, or remove @call
? is there a need for call and update? -- I think no


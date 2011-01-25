MEFA TODO LIST -- also a memo

*** mefa ***
- fulldrop issues
- xtab representation (sum/mean)

*** mefa4 ***
< methods to add to >
OK - dim
OK - dimnames
OK - xtab, xtab<-
OK - samp, samp<-
OK - taxa, taxa<-
OK - [<-
OK - coercion methods between S3 and S4 classes (went to mefa)
OK - coercion between Xtab and Mefa
OK - aggregate -- no sparse implementation, use aggregate(as.mefa), ...)
     but that is sloooow. An implementation is ready in my Dropbox folder
     it is implemented as groupSums and groupMeans methods
OK - show
OK - test directory with example and performance comparison code

- Xtab methods for [ and [<- (different siglists?), now returns dgCMatrix

- print, summary (not sure what to do with these)
- plot, image (see image plot for Matrix classes) - it is not urgent, image(as.mefa(x))
? set a class union for XtabMatrix and Mefa -- currently no need for it
? which operatons should affect @call? -- update all the time, or remove @call
? is there a need for call and update? -- I think no

- join 2 Xtab, Mefa objects, inner/outer/left/right join operations
     needs careful planning, but should be pretty straightforward

- add a vignette on performance comparisons, new classes and methods

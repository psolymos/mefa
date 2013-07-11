:: cleanup
::rmdir c:\svn\mefa\dev\tests /s /q
:: export pkg dir
mkdir c:\svn\mefa\dev\tests\
svn export c:\svn\mefa\pkg\mefa c:\svn\mefa\dev\tests\mefa
::svn export c:\svn\mefa\pkg\mefa4 c:\svn\mefa\dev\tests\mefa4
:: update R packages
R CMD BATCH --vanilla c:\svn\dcr\dev\pkg_check\updates.R c:\svn\mefa\dev\tests\updates.Rout
:: change dir to test
cd c:\svn\mefa\dev\tests\
:: export pkg dirs from svn
R CMD build mefa --compact-vignettes
::R CMD build mefa4 --compact-vignettes
:: check pkgs
R CMD check mefa_*.tar.gz --as-cran
::R CMD check mefa4_*.tar.gz --as-cran



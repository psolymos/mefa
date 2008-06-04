.First.lib <- function(lib, pkg)  {
    library.dynam("mefa", pkg, lib)
    packageStartupMessage("This is mefa ",
                          utils::packageDescription("vegan", field="Version"),
                          appendLF = TRUE)
}

library(testthat)

# When testing locally with devtools::test(), AutoRel is already loaded.
# When testing via R CMD check, test_check("AutoRel") handles it.

test_check("AutoRel")

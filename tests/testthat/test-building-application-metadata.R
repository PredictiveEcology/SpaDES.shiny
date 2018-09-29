context("Building application metadata object")

test_that("shinyModule returns a metadata object that is just the name of the module.", {
  aModuleName <- paste(sample(LETTERS, 20), collapse = "")
  expect_equal(aModuleName, shinyModule(aModuleName))
})

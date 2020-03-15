
test_that("as_normalized() function works", {

  ## normal
  df1 <- as_normalized(df, formula)
  col_names1 <- c("Sepal.Width", "Petal.Length", "Petal.Width", "Species",
                  "is_setosa", "Species_ord", "Sepal.Length")
  expect_named(df1, col_names1)
  expect_equal(mean(df1$Sepal.Width), 0)
  expect_equal(mean(df1$Petal.Length), 0)
  expect_equal(mean(df1$Petal.Width), 0)

  expect_equal(sd(df1$Sepal.Width), 1)
  expect_equal(sd(df1$Petal.Length), 1)
  expect_equal(sd(df1$Petal.Width), 1)

})

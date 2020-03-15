
test_that("as_dummy() function works", {

  ## normal
  df1 <- as_dummy(df, formula)
  col_names1 <- c("Sepal.Width", "Petal.Length", "Petal.Width",
                  "is_setosa", "Species_ord", "Sepal.Length",
                  "Species_versicolor", "Species_virginica")
  expect_named(df1, col_names1)
  expect_is(df1$is_setosa, "logical")
  expect_is(df1$Species_ord, "ordered")
  expect_is(df1$Species_versicolor, "numeric")
  expect_is(df1$Species_virginica, "numeric")

  ## one_hot
  df2 <- as_dummy(df, formula, one_hot = TRUE)
  col_names2 <- c("Sepal.Width", "Petal.Length", "Petal.Width",
                  "is_setosa", "Species_ord", "Sepal.Length",
                  "Species_setosa", "Species_versicolor", "Species_virginica")
  expect_named(df2, col_names2)
  expect_is(df2$is_setosa, "logical")
  expect_is(df2$Species_ord, "ordered")
  expect_is(df2$Species_setosa, "numeric")
  expect_is(df2$Species_versicolor, "numeric")
  expect_is(df2$Species_virginica, "numeric")

  ## ordered
  df3 <- as_dummy(df, formula, ordered = TRUE)
  col_names3 <- c("Sepal.Width", "Petal.Length", "Petal.Width",
                  "is_setosa", "Sepal.Length",
                  "Species_versicolor", "Species_virginica",
                  "Species_ord_1", "Species_ord_2")
  expect_named(df3, col_names3)
  expect_is(df3$is_setosa, "logical")
  expect_is(df3$Species_ord_1, "numeric")
  expect_is(df3$Species_ord_2, "numeric")
  expect_is(df3$Species_versicolor, "numeric")
  expect_is(df3$Species_virginica, "numeric")

  ## logical
  df4 <- as_dummy(df, formula, logical = TRUE)
  col_names4 <- c("Sepal.Width", "Petal.Length", "Petal.Width",
                  "Species_ord", "Sepal.Length",
                  "Species_versicolor", "Species_virginica",
                  "is_setosa_TRUE.")
  expect_named(df4, col_names4)
  expect_is(df4$Species_ord, "ordered")
  expect_is(df4$is_setosa_TRUE., "numeric")
  expect_is(df4$Species_versicolor, "numeric")
  expect_is(df4$Species_virginica, "numeric")

  df5 <- as_dummy(df, formula, one_hot = TRUE, ordered = TRUE, logical = TRUE)
  col_names5 <- c("Sepal.Width", "Petal.Length", "Petal.Width", "Sepal.Length",
                  "Species_setosa", "Species_versicolor", "Species_virginica",
                  "is_setosa_FALSE.", "is_setosa_TRUE.",
                  "Species_ord_1", "Species_ord_2", "Species_ord_3")
  expect_named(df5, col_names5)
  expect_is(df5$is_setosa_TRUE., "numeric")
  expect_is(df5$is_setosa_FALSE., "numeric")
  expect_is(df5$Species_ord_1, "numeric")
  expect_is(df5$Species_ord_2, "numeric")
  expect_is(df5$Species_ord_3, "numeric")
  expect_is(df5$Species_setosa, "numeric")
  expect_is(df5$Species_versicolor, "numeric")
  expect_is(df5$Species_virginica, "numeric")

})

test_that("as_ordinalscore() function works", {

  ## normal
  df1 <- as_ordinalscore(df, formula)
  col_names1 <- c("Sepal.Width", "Petal.Length", "Petal.Width", "Species",
                  "is_setosa", "Species_ord", "Sepal.Length")
  expect_named(df1, col_names1)
  expect_is(df1$is_setosa, "logical")
  expect_is(df1$Species_ord, "numeric")
  expect_is(df1$Species, "factor")

})


df <- iris %>%
  dplyr::mutate(is_setosa = Species == "setosa") %>%
  dplyr::mutate(Species_ord = as.ordered(Species))

formula <- Sepal.Length ~ .

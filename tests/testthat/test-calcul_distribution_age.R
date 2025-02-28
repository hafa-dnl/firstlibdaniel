test_that("calcul_distribution_age retourne une distribution correcte", {
  df_test <- data.frame(Date.de.naissance = as.Date(c("2000-01-01", "1990-06-15", "1980-12-30")))
  resultat <- calcul_distribution_age(df_test)
  expect_length(resultat, 3)  # Min, Q1, Médiane, Q3, Max
})

test_that("calcul_distribution_age retourne une erreur si la colonne n'existe pas", {
  df_test <- data.frame(Age = c(25, 40, 60))
  expect_error(calcul_distribution_age(df_test))
})

test_that("plot_code_professions fonctionne correctement", {
  # Test 1 : Vérifier avec un jeu de données d'exemple
  df_test <- data.frame(Code_profession = c("A", "B", "A"))
  expect_s3_class(plot_code_professions(df_test), "gg")

  # Test 2 : Vérifier pour un jeu de données vide
  df_empty <- data.frame(Code_profession = character(0))
  expect_s3_class(plot_code_professions(df_empty), "gg")
})


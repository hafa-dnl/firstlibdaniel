test_that("plot_code_professions fonctionne correctement", {
  # Test 1 : Vérifier avec un jeu de données d'exemple
  df_test <- data.frame(Code.de.la.categorie.socio.professionnelle = c("A", "B", "A"))
  expect_s3_class(plot_code_professions(df_test), "gg")

  # Test 2 : Vérifier pour un jeu de données vide
  df_empty <- data.frame(Code.de.la.categorie.socio.professionnelle = character(0))
  expect_s3_class(plot_code_professions(df_empty), "gg")
})

test_that("creer_commune assigne correctement la classe Commune", {
  result <- creer_commune(df_Nantes)
  expect_s3_class(result, "Commune")
})

test_that("creer_commune échoue si plusieurs communes sont présentes", {
  df_multi_commune <- df_Gers
  expect_error(creer_commune(df_multi_commune), "L'objet doit contenir 1 commune unique")
})

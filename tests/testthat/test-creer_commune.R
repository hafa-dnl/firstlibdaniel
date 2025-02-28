test_that("creer_commune fonctionne avec une seule commune", {
  df_test <- elus_conseillers_municipaux_cm %>%
    dplyr::filter(Libelle.de.la.commune == "Paris")
  result <- creer_commune(df_test)
  expect_true(inherits(result, "commune"))
})

test_that("creer_commune renvoie une erreur si plusieurs communes", {
  expect_error(creer_commune(elus_conseillers_municipaux_cm),
               "Le dataframe contient plusieurs communes. Fournissez les donnees d'une seule commune.")
})

test_that("summary.departement affiche un résumé correct", {
  df_test <- data.frame(
    Libelle_departement = rep("Seine-Saint-Denis", 5),
    Libelle_commune = rep("Paris", 5),
    Nom_elu = c("Elu1", "Elu2", "Elu3", "Elu4", "Elu5"),
    Age = c(40, 45, 50, 60, 35),
    Date_de_naissance = as.Date(c("1980-01-01", "1990-05-10", "2000-07-20", "1975-03-30", "1985-12-15"))
  )

  # Vérification que la fonction ne génère aucune sortie
  expect_silent(summary.departement(df_test))
})


test_that("summary.departement retourne une erreur si plusieurs départements sont présents", {
  df_test <- data.frame(
    Libelle_departement = c("Seine-Saint-Denis", "Val-de-Marne"),
    Libelle_commune = c("Paris", "Créteil"),
    Date_de_naissance = as.Date(c("1980-01-01", "1990-05-10"))
  )
  expect_error(summary.departement(df_test))
})

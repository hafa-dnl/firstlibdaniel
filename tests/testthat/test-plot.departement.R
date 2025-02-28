test_that("plot.departement crée un graphique valide", {
  # Création d'un dataframe de test avec les colonnes nécessaires
  df_test <- data.frame(
    Code.du.departement = rep("75", 7),  # Code du département (par exemple Paris)
    Libelle.du.departement = rep("Île-de-France", 7),
    Libelle.de.la.commune = c("Paris", "Lyon", "Marseille", "Nice", "Toulouse", "Bordeaux", "Nantes"),
    Code.de.la.categorie.socio.professionnelle = c("Informatique", "Santé", "Informatique", "Commerce", "Informatique", "Santé", "Commerce"),
    stringsAsFactors = FALSE
  )

  # Appliquer la classe 'departement' au dataframe de test
  class(df_test) <- c("departement", class(df_test))

  # Créer un plot et vérifier qu'il est généré sans erreur
  expect_silent({
    plot <- plot.departement(df_test)
  })

  # Vérifier si le titre contient bien le nom du département
  expect_true(grepl("Île-de-France", plot$labels$title))

  # Vérifier qu'il y a des éléments dans le graphique
  expect_true(length(plot$layers) > 0)

  # Vérifier que le graphique contient bien une couche 'geom_bar'
  expect_true(any(sapply(plot$layers, function(layer) inherits(layer$geom, "GeomBar"))))
})


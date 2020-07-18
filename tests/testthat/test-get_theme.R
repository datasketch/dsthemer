test_that("Get theme", {

  theme_list()


  org_theme_list()


  org <- "datasketch"
  theme_get("datasketch")
  theme_list("datasketch")

  theme_get("poder")
  theme_get("poder", theme = "dark", palette = "divergent")
  theme_palettes("poder", "dark")
  theme_palette("poder", theme = "light", palette = "main")

  theme_get("dialogos")
  theme_get("dialogos", theme = "dark", palette = "divergent")

  th <- theme_get("dialogos", theme = "light")
  expect_true(file.exists(th$logo))
  th <- theme_get("dialogos", theme = "dark")
  expect_true(grepl("dialogos/dark.png$",th$logo))

  theme_palettes("dialogos", theme = "light")
  theme_palettes("dialogos", theme = "light", type = "Categorical")
  theme_palettes("dialogos", theme = "light", type = "Sequential")
  theme_palettes("dialogos", theme = "light", type = "Divergent")

  theme_palette("dialogos", theme = "light", palette = "main")



})

test_that("Fonts", {

  # TODO
  # Test all fonts for all themes are available locally

})

test_that("App works", {

  # TODO
  # Test all parmesan inputs are theme inputs in dsvizopts

})

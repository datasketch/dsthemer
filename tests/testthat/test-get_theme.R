test_that("Get theme", {

  dsthemer_list()

  org <- "datasketch"
  dsthemer_get("datasketch")
  dsthemer_list("datasketch")

  dsthemer_get("poder")
  dsthemer_get("poder", theme = "dark", palette = "divergent")
  dsthemer_palettes("poder", "dark")
  dsthemer_palette("poder", theme = "light", palette = "main")

  dsthemer_get("dialogos")
  dsthemer_get("dialogos", theme = "dark", palette = "divergent")

  th <- dsthemer_get("dialogos", theme = "light")
  expect_true(file.exists(th$logo))
  th <- dsthemer_get("dialogos", theme = "dark")
  expect_true(grepl("dialogos/dark.png$",th$logo))

  dsthemer_palettes("dialogos", theme = "light")
  dsthemer_palettes("dialogos", theme = "light", type = "Categorical")
  dsthemer_palettes("dialogos", theme = "light", type = "Sequential")
  dsthemer_palettes("dialogos", theme = "light", type = "Divergent")

  dsthemer_palette("dialogos", theme = "light", palette = "main")



})

test_that("Get theme", {

  library(parmesan)
  th <- dsthemer_get("datasketch", "light")
  parmesan <- parmesan::parmesan_load(system.file("app/parmesan",
                          package = "dsthemer"))
  parmesan_pal_colors <- parmesan$palette$inputs[[1]]$input_params$colors
  th_colors <- th$palette_colors
  presets <- dsthemer_presets(th)
  parmesan <- parmesan::parmesan_load(path = system.file("app/parmesan",
                                                  package = "dsthemer"),
                                      presets = presets)
  parmesan_pal_colors <- parmesan$palette$inputs[[1]]$input_params$colors
  expect_equal(parmesan_pal_colors, th_colors)

})

test_that("Fonts", {

  # TODO
  # Test all fonts for all themes are available locally

})

test_that("App works", {

  # TODO
  # Test all parmesan inputs are theme inputs in dsvizopts

})

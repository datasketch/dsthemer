test_that("Get theme", {

  org <- "datasketch"
  get_theme("datasketch")


  get_theme("poder")
  get_theme("poder", theme = "dark", palette = "divergent")
  get_org_palettes("poder", "dark")
  get_org_palette("poder","light","main")

})

test_that("Fonts", {

  # TODO
  # Test all fonts for all themes are available locally

})

test_that("App works", {

  # TODO
  # Test all parmesan inputs are theme inputs in dsvizopts

})

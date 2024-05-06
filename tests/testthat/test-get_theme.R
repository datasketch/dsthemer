test_that("call theme", {


  org <- "datasketch"
  load_dsthemer_json("datasketch")

  load_dsthemer_json("elconfidencial")
  dsthemer_palette("elconfidencial", "dark")
  dsthemer_palette("elconfidencial", theme = "light", palette = "divergening")


})


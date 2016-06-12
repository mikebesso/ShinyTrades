




test_that(
  "Scrapes", {

    expect_that(
      Scraper$InitializeAll(),
      equals(TRUE)
    )

  }
)

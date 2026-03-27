context('Internal validity of regex')

iso3c_of <- function(name) countrycode(name, 'country.name', 'iso3c')

test_that('all country names with iso3c codes are matched exactly once', {
    name <- subset(codelist, !is.na(iso3c))$country.name.en
    iso3c_from_name <- countrycode(name, 'country.name', 'iso3c', warn = TRUE)
    expect_warning(iso3c_from_name, NA)
})

test_that('iso3c-to-country.name-to-iso3c is internally consistent', {
    for(iso3c_original in codelist$iso3c){
        if(!is.na(iso3c_original)){
            name <- countrycode(iso3c_original, 'iso3c', 'country.name')
            iso3c_result <- countrycode(name, 'country.name', 'iso3c')
            expect_equal(iso3c_result, iso3c_original)
        }
    }
})

test_that("Italian regex vs. CLDR", {
    x <- countrycode(codelist$cldr.name.it, "country.name.it", "cldr.name.it")
    expect_equal(x, codelist$cldr.name.it)
})

test_that("Spanish regex vs. CLDR", {
  x <- countrycode(codelist$cldr.name.es, "country.name.es", "cldr.name.es")
  expect_equal(x, codelist$cldr.name.es)
})

test_that("German regex vs. CLDR", {
    x <- countrycode(codelist$cldr.name.de, "country.name.de", "cldr.name.de")
    expect_equal(x, codelist$cldr.name.de)
})

test_that("French regex vs. CLDR", {
    x <- countrycode(codelist$cldr.name.fr, "country.name.fr", "cldr.name.fr")
    expect_equal(x, codelist$cldr.name.fr)
})

test_that("Spanish regex vs. CLDR 419", {
  x <- countrycode(codelist$cldr.name.es_419, "country.name.es", "cldr.name.es_419")
  expect_equal(x, codelist$cldr.name.es_419)
})

test_that("Spanish regex vs. CLDR short", {
  x <- countrycode(codelist$cldr.short.es, "country.name.es", "cldr.short.es")
  expect_equal(x, codelist$cldr.short.es)
})

test_that("Spanish regex vs. CLDR variant", {
  x <- countrycode(codelist$cldr.variant.es, "country.name.es", "cldr.variant.es")
  expect_equal(x, codelist$cldr.variant.es)
})
test_that("Spanish regex vs. UN", {
  x <- countrycode(codelist$un.name.es, "country.name.es", "un.name.es")
  expect_equal(x, codelist$un.name.es)
})

test_that("English regex vs. CLDR short", {
  x <- countrycode(codelist$cldr.short.en, "country.name.en", "cldr.short.en")
  expect_equal(x, codelist$cldr.short.en)
})

test_that("English regex vs. CLDR variant", {
  x <- countrycode(codelist$cldr.variant.en, "country.name.en", "cldr.variant.en")
  expect_equal(x, codelist$cldr.variant.en)
})

test_that("English regex vs. UN", {
  x <- countrycode(codelist$un.name.en, "country.name.en", "un.name.en")
  expect_equal(x, codelist$un.name.en)
})

test_that("French regex vs. CLDR short", {
  x <- countrycode(codelist$cldr.short.fr, "country.name.fr", "cldr.short.fr")
  expect_equal(x, codelist$cldr.short.fr)
})

test_that("French regex vs. CLDR variant", {
  x <- countrycode(codelist$cldr.variant.fr, "country.name.fr", "cldr.variant.fr")
  expect_equal(x, codelist$cldr.variant.fr)
})

test_that("French regex vs. UN", {
  x <- countrycode(codelist$un.name.fr, "country.name.fr", "un.name.fr")
  expect_equal(x, codelist$un.name.fr)
})

test_that("Italian regex vs. CLDR short", {
  x <- countrycode(codelist$cldr.short.it, "country.name.it", "cldr.short.it")
  expect_equal(x, codelist$cldr.short.it)
})

test_that("Italian regex vs. CLDR variant", {
  x <- countrycode(codelist$cldr.variant.it, "country.name.it", "cldr.variant.it")
  expect_equal(x, codelist$cldr.variant.it)
})

test_that("German regex vs. CLDR short", {
  x <- countrycode(codelist$cldr.short.de, "country.name.de", "cldr.short.de")
  expect_equal(x, codelist$cldr.short.de)
})

test_that("German regex vs. CLDR variant", {
  x <- countrycode(codelist$cldr.variant.de, "country.name.de", "cldr.variant.de")
  expect_equal(x, codelist$cldr.variant.de)
})


cs <- countrycode::codelist
pan <- countrycode::codelist_panel

dest <- c('year', 'ar5', 'continent', 'eu28', 'eurocontrol_pru',
          'eurocontrol_statfor', 'icao', 'icao.region', 'region', 'region23')

##############
#  codelist  #
##############

context('codelist')

# class
test_that('codelist is a data.frame', {
    expect_true(inherits(cs, 'data.frame'))
})

# dimensions
test_that('codelist has (roughly) correct dimensions', {
    expect_gt(nrow(cs), 250)
    expect_lt(nrow(cs), 300)
    expect_gt(ncol(cs), 720)
    expect_lt(ncol(cs), 740)
})

# columns
cols <- c('country.name.en.regex', 'country.name.en', 'iso3c', 'cowc', 'p4c', 'vdem')
for (i in cols) {
    test_that(paste('codelist includes', i), {
        expect_true(i %in% colnames(cs))
    })
}

# missing
test_that('codelist missing values', {
    expect_false(any(is.na(cs$country.name.en.regex)))
    expect_false(any(is.na(cs$country.name.en)))
    expect_lt(mean(is.na(cs$iso3c)), .2)
    expect_lt(mean(is.na(cs$cowc)), .3)
    for (i in colnames(cs)) {
        expect_false(all(is.na(cs[[i]])))
    }
})

# duplicate
for (i in colnames(cs)) {
    if (!i %in% dest) {
        test_that(paste0('codelist$', i, ' has no duplicates'), {
            expect_equal(anyDuplicated(na.omit(cs[[i]])), 0)
         })
    }
}


###########
#  panel  #
###########

context('codelist_panel')

# class
test_that('codelist is a data.frame', {
    expect_true(inherits(pan, 'data.frame'))
})

# dimensions
test_that('codelist has (roughly) correct dimensions', {
    expect_gt(nrow(pan), 25000)
    expect_lt(nrow(pan), 30000)
    expect_gt(ncol(pan), 35)
    expect_lt(ncol(pan), 45)
})

# columns
cols <- c('country.name.en.regex', 'country.name.en', 'year', 'iso3c', 'cowc', 'p4c', 'vdem')
for (i in cols) {
    test_that(paste('codelist includes', i), {
        expect_true(i %in% colnames(pan))
    })
}

# missing
test_that('codelist missing values', {
    expect_false(any(is.na(pan$country.name.en.regex)))
    expect_false(any(is.na(pan$country.name.en)))
    expect_false(any(is.na(pan$year)))
    expect_lt(mean(is.na(pan$iso3c)), .1)
    expect_lt(mean(is.na(pan$cowc)), .45)
    for (i in colnames(pan)) {
        expect_false(all(is.na(pan[[i]])))
    }
})

# duplicate
for (i in colnames(pan)) {
    if (!i %in% dest) {
        test_that(paste0('codelist$', i, ' has no duplicates'), {
            idx <- pan[, c(i, 'year')]
            idx <- na.omit(idx)
            idx <- paste(idx[[1]], idx[[2]])
            expect_equal(anyDuplicated(idx), 0)
         })
    }
}

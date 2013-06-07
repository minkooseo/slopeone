source('../slopeone.R')

context('slopeone')

test_that('empty rating', {
  model <- build_slopeone(data.frame())
  expect_equal(NROW(model), 0)
})

test_that('duplicate item ratins', {
  model <- build_slopeone(
    data.table(user_id=c('u1', 'u1', 'u1'),
               item_id=c('i1', 'i2', 'i1'),
               rating=c(3, 4, 4)))
  expected_model <- data.frame(
    item_id1=c('i1', 'i2'),
    item_id2=c('i2', 'i1'),
    b=c(.5, -.5),
    support=c(1, 1),
    stringsAsFactors=FALSE)
  expect_that(model, is_equivalent_to(expected_model))
})

test_that('multiple users', {
  model <- build_slopeone(
    data.table(user_id=c('u1', 'u1', 'u1', 'u2', 'u2'),
               item_id=c('i1', 'i2', 'i3', 'i1', 'i2'),
               rating=c(3, 4, 5, 4, 5)))
  expected_model <- data.frame(
    item_id1=c('i1', 'i1', 'i2', 'i2', 'i3', 'i3'),
    item_id2=c('i2', 'i3', 'i1', 'i3', 'i1', 'i2'),
    b=c(1, 2, -1, 1, -2, -1),
    support=c(2, 1, 2, 1, 1, 1),
    stringsAsFactors=FALSE)
  expect_that(model, is_equivalent_to(expected_model))
})

test_that('predict_slopeone_for_user', {
  model <- data.table(data.frame(
    item_id1=c('i1', 'i1', 'i2', 'i2', 'i3', 'i3'),
    item_id2=c('i2', 'i3', 'i1', 'i3', 'i1', 'i2'),
    b=c(1, 2, -1, 1, -2, -1),
    support=c(3, 1, 2, 1, 3, 1),
    stringsAsFactors=FALSE))
  setkey(model, item_id1, item_id2)
  expect_equal(
      predict_slopeone_for_user(model, 'i2',
                                data.table(item_id=c('i1'), rating=c(4))),
      5)
  expect_equal(
      predict_slopeone_for_user(model, 'i3',
                                data.table(item_id=c('i1'), rating=c(4))),
      6)
  expect_equal(
      predict_slopeone_for_user(model, 'i2',
                                data.table(item_id=c('i1', 'i3'),
                                           rating=c(4, 2))),
      4)  # ((4+1) * 3 + (2-1) * 1) / (3+1)
  expect_equal(
    predict_slopeone_for_user(model, 'i1',
                              data.table(item_id=c('i2', 'i3'),
                                         rating=c(4, 3))),
    1.8)  # ((4-1) * 2 + (3-2) * 3) / (2+3)
  expect_equal(
      predict_slopeone_for_user(model, 'i999',
                                data.table(item_id=c('i1', 'i3'),
                                           rating=c(4, 2))),
      NA)
  expect_warning(
    (first_rating <- predict_slopeone_for_user(model, 'i3',
                              data.table(item_id=c('i3', 'i3'),
                                         rating=c(4, 2)))),
    c('i3  is already rated by user, but there are multiple ratings.'))
  # First rating take precedence
  expect_equal(first_rating, 4)
})


test_that('predict_slopeone', {
  ratings <- data.table(
    user_id=c('u1', 'u1', 'u2', 'u2', 'u3', 'u3', 'u4', 'u4'),
    item_id=c('i1', 'i2', 'i1', 'i3', 'i1', 'i3', 'i2', 'i3'),
    rating=c( 5,    2,    3,    1,    5,    1,    3,    2))
  model <- build_slopeone(ratings)

  # Previously rated item.
  targets <- data.table(user_id=c('u1'), item_id=c('i1'))
  expected_ratings <- targets
  expected_ratings$predicted_rating <- c(5)
  expect_that(predict_slopeone(model, targets, ratings),
              is_equivalent_to(expected_ratings))
  
  # Item that doesn't exist.
  targets <- data.table(user_id=c('u2'), item_id=c('i999'))
  expected_ratings <- targets
  expected_ratings$predicted_rating <- c(NA)
  expect_that(predict_slopeone(model, targets, ratings),
              is_equivalent_to(expected_ratings))
  
  # Item that hasn't been rated.  
  targets <- data.table(user_id=c('u1', 'u2'), item_id=c('i3', 'i2'))
  expected_ratings <- targets
  expected_ratings$predicted_rating <- c(
    ((5-3)*2 + (2-1)*1)/3,
    ((3-3)*1 + (1+1)*1)/2)
  expect_that(predict_slopeone(model, targets, ratings),
              is_equivalent_to(expected_ratings))
})

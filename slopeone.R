# Implementation of slope one algorithm described at
# http://en.wikipedia.org/wiki/Slope_One

require(data.table)
require(foreach)
require(plyr)
require(reshape2)

# Build slope one model.
#
# Params:
#   ratings: A data table containing (user_id, item_id, rating).
#   ...: Extra parameters for ddply.
#
# Returns:
#   A data table of (item_id1, item_id2, b, support) where b represents the 
#   average rating difference of 'item 2 rating' - 'item 1 rating'. support
#   represents number of ratings used to compute b.
build_slopeone <- function(ratings, ...) {
  if (NROW(ratings) == 0) {
    return(data.table(data.frame(item_id1=c(), item_id2=c(), b=c(), support=c())))
  }
  # Generates all pairs of (item id 1, item id 2, diff) if both of item 1 and
  # 2 is rated by the same user.
  score_diff_per_user <- dlply(ratings, .(user_id), function(rows) {
    if (NROW(rows) > 1) {
      # A user may have rated an item multiple times. In such case, get average ratings 
      # for such items.
      rows <- unique(ddply(rows, .(item_id), transform, rating=mean(rating)))  
      # Compute diff of every pair of items.
      pair_rows_nums <- subset(
          expand.grid(rows_num1=1:NROW(rows), rows_num2=1:NROW(rows)),
          rows_num1 != rows_num2)
      data.table(
          item_id1=rows[pair_rows_nums$rows_num1, 'item_id'],
          item_id2=rows[pair_rows_nums$rows_num2, 'item_id'],
          diff=rows[pair_rows_nums$rows_num2, 'rating'] 
              - rows[pair_rows_nums$rows_num1, 'rating'])
    }
  }, ...)
  # ddply is slow when merging data frames within list while rbindlist is 
  # much faster.
  score_diff_per_user <- rbindlist(score_diff_per_user)
  score_diff_per_user$item_id1 <- as.character(score_diff_per_user$item_id1)
  score_diff_per_user$item_id2 <- as.character(score_diff_per_user$item_id2)
  # Compute average score diff between item 1 and item 2.
  model <- score_diff_per_user[, 
                               list(b=mean(diff), support=NROW(diff)),
                               by='item_id1,item_id2']
  setkey(model, item_id1, item_id2)
  return(model)
}


# Predict ratings for multiple users and items given known ratings.
#
# Param:
#  model: A data table produced by build_slopeone.
#  targets: A data table of (user_id, item_id) to predict ratings.
#  ratings: A data table containig (user_id, item_id, rating) of known
#    ratings.
#  ...: Extra parameters for adply.
# Returns:
#  A data table containig (user_id, item_id, predicted_rating).
predict_slopeone <-function(model, targets, ratings, ...) {
  setkey(ratings, user_id)
  adply(targets,
        1,
        function(row) {
          data.frame(
              predicted_rating=predict_slopeone_for_user(
                  model, row$item_id, ratings[J(row$user_id), ]))
        }, ...)
} 

# Predict score for target_item_id given the known ratings of a single user.
#
# Params:
#   model: A data table produced by build_slopeone.
#   target_item_id: Target item id to predict rating.
#   ratings: A data table containing (item id, rating) of the user's known
#     ratings.
#
# Returns:
#   Predicted rating score.
predict_slopeone_for_user <- function(model, target_item_id, ratings) {
  # If target_id is already rated by the user, return that rating.
  already_rated <- subset(ratings, ratings$item_id == target_item_id)
  if (NROW(already_rated) == 1) {
    return(already_rated$rating)
  } else if (NROW(already_rated) > 1) {
    warning(paste(target_item_id, 
                  ' is already rated by user, but there are multiple ratings.'))
    return(already_rated[1, ]$rating)
  }
  # Compute weighted average ratings.
  ratings <- rename(ratings, c('item_id'= "item_id1"))
  ratings <- cbind(ratings, item_id2=target_item_id)
  setkey(ratings, item_id1, item_id2)
  joined <- model[ratings, ]
  joined <- joined[complete.cases(joined), ]
  if (NROW(joined) == 0) {
    return(NA)
  }
  return(sum(model[ratings, (b + rating) * support]$V1) /
         sum(model[ratings, sum(support)]$V1))
}
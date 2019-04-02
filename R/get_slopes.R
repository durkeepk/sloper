#' Extract response-contingency slopes
#'
#' @param data data to use when fitting mixed model
#' @param r_id column of rater idenitfiers
#' @param t_id column of target identifiers
#' @param response column of responses (e.g., ratings, scores)
#' @param contingency column containing values that ratings are contingent on
#' @param compress column to compress data by ('rater' or 'target'). Default is no compression.
#' @return New data frame containing individual slopes and intercepts representing
#' the relationship between ratings and contingencies for each rater
#' @examples
#' ## get_slopes(sloper_exdat, r_id = "r_id", t_id = "t_id",
#' ##           response = "strength_rating", contingency = "target_strength_ave", compress = "rater")
#' ## get_slopes(sloper_exdat, "strength_rating","target_strength_ave")
#' @import dplyr
#' @importFrom magrittr "%>%"
#' @export

get_slopes <- function(data = NULL,
                   r_id = 'r_id', # column identifying raters
                   t_id = 't_id', # column identifying varying targets or timepoints
                   response = "response", # column of ratings or scores
                   contingency = "contingency", # column of rating contingencies
                   compress = F
                   ) {

  # do this super annoying thing to get the actual variables
  df1 <- data
  df <- data
  r_id <- df[[r_id]]
  t_id <- df[[t_id]]
  response <- df[[response]]
  contingency <- df[[contingency]]

  # run the random-effects model and extract slopes
  message("Note: Mixed models can take a while to run.")
  # model <- lme4::lmer(strength_rating ~ t_measured_strength +
  #                       (t_measured_strength | r_id) +
  #                       (t_measured_strength | t_id), data = sloper_exdat)
  model <- lme4::lmer(response ~ contingency +
                        (contingency | r_id) +
                        (contingency | t_id), data = df)
  intercepts_slopes <- stats::coef(model)$r_id
  intercepts_slopes <- data.frame(intercepts_slopes)
  names(intercepts_slopes) <- c('intercept', 'slope')
  intercepts_slopes$r_id <- rownames(intercepts_slopes)
  notflat <- merge(df1, intercepts_slopes, by = 'r_id')

  if(compress == "rater"){
    flat_df_r <- notflat %>%
      dplyr::group_by(r_id) %>%
      dplyr::summarise_all(list(mean))
  flat_df_r <- flat_df_r[grep("t_", colnames(flat_df_r), invert = TRUE)]
  message("Success!")
  return(flat_df_r)
  }

  if(compress == "target"){
    flat_df_t <- notflat %>%
      dplyr::group_by(t_id) %>%
      dplyr::summarise_all(list(mean))
  flat_df_t <- flat_df_t[grep("r_", colnames(flat_df_t), invert = TRUE)]
  message("Success!")
  return(flat_df_t)
  }

  if(compress == F){
    return(notflat)
    message("Success!")
    }


}







#' Extract response-contingency slopes
#'
#' @param data data to use when fitting mixed model.
#' @param r_id column of rater idenitfiers.
#' @param t_id column of target identifiers.
#' @param response column of responses (e.g., ratings, scores)
#' @param contingency column containing values that ratings are contingent on
#' @param compress column to compress data by ('rater' or 'target').
#' Default is no compression.
#' @param maximal Should random slopes be specified for both targets and raters [T]
#' or just targets [F].
#' @return New data frame containing individual slopes and intercepts representing
#' the relationship between ratings and contingencies for each rater.
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
                   compress = "none",
                   maximal = T) {

  # do this super annoying thing to get the actual variables
  df1 <- data
  df <- data
  r_id <- df[[r_id]]
  t_id <- df[[t_id]]
  response <- df[[response]]
  contingency <- df[[contingency]]

  # run the random-effects model and extract slopes
  message("Note: Mixed models can take a while to run.")

  if(maximal == F){
    model <- lme4::lmer(response ~ contingency +
                        (1 + contingency | r_id) +
                        (1 | t_id), data = df)
  }

  if(maximal == T){
    model <- lme4::lmer(response ~ contingency +
                          (contingency | r_id) +
                          (contingency | t_id), data = df)
  }

  intercepts_slopes <- stats::coef(model)$r_id
  intercepts_slopes <- data.frame(intercepts_slopes)
  names(intercepts_slopes) <- c('intercept', 'slope')
  intercepts_slopes$r_id <- rownames(intercepts_slopes)
  notflat <- merge(df1, intercepts_slopes, by = 'r_id')

  if(compress == "rater"){
    final_data <- notflat %>%
      dplyr::group_by(r_id) %>%
      #dplyr::select(-t_id) %>%
      dplyr::summarise_all(list(mean))
    final_data <- final_data[grep("t_", colnames(final_data), invert = TRUE)]
  message("Success!")
  }

  if(compress == "target"){
    final_data <- notflat %>%
      dplyr::group_by(t_id) %>%
      dplyr::summarise_all(list(mean))
    final_data <- final_data[grep("r_", colnames(final_data), invert = TRUE)]
  message("Success!")
  }

  if(compress == "none"){
    message("Success!")
    final_data <- notflat
    }

  return(final_data)

}







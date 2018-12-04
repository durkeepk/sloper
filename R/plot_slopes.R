#' Visualize rating-contingency slopes
#'
#' @param data data to use for plotting
#' @param r_id column of rater idenitfiers
#' @param rating column of ratings (y-axis)
#' @param contingency column containing values that ratings are contingent on (x-axis)
#' @param xname custom name for x-axis
#' @param yname custom name for y-axis
#' @param groupfactor factor/grouping variable (color and linetype)
#' @param gflevels vector of custom labels for factor levels
#' @param linear logical argument indicating whether to plot linear function or nonlinear
#' @return plot of the individual slopes and overall slope
#' @examples
#' # plot_slopes(sloper_exdat, rating = "strength_rating", contingency = "t_measured_strength",
#' #            yname = "Stregth Rating", xname = "Target Strength",
#' #            groupfactor = "ratersex", linear = F, gflevels = c("Men", "Women"))
#' # plot_slopes(sloper_exdat, rating = "strength_rating", contingency = "t_measured_strength")
#' @import ggplot2
#' @importFrom utils capture.output
#' @export

plot_slopes <- function(data = NULL,
                       r_id = 'r_id', # column identifying raters
                       rating = "rating", # column of ratings or scores
                       contingency = "contingency", # column of rating contingencies
                       xname = NULL,
                       yname = NULL,
                       groupfactor = NULL,
                       gflevels = NULL,
                       linear = T
) {

  # do this super annoying thing to get the actual variables
  df <- data
  r_id <- df[[r_id]]
  rating <- df[[rating]]
  contingency <- df[[contingency]]

if(linear == F){message("This may take a while...")}

if(!is.null(groupfactor)){

  groupfactor <- factor(df[[groupfactor]])
  if(!is.null(gflevels)){invisible(capture.output(levels(groupfactor) <- dput(gflevels)))}

p <- ggplot(df, aes(x = contingency, y = rating)) +
  geom_smooth(aes(group = r_id, color = groupfactor,
                  linetype = groupfactor),
                  method = ifelse(linear == T,  "lm", "loess"),
              se = F, size = .4) +
  geom_smooth(method = ifelse(linear == T,  "lm", "loess"),
              color = "black", size = 2) +
  theme_classic() +
  ylab(ifelse(!is.null(yname), yname, "Contingency")) +
  xlab(ifelse(!is.null(xname), xname, "Rating")) +
  theme(legend.position = "bottom", legend.title=element_blank())
  }

if(is.null(groupfactor)){

p <- ggplot(df, aes(x = contingency, y = rating)) +
  geom_smooth(aes(group = r_id),
              method = "lm", se = F, size = .2, color = "darkgray") +
  geom_smooth(method = "lm", color = "black", size = 2) +
  theme_classic() +
  ylab(ifelse(!is.null(yname), yname, "Contingency")) +
  xlab(ifelse(!is.null(xname), xname, "Rating"))
  }

return(p)
}


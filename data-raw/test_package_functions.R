library(sloper)

newdata <- get_slopes(sloper_exdat,
                      rating = "strength_rating",
                      contingency = "t_measured_strength",
                      compress = "rater")

psych::corr.test(subset(newdata))

plot_slopes(sloper_exdat,
            rating = "strength_rating",
            contingency = "t_measured_strength",
            yname = "Stregth Rating",
            xname = "Average Target Strength Rating",
            groupfactor = "ratersex",
            gflevels = c("Men", "Women"),
            linear = T)

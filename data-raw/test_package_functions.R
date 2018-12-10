library(sloper)

plot_slopes(sloper_exdat,
            response = "strength_rating",
            contingency = "t_measured_strength",
            yname = "Stregth Rating",
            xname = "Target Strength (Measured)",
            groupfactor = "ratersex",
            gflevels = c("Men", "Women"),
            linear = T)

newdata <- get_slopes(sloper_exdat,
                      response = "strength_rating",
                      contingency = "t_measured_strength",
                      compress = "rater")

psych::corr.test(subset(newdata))



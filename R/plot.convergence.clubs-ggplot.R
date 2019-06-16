library(ConvergenceClubs)
library(tidyr)
library(ggplot2)
library(dplyr)
library(gridExtra)
library(patchwork)
library(ggforce)

# devtools::install_github("user/LW1949")

data("filteredGDP")

# clubs <- findClubs(filteredGDP, dataCols=2:35, unit_names = 1, refCol=35,
#                    time_trim = 1/3, cstar = 0, HACmethod = "FQSB")
x <- findClubs(filteredGDP, dataCols=2:35, unit_names = 1, refCol=35,
               time_trim = 1/3, cstar = 0, HACmethod = "FQSB")
# summary(clubs)
# clubs
#
#
# plot(clubs)
#

# str(clubs)


### ----------------------------------------------------------------------------


# divergent <- !is.null( x$divergent ) # serve?
#
# X <- attributes(x)$data[,attributes(x)$dataCols]  #dati numerici per calcolare h
# data <- data.frame(id=1:nrow(X), attributes(x)$data) #dati da utilizzare per fare il plot
#
# cdf <- lapply(names(x), # i divergenti vanno considerati?
#        function(cnm){
#            data.frame(id=x[[cnm]]$id, club=cnm)
#        }
# )
# cdf <- Reduce('rbind', cdf)
# data <- merge(cdf, data)
#
# h <- computeH(X, quantity = "h")
#
# ## Domanda:
# #average plot: media di che? di tutti i club, dei soli club nel grafico, vanno incluse le divergenti?
#

### ----------------------------------------------------------------------------


data <- attributes(x)$data[,attributes(x)$dataCols]  #dati numerici per calcolare h
# data <- data.frame(id=1:nrow(X), attributes(x)$data) #dati da utilizzare per fare il plot

cdf <- lapply(names(x),
              function(cnm){
                  data.frame(id=factor(x[[cnm]]$id), club=cnm)
              }
)
cdf <- Reduce('rbind', cdf)

h <- data.frame(id=factor(seq_len(nrow(data))), computeH(data, quantity = "h"))
h <- merge(cdf, h)

## Domanda:
#average plot: media di che? di tutti i club, dei soli club nel grafico, vanno incluse le divergenti?


a <-
    h %>%
    gather('period', 'h', seq_len(ncol(h))[-c(1,2)])


# a %>%
# ggplot( aes(x=period, y=h, colour=id)) +
#     geom_line(aes(group=id) ) +
#     # facet_wrap(~club)
#     facet_wrap_paginate(~club)




out <- by(data = a, INDICES = a$club, FUN = function(p) {
    p <- droplevels(p)
    nr <- nrow(p)
    p <- ggplot(p, aes(x=period, y=h, colour=id, group=id)) +
        # geom_point(aes(x=period[seq(1, nr, by=2)], y = h[seq(1,nr,by=2)]) ) +
        geom_point(size=1) +
        geom_line() +
        facet_wrap(~club) +
        theme(legend.key.height = unit(0.6, 'lines')
              # , legend.box.just = 'top'
              , legend.justification = "top"  # (se position è 'right')
        )
    # guide_legend(title = waiver(), title.position = NULL,
    #              title.theme = NULL, title.hjust = NULL, title.vjust = NULL,
    #              label = TRUE, label.position = NULL, label.theme = NULL,
    #              label.hjust = NULL, label.vjust = NULL,
    #              keywidth = grid::unit(2, 'pt'),
    #              keyheight = NULL, direction = NULL, default.unit = "line",
    #              override.aes = list(), nrow = NULL, ncol = NULL, byrow = FALSE,
    #              reverse = FALSE, order = 0)
})


### cowplot
pp <- cowplot::plot_grid(plotlist = out, align = 'hv')
ggsave('provacow.pdf', plot=pp)


pp


dev.new()
pp
dev.off()

### grid.arrange
# do.call(grid.arrange, out)

# If you want to supply the parameters to grid.arrange
# do.call(grid.arrange, c(out, ncol=3))


### patchwork
pp <- patchwork::wrap_plots(out)
ggplot2::ggsave('prova.pdf', plot=pp)


### multiplot
# https://geekcologist.wordpress.com/2018/09/21/multiplot-with-ggplot/


# ggplot2::ggsave(filename, plot = last_plot(), device = NULL, path = NULL,
#        scale = 1, width = NA, height = NA, units = c("in", "cm", "mm"),
#        dpi = 300, limitsize = TRUE, ...)
### ----------------------------------------------------------------------------












z <-
    structure(list(Date = structure(c(2L, 1L, 3L, 4L, 5L, 2L, 1L,
                                      3L, 4L, 5L, 2L, 1L, 3L, 4L, 5L), .Label = c("1/2/2012", "12/1/2011",
                                                                                  "2/1/2012", "2/10/2012", "2/13/2012"), class = "factor"), Server = structure(c(1L,
                                                                                                                                                                 1L, 1L, 1L, 1L, 2L, 2L, 2L, 2L, 2L, 3L, 3L, 3L, 3L, 3L), .Label = c("A",
                                                                                                                                                                                                                                     "B", "C"), class = "factor"), FileSystem = structure(c(1L, 9L,
                                                                                                                                                                                                                                                                                            14L, 5L, 2L, 10L, 11L, 12L, 6L, 13L, 1L, 8L, 3L, 7L, 4L), .Label = c("/",
                                                                                                                                                                                                                                                                                                                                                                 "/app", "/data", "/database", "/db", "/restore", "/Storage",
                                                                                                                                                                                                                                                                                                                                                                 "/tmp", "/var", "C:", "D:", "F:", "G:", "tmp"), class = "factor"),
                   PercentUsed = c(60L, 50L, 90L, 86L, 90L, 67L, 67L, 34L, 89L,
                                   56L, 90L, 78L, 67L, 34L, 12L)), .Names = c("Date", "Server",
                                                                              "FileSystem", "PercentUsed"), class = "data.frame", row.names = c(NA,
                                                                                                                                                -15L))



# make list of plots
ggList <- lapply(split(x, x$Server), function(i) {
    ggplot(i, aes(Date, PercentUsed, group = 1, colour = FileSystem)) +
        geom_jitter(size = 2) +
        geom_smooth(method = "loess", se = TRUE)})

# plot as grid in 1 columns
cowplot::plot_grid(plotlist = ggList, ncol = 1,
                   align = 'v', labels = levels(x$Server))





### dividere su più pagine

library(grid)
grid.arrange(rectGrob(), rectGrob())
## Not run:
library(ggplot2)
pl <- lapply(1:11, function(.x) qplot(1:10, rnorm(10), main=paste("plot", .x)))
ml <- marrangeGrob(pl, nrow=2, ncol=2)
## non-interactive use, multipage pdf
ggsave("multipage.pdf", ml)
## interactive use; open new devices
ml

## End(Not run)



source('mc.plot.igraph.R')
library(scales)

a = read.csv('test8.csv')

# params
blue = brewer_pal(pal = 'Blues')(9)[c(3,5,7)]
blues = ps(blue, toupper(as.hexmode(round(1 * 255))))
lwd = 3

plotp <-
function(df, main, sub) {
    par(mar=c(5,5,7,5), family = 'Roboto Light')
    edges = df$edges
    plot(edges, df$igraph, type = 'l', col = blues[1], lwd = lwd, 
         xlab = '-- Number of edges --', cex.lab = 1.2, 
         ylab = '-- Seconds elapsed --', main = '', las = 1, family = 'Roboto Light')
    lines(edges, df$seg, col = blues[2], lwd = lwd, type = 'l')
    lines(edges, df$seg.mc, col = blues[3], lwd = lwd, type = 'l')
    axis(4, las = 1, family = 'Roboto Light')
    abline(h = axTicks(2), col = 'grey70', lty = 3)
    abline(v = axTicks(1), col = 'grey70', lty = 3)
    legend(x = min(edges), y = (max(df$igraph)/2)*1.15, 
           legend = c('igraph', 'seg', 'seg.mc'), col = blues, lwd = lwd)
    mtext(main, side = 3, line = 3, family = 'Roboto', cex = 1.5)
    mtext(sub, side = 3, line = 1.5, family = 'Roboto Light', cex = 1.2, col = 'grey30')
}

for (n in unique(a$side.px)) {
    cDf = a[which(a$side.px == n), ]
    svg(ps('8-perf-px-', n, '.svg'), width = 14, height = 10)
        plotp(cDf, main = ps('Performance comparison on 1000-node Erdos-Renyi networks (', n, 'x', n, 'px output png)'), 
              sub = 'Ubuntu 12.04 / 4 x Intel i5-2400 CPUs @ 3.10GHz / 16GB RAM')
    dev.off()
}

# perfomance ~ picture size
a = data.frame(a, mean = rowMeans(a[, c('igraph', 'seg', 'seg.mc')]))
png('8-perf-px-boxplot.png', width = 700, height = 700)
    par(mar=c(5,6,5,5), family = 'Roboto Light', bty = 'n', cex.lab = 1.2)
    boxplot(mean ~ side.px, data = a, family = 'Roboto Light', las = 1, col = blues[3], 
            xlab = 'PNG side size in pixels', ylab = 'Seconds elapsed')
    mtext('Rendering time vs. bitmap size', side = 3, line = 1.5, family = 'Roboto', cex = 1.5, col = 'grey10')
    abline(h = axTicks(2), lty = 3, col = 'grey80')
dev.off()

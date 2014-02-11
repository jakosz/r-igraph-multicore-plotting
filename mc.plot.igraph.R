library(parallel)
library(igraph)

# ------------------------------------------------------------------------------ helpers:

# Plots a blank window, width coordinates derived from layout:
plot_empty <-
function(xlim = c(-1, 1), ylim = c(-1, 1), layout = NULL) {
    if (!is.null(layout)) {
        xlim = c(min(layout[,1]), max(layout[,1]))
        ylim = c(min(layout[,2]), max(layout[,2]))
    }
    plot(0,xlim=xlim,ylim=ylim,bty='n',xaxt='n',yaxt='n',xlab='',ylab='',col='white')
}

# Groups indices of a list or vector into n (almost) equal chunks:
iTable <-
function (x, n) 
{
    x = seq_along(x)
    i = floor(seq(from = x[1], to = x[length(x)], length.out = n + 
        1))
    res = data.frame(from = i[1:(length(i) - 1)], to = i[2:length(i)])
    res$from[2:nrow(res)] = res$from[2:nrow(res)] + 1
    return(res)
}

# Alias of paste:
ps <-
function (..., sep = "") 
{
    return(paste(..., sep = sep))
}

# Invokes imagemagick to merge png files and cleans up tmp files produced by plot_segments_mc:
compose_clean <-
function(file, mc.cores) {
    t0 = Sys.time()
    for (i in mc.cores:1) {
        system(ps('composite mcp-tmp-', i, '.png mcp-tmp-', i - 1, '.png mcp-tmp-', i - 1, '.png'))
    }
    # composite execution time; catched by a testing script
    .imTime <<- Sys.time() - t0
    print(.imTime)
    file.rename('mcp-tmp-0.png', file)
    file.remove(ps('mcp-tmp-', 1:mc.cores, '.png'))
}

# ------------------------------------------------------------------------------ plotting functions:

# Plots parts of the graph using multiple cores; 
# edge drawing method is the same as in plot_segments
plot_segments_mc <-
function(iGraph, file, width = 500, height = 500, mc.cores = detectCores(), 
         layout = NULL, layout.fun = layout.auto) {
    if (is.null(layout)) layout = layout.fun(iGraph)
    # (nodes is a data.frame for possible future extensions, like size & color)
    nodes = data.frame(id = as.vector(V(iGraph)))
    edges = get.data.frame(iGraph, 'e')
    subg = iTable(1:nrow(edges), mc.cores)
    # plot nodes
    png('mcp-tmp-0.png', width = width, height = height)
        plot_empty(layout = layout)
        points(x = layout[,1], y = layout[,2], pch = 19)
    dev.off()
    # plot edges    
    mclapply(1:mc.cores, mc.cores = mc.cores, function(i) {
        # select edges subset
        cEdges = edges[subg$from[i]:subg$to[i], ]
        # plot
        png(ps('mcp-tmp-', i, '.png'), width = width, height = height)
            par(bg=NA)
            plot_empty(layout = layout)        
            lapply(1:nrow(cEdges), function(j) {
                iFrom = which(nodes$id == cEdges$from[j])
                iTo = which(nodes$id == cEdges$to[j])
                    segments(x0 = layout[iFrom, 1], x1 = layout[iTo, 1], 
                             y0 = layout[iFrom, 2], y1 = layout[iTo, 2], 
                             lwd = 1, col = rgb(0,0,0,.2))
            })
        dev.off()
    })
    compose_clean(file, mc.cores)
}

# Plots a graph using segments() function:
plot_segments <-
function(iGraph, layout = NULL, layout.fun = layout.auto) {
    if (is.null(layout)) layout = layout.fun(iGraph)
    # (nodes is a data.frame for possible future extensions, like size & color)
    nodes = data.frame(id = as.vector(V(iGraph)))
    edges = get.data.frame(iGraph)
    # window:
    plot_empty(layout = layout)
    # edges
    lapply(1:nrow(edges), function(i) {
        iFrom = which(nodes$id == edges$from[i])
        iTo = which(nodes$id == edges$to[i])
        segments(x0 = layout[iFrom, 1], x1 = layout[iTo, 1], 
                 y0 = layout[iFrom, 2], y1 = layout[iTo, 2], 
                 lwd = 1, col = rgb(0,0,0,.2))
    })
    # nodes
    points(x = layout[,1], y = layout[,2], pch = 19)
}

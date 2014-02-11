source('mc.plot.igraph.R')

# Wrappers: include opening and closing of graphical device;

plot_igraph <-
function(g, file, width, height) {
    png(file, width = width, height = width)
        plot(g, vertex.size = 3, vertex.color = 'black', vertex.frame.color = NA, 
             vertex.label = NA, edge.arrow.mode = 0, edge.color = rgb(0,0,0,.2))
    dev.off()
}

png_segments <-
function(g, file, width, height) {
    png(file, width = width, height = width)
        plot_segments(g)
    dev.off()    
}

# Fire all three functions on an igraph object and get the execution times:
test_times <-
function(g, width, height) {
    return(list(
    tig = system.time(plot_igraph(g, file = 'igraph.png', width = width, height = height)), 
    tse = system.time(png_segments(g, file = 'segments.png', width = width, height = height)), 
    tmc = system.time(plot_segments_mc(g, file = 'segments_mc.png', width = width, height = height)), 
    imt = as.numeric(.imTime)
    ))
}

# Measuers execution times for given combinations of n, p.or.m and wh (image size)
test_erdos <-
function(n, p.or.m, wh, file) {
    res = data.frame(game = 0, nodes = 0, edges = 0, side.px = 0, igraph = 0, seg = 0, seg.mc = 0, composite = 0)[-1, ]
    for (i in n) {
        for (j in p.or.m) {
            g = erdos.renyi.game(n = i, p.or.m = j)
            for (px in wh) {
                cTest = test_times(g, px, px)
                res = rbind(res, 
                            data.frame(game = 'erdos.renyi', 
                                       nodes = i, 
                                       edges = length(E(g)), 
                                       side.px = px,
                                       igraph = as.vector(cTest$tig[3]), 
                                       seg = as.vector(cTest$tse[3]), 
                                       seg.mc = as.vector(cTest$tmc[3]), 
                                       composite = cTest$imt))
                write.csv(res, file, row.names = FALSE)
                Sys.sleep(3)
            }
            rm(g); gc()
        }
    }
    return(res)
}

test_erdos(n = 1000, p.or.m = (1:100/100)^3, wh = c(500, 1000, 2000, 4000, 8000), file = 'seg_mc_test.csv')

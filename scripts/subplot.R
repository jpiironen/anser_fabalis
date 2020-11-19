

ggplot_xrange <- function(pp) {
  # returns x-range for a given ggplot-object
  build <- ggplot_build(pp)
  build$layout$panel_scales$x[[1]]$range$range
}

ggplot_yrange <- function(pp) {
  # returns y-range for a given ggplot-object
  build <- ggplot_build(pp)
  build$layout$panel_scales$y[[1]]$range$range
}


set_panel_heights <- function(grobs, heights){
  # taken from https://stackoverflow.com/questions/36198451/specify-widths-and-heights-of-plots-with-grid-arrange
  #grobs$heights <- grid:::unit.list(grobs$heights) # hack until R 3.3 comes out
  id_panels <- unique(grobs$layout[grobs$layout$name=="panel", "t"])
  grobs$heights[id_panels] <- heights
  grobs
}

set_panel_widths <- function(grobs, widths){
  #grobs$widths <- grid:::unit.list(grobs$widths) # hack until R 3.3 comes out
  id_panels <- unique(grobs$layout[grobs$layout$name=="panel", "l"])
  grobs$widths[id_panels] <- widths
  grobs
}


get_row_titles <- function(labels, titlesize=10, angle=90, add_blank=FALSE) {
  #
  # Creates the row labels (=titles) for a subplot. This is simply a list of ggplots with
  # only the annotation that equals the corresponding row label.
  #
  n <- length(labels)
  size_scalar <- 3/8
  pps <- list()
  if (add_blank) {
    theme(axis.line = element_blank())
    blank_frame <- ggplot() + 
      theme_classic() + 
      theme(plot.margin=unit(c(0,0,0,0),'points'), axis.line = element_blank())
    pps <- c(pps, list(blank_frame))
  }
    
  
  for (i in 1:n) {
    hjust <- 0.5*(angle==90) # angle=0 => hjust=0, angle=90 => hjust=0.5
    vjust <- ifelse(angle==90, 1, 0.5)
    ppann <- ggplot() + xlim(0,1) +
      annotate('text', x=0, y=0, label=labels[i], hjust=hjust, vjust=vjust, size=titlesize*size_scalar, angle=angle) + 
      theme(panel.background = element_blank(),  panel.grid = element_blank(),
            panel.border = element_blank(), axis.line=element_blank(),
            axis.title = element_blank(), axis.text = element_blank(), axis.ticks = element_blank(),
            plot.margin = unit(c(0,0,0,0),'points'))
    pps <- c(pps, list(ppann))
  }
  return(pps)
}


get_col_titles <- function(labels, titlesize=10) {
  #
  # Creates the column labels (=titles) for a subplot. This is simply a list of ggplots with
  # only the annotation that equals the corresponding row label.
  #
  n <- length(labels)
  size_scalar <- 3/8
  pps <- list()
  
  for (i in 1:n) {
    ppann <- ggplot() + xlim(0,1) + ylim(0,1) +
      annotate('text', x=0.5, y=1, label=labels[i], vjust=1, hjust=0.5, size=titlesize*size_scalar) + 
      theme(panel.background = element_blank(), panel.grid = element_blank(),
            panel.border = element_blank(), axis.line=element_blank(),
            axis.title = element_blank(), axis.text = element_blank(), axis.ticks = element_blank(),
            plot.margin = unit(c(0,0,0,0),'points'))
    pps <- c(pps, list(ppann))
  }
  return(pps)
}




subplot <- function(plotlist, nrow=1, byrow=T, heights=NULL, widths=NULL, 
                    hspace=0.025, vspace=0.025, tight=FALSE,
                    xlabs=NULL, ylabs=NULL, axlab.size=9, ticklab.size=8, 
                    ylab.angle=90, ylab.space=5,
                    rowtitles=NULL, coltitles=NULL, rowtitle.size=10, coltitle.size=10, 
                    rowtitles.width=0.35, coltitles.height=0.35, rowtitle.angle=90, 
                    tight.marginal=0.01) {
  
  require('ggplot2')
  require('gridExtra')
  require('grid')
  
  if (!('list' %in% class(plotlist)))
    plotlist <- list(plotlist)
  
  n <- length(plotlist)
  ncol <- ceiling(n / nrow)
  if (byrow) {
    rowind <- rep(1:nrow, each=ncol)
    colind <- rep(1:ncol, nrow)
  } else {
    rowind <- rep(1:nrow, ncol)
    colind <- rep(1:ncol, each=nrow)
  }
  
  # set x and y labels if they are given
  if (!is.null(xlabs)) {
    if (length(xlabs)==1)
      xlabs <- rep(xlabs, ncol)
    bottomrowinds <- which(rowind==nrow)
    for (i in seq_along(bottomrowinds)) {
      k <- bottomrowinds[i]
      if (k > n)
        # fewer plots than nrow*ncol
        break
      plotlist[[k]] <- plotlist[[k]] + xlab(xlabs[i]) + theme(axis.title.x = element_text(size=axlab.size))
    }
    for (k in which(rowind!=nrow))
      plotlist[[k]] <- plotlist[[k]] + theme(axis.title.x = element_blank())
  } else {
    # x labels not given, but set the label size
    for (k in seq_along(plotlist))
      plotlist[[k]] <- plotlist[[k]] + theme(axis.title.x = element_text(size=axlab.size))
  }
  if (!is.null(ylabs)) {
    if (length(ylabs)==1)
      ylabs <- rep(ylabs, nrow)
    firstcolinds <- which(colind==1)
    for (i in seq_along(firstcolinds)) {
      k <- firstcolinds[i]
      ylabstyle <- element_text(size=axlab.size, angle=ylab.angle, hjust=0.5, vjust=0.5,
                                margin = ggplot2::margin(r=ylab.space))
      plotlist[[k]] <- plotlist[[k]] + ylab(ylabs[i]) + theme(axis.title.y = ylabstyle )
    }
    for (k in which(colind!=1)) {
      if (k > n)
        # fewer plots than nrow*ncol
        break
      plotlist[[k]] <- plotlist[[k]] + theme(axis.title.y = element_blank())
    }
  } else {
    # y labels not given, but set the label size
    for (k in seq_along(plotlist))
      plotlist[[k]] <- plotlist[[k]] + theme(axis.title.y = element_text(size=axlab.size))
  }
  
  # set the margins and tick label sizes
  for (i in seq_along(plotlist)) {
    # top, right, bottom, left
    if (tight) {
      marg <- unit(c(max(0.5*vspace*(rowind[i]>1), tight.marginal), 
                     max(0.5*hspace*(colind[i]<ncol), tight.marginal), 
                     max(0.5*vspace*(rowind[i]<nrow), tight.marginal), 
                     max(0.5*hspace*(colind[i]>1), tight.marginal)), units='npc')
    }
    else
      marg <- unit(c(0.5*vspace, 0.5*hspace, 
                     0.5*vspace, 0.5*hspace), units='npc')
    # if tick labels are removed (element_blank), then do not show them; otherwise use them and set the correct label size
    if('element_blank' %in% class(plotlist[[i]]$theme$axis.text.y) ||
       'element_blank' %in% class(plotlist[[i]]$theme$axis.text))
      axis_text_y <- element_blank()
    else
      axis_text_y <- element_text(size=ticklab.size)
    if('element_blank' %in% class(plotlist[[i]]$theme$axis.text.x) ||
       'element_blank' %in% class(plotlist[[i]]$theme$axis.text))
      axis_text_x <- element_blank()
    else
      axis_text_x <- element_text(size=ticklab.size)
    plotlist[[i]] <- plotlist[[i]] + theme(plot.margin=marg, axis.text.x = axis_text_x, axis.text.y = axis_text_y)
  }
  
  # grobs for the actual plots
  grobs <- lapply(plotlist, ggplotGrob)
  
  if (is.null(widths))
    widths <- rep(1,ncol)
  if (is.null(heights))
    heights <- rep(1,nrow)
  
  # row and column labels 
  if (!is.null(rowtitles)) {
    rowtitles_list <- get_row_titles(rowtitles, titlesize=rowtitle.size, angle=rowtitle.angle,
                                     add_blank = !is.null(coltitles))
    rowtitles_grobs <- lapply(rowtitles_list, ggplotGrob)
    widths <- c(rowtitles.width, widths)
  }
  
  # else
    # rowtitles_list <- NULL
  if (!is.null(coltitles)) {
    coltitles_list <- get_col_titles(coltitles, titlesize=coltitle.size)
    coltitles_grobs <- lapply(coltitles_list, ggplotGrob)
    heights <- c(coltitles.height, heights)
  }
  
    
  cols <- NULL # this will contain all the columns
  
  j <- 0
  for (icol in 1:(ncol+!is.null(rowtitles))) {
    
    if (icol == 1 && !is.null(rowtitles)) {
      # the first column is the row title column
      if (is.null(rowtitles))
        next
      col_grobs <- rowtitles_grobs
    } else {
      # plot indices that are in the jth column
      j <- j+1
      if (byrow)
        ind <- seq(j,n,by=ncol)
      else {
        ind <- (j-1)*nrow + (1:nrow)
        ind <- ind[ind <= n] # this is needed to make this work if the number of plots is not exactly nrow*ncol
      }
      # pick the plots
      col_grobs <- grobs[ind]
    }
    
    # add the column title as the first plot in the column
    if (!is.null(coltitles) && j > 0)
      col_grobs <- c(list(coltitles_grobs[[j]]), col_grobs)
    
    # adjust the heights of the plots in the column
    col_grobs <- sapply(seq_along(col_grobs), function(ri) set_panel_heights(col_grobs[[ri]], unit(heights[ri],'null')) )
    
    # bind the rows of the plots in the same column
    col <- col_grobs[[1]]
    for (k in seq_along(col_grobs)[-1])
      col <- gtable_rbind(col, col_grobs[[k]])
    
    # adjust the width of the column
    col <- set_panel_widths(col, unit(widths[icol],'null'))
    
    # bind the column with the previous columns
    if (icol == 1)
      cols <- col
    else {
      if (nrow(col)!=nrow(cols)) {
        # not enough plots for full grid plot, so add a dummy plot
        theme_null <- theme(panel.border = element_blank(), panel.background = element_blank())
        col <- gtable_rbind(col, ggplotGrob(ggplot()+theme_null))
      }
      cols <- gtable_cbind(cols, col, size='max')
    }
    
  }
  
  # plot
  grid.draw(cols)
}








add_legend <- function(pp, ind=NULL, labels=NULL, position=c(0.75,0.75),
                       justification = c(0,0), background = element_blank(), 
                       textsize = 6, keysize = 0.035, ...) {
  
  if (is.null(ind))
    ind <- 1:length(pp$layers)
  if (is.null(labels))
    labels <- letters
  
  tags <- letters
  colors <- rep(NA, length(ind))
  linetypes <- rep(NA, length(ind))
  
  for (i in seq_along(ind)) {
    if ( 'legend' %in% names(pp$layers[[ind[i]]]) ) {
      # given an object for which this object has been called before
      color <- pp$layers[[ind[i]]]$legend$colour
      linetype <- pp$layers[[ind[i]]]$legend$linetype
    } else {
      # fetch the user specified color and linetype
      color <- pp$layers[[ind[i]]]$aes_params$colour 
      linetype <- pp$layers[[ind[i]]]$aes_params$linetype
    }
    # if not set, use defaults
    colors[i] <- ifelse(!is.null(color), color, 'black') 
    linetypes[i] <- ifelse(!is.null(linetype), linetype, 1)
    
    # destroy the color from the aes_param list
    pp$layers[[ind[i]]]$aes_params$colour <- NULL 
    pp$layers[[ind[i]]]$aes_params$linetype <- NULL 
    
    # set the colour and linetype to be a certain tag
    pp$layers[[ind[i]]]$mapping$colour <- tags[i] 
    pp$layers[[ind[i]]]$mapping$linetype <- tags[i] 
    
    # save for later use
    pp$layers[[ind[i]]]$legend$colour <- colors[i] 
    pp$layers[[ind[i]]]$legend$linetype <- linetypes[i] 
  }
  
  # map the tags to the user specified colours and linetypes
  pp + scale_color_manual(name='', values=colors, labels=labels) +
    scale_linetype_manual(name='', values=linetypes, labels=labels) +
    theme(legend.position = position, legend.justification = justification,
                   legend.background = background, legend.text = element_text(size=textsize),
                   legend.key.size = unit(keysize,units = 'npc'))
}




gg_circle <- function(r, xc, yc, color="black", fill=NA, ...) {
  # taken from https://stackoverflow.com/questions/6862742/draw-a-circle-with-ggplot2
  x <- xc + r*cos(seq(0, pi, length.out=100))
  ymax <- yc + r*sin(seq(0, pi, length.out=100))
  ymin <- yc + r*sin(seq(0, -pi, length.out=100))
  annotate("ribbon", x=x, ymin=ymin, ymax=ymax, color=color, fill=fill, ...)
}





get_nticks <- function(vmin, vmax, prec=0.1, n=3, by=NULL, extra=0) {
  range <- c(vmin, vmax)
  range <- range + (range[2]-range[1])*extra*c(-1,1) # add little extra
  # range <- round(range/prec)*prec # round to nearest 'prec'
  range[1] <- floor(range[1]/prec)*prec # round down within accuracy prec
  range[2] <- ceiling(range[2]/prec)*prec # round up within accuracy prec
  if (is.null(by))
    ticks <- seq(range[1], range[2], len=n)
  else 
    ticks <- seq(range[1], range[2], by=by)
  
  return(ticks)
}







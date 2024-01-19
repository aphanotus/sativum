#' Label a clade on the plot of a phylogenetic tree
#'
#' This function modifies \code{phytools::cladelabels}, written by Liam Revell,
#' making it easier to identify clades based on tip labels and
#' to allow users more formatting options.
#' The names provided to \code{clade} can be any unique component of the taxon names (tip labels).
#' Only works with rightward facing trees.
#'
#' @param tree An object of class \code{phylo}.
#' @param text Text that will label the clade.
#' @param clade A character vector with two or more taxon names (tip labels) to define the clade.
#' @param offset An optional integer value to shift the clade label to the right.
#' @param wing.length An optional integer value for the length of top and bottom brackets on the line that highlights the clade.
#' @param cex An optional integer value for the character expansion factor to make the text larger or smaller.
#' @param orientation The orientation of the text. Can be orientation = "vertical" (the default) or "horizontal".
#' @param font An integer specifying the type of font for the labels: 1 (plain text), 2 (bold), 3 (italic, the default), or 4 (bold italic).
#' @param line.width An integer specifying the width of branches in the tree.
#' @param line.color A color name for the line.
#' @param text.color A color name for the label text.
#'
#' @source Dave Angelini \email{david.r.angelini@@gmail.com} [aut, cre]
#'
#' @export
#'
#' @examples
#' plot.tree(anole.tree)
#'

label.clade <- function (
    tree = NULL, text, clade, offset = NULL, wing.length = NULL,
    cex = 1, orientation = "vertical",
    font = 2, line.width = 5,
    line.color = "black", text.color = "black" )
{
  if (!require(ape)) { stop("Please run  `install.packages('ape')`  first.") }

  if (class(tree)[1] != "phylo") {
    stop("Argument `tree` is not a recognized data type. (See the help entry: `?label.clade`)\n")
  }
  if (class(text)[1] != "character") {
    stop("Argument `text` is not a recognized data type. (See the help entry: `?label.clade`)\n")
  }
  if (class(clade)[1] != "character") {
    stop("Argument `clade` is not a recognized data type. (See the help entry: `?label.clade`)\n")
  }
  if (length(clade)<2) {
    stop("Argument `clade` requires at least two names. (See the help entry: `?label.clade`)\n")
  }
  tip.numbers <- unique(unlist(lapply(clade, function(x) grep(x,tree$tip.label))))
  if(length(tip.numbers!=length(clade))) {
    stop("The names provides to the arguement `clade` do not uniquely identify taxa. (See the help entry: `?label.clade`)\n")
  }
  node <- getMRCA(tree, tip.numbers)

  # Revell's original code
  # subfunction
  labelsubtree <- function(tree,nn,label,pp,offset,wl,cex,orientation, font, line.width, line.color, text.color){
    if(is.null(wl)) wl<-1
    tree<-reorder(tree)
    tips<-phytools::getDescendants(tree,nn)
    tips<-tips[tips<=Ntip(tree)]
    ec<-0.7 ## expansion constant
    sw<-pp$cex*max(strwidth(tree$tip.label[tips]))
    sh<-pp$cex*max(strheight(tree$tip.label))
    cw<-mean(strwidth(LETTERS)*cex)
    h<-max(sapply(tips,function(x,tree)
      phytools::nodeHeights(tree)[which(tree$edge[,2]==x),2],
      tree=tree))+sw+offset*cw
    y<-range(pp$yy[tips])
    lines(c(h,h),y+ec*c(-sh,sh),col=line.color,lwd=line.width)
    lines(c(h-wl*cw,h),
          c(y[1]-ec*sh,y[1]-ec*sh),col=line.color,lwd=line.width)
    lines(c(h-wl*cw,h),
          c(y[2]+ec*sh,y[2]+ec*sh),col=line.color,lwd=line.width)
    text(h+cw,mean(y),
         label,srt=if(orientation=="horizontal") 0 else 90,
         adj=if(orientation=="horizontal") 0 else 0.5,cex=cex,
         col=text.color, font = font)
    list(xx=c(h,h),yy=y+ec*c(-sh,sh))
  }
  # End subfunction

  lastPP <- get("last_plot.phylo", envir = .PlotPhyloEnv)
  if (is.null(tree)) {
    if (is.null(wing.length))
      wing.length <- 1
    if (is.null(offset))
      offset <- 8
    tree <- list(edge = lastPP$edge, tip.label = 1:lastPP$Ntip,
                 Nnode = lastPP$Nnode)
    H <- matrix(lastPP$xx[tree$edge], nrow(tree$edge), 2)
    tree$edge.length <- H[, 2] - H[, 1]
    class(tree) <- "phylo"
  }
  if (is.null(offset))
    offset <- 0.5
  xx <- mapply(labelsubtree, node, text, MoreArgs = list(
    tree = tree,
    font = font, line.width = line.width,
    line.color = line.color, text.color = text.color,
    pp = lastPP, offset = offset, wl = wing.length, cex = cex,
    orientation = orientation), SIMPLIFY = FALSE)
}
# End of function

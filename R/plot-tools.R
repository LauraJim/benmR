# Function 'plotdata' --------
#
#' Visualization of datasets to be used for niche estimatition.
#'
#' \code{plotdata} returns two plots that display the data needed to estimate the
#' niche of a species in both geographical and environmental space.
#'
#' This function produce a visualization of the background points, the
#' occurrence data and the tolerance ranges of the species of interest, in both
#' the geographical and the environmental spaces (displaying only the firs two
#' or three dimensions of the latter).
#' The first two columns of \code{back} must contain the longitude and latitude
#' coordinates of all the points in the grid that define the region of interest,
#' the remaining columns must contain the values (at least two) of the
#' environmental variables at each location. Similarly, the first two columns of
#' \code{occu.sp} must contain the geographical coordinates for the occurrence
#' data, and the remaining columns must contain the environmental combinations.
#' \code{tolran} must provide the tolerance limits (lower and upper bounds) for
#' each environmental variable odd columns contain lower limits even columns
#' containd the upper limits.
#'
#' @param back numeric matrix that contains the background data for the species.
#' @param occ.sp numeric matrix that contains the species occurrence data.
#' @param tolran numeric matrix with one row and four columns named: xleft,
#'   ybottom, xright, ytop.
#' @param sp.col character strign with a color name identifying the species.
#' @return A plot with two panels. The panel on the left will show the data in
#'   geographical space and the panel on the right will show the data in
#'   environmental space.
#' @examples
#' plotdata(back1,occsp1,tolran1,"tomato")
#
# CODE:
plotdata <- function(back,occ.sp,tolran,sp.col)
{
  par(mfrow=c(1,2))

  ## Geographical Space:
  # Plot the geographical locations of reported presences
  # of the species, on top of the global environmental variables
  plot(back[,1],back[,2],pch=".",col=1,xlab="Longitude",ylab="Latitude",main="Geographical space")
  points(occ.sp[,1],occ.sp[,2],pch=19,col=sp.col)

  ## Environmental Space:
  # Plot environmental variables of species data and the location of reported
  # presences of the species on top
  plot(back[,3],back[,4], pch=".", col=1,xlab="Bio1WHStnd",ylab="Bio12WHStnd",main="Environmental space")
  points(occ.sp[,3],occ.sp[,4], pch=19, col=sp.col)
  rect(xleft=tolran$xleft,xright=tolran$xright,ybottom=tolran$ybottom,ytop=tolran$ytop,border="gold",lwd=2)
  legend("topleft",legend=c("Species presences","Tolerance ranges"),pch=c(19,NA),
         bty="n",lty=c(0,1),col=c(sp.col,"gold"),lwd=2)
}

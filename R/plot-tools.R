# Function 'plotdata' --------
#
#' Visualization of datasets to be used for niche estimation.
#'
#' \code{plotdata} returns two plots that display the data needed to estimate the
#' niche of a species in both geographical and environmental space.
#'
#' This function produce a visualization of the background points, the
#' occurrence data and the tolerance ranges of the species of interest, in both
#' the geographical and the environmental spaces (displaying only the firs two
#' or three dimensions of the latter).
#'
#' The first two columns of \code{back} must contain the longitude and latitude
#' coordinates of all the points in the grid that define the region of interest,
#' the remaining columns must contain the values (at least two) of the
#' environmental variables at each location. Similarly, the first two columns of
#' \code{occu.sp} must contain the geographical coordinates for the occurrence
#' data, and the remaining columns must contain the environmental combinations.
#' See the datasets \code{backAM} and \code{Spocc}, they are examples of the
#' matrices \code{back} and \code{occu.sp}.
#'
#' \code{tolran} must provide the tolerance limits (lower and upper bounds) for
#' each environmental variable odd columns contain lower limits even columns
#' containd the upper limits.
#'
#' @param back numeric matrix that contains the background data for the species.
#' @param occ.sp numeric matrix that contains the species occurrence data.
#' @param tolran numeric matrix with one row and four columns named: xleft,
#'   ybottom, xright, ytop.
#' @param sp.col character strign with a color name identifying the species.
#' @param envnames character vector with the names of the environmental
#'   variables to work with.
#' @return A plot with two panels. The panel on the left will show the data in
#'   geographical space and the panel on the right will show the data in
#'   environmental space.
#' @examples
#' data("backAM","Spocc")
#' tolran1 <- c(0,1.2,-1,0.8)
#' plotdata(backAM,Spocc,tolran1,"tomato",c("Annual Mean Temp","Annual Mean Precip"))
# CODE:
plotdata <- function(back,occ.sp,tolran,sp.col,envnames)
{
  back <- as.matrix(back)
  occ.sp <- as.matrix(occ.sp)
  par(mfrow=c(1,2))

  ## Geographical Space:
  # Plot the geographical locations of reported presences
  # of the species, on top of the global environmental variables
  plot(back[,1],back[,2],pch=".",col=1,xlab="Longitude",ylab="Latitude",main="Geographical space")
  points(occ.sp[,1],occ.sp[,2],pch=19,col=sp.col)

  ## Environmental Space:
  # Plot environmental variables of species data and the location of reported
  # presences of the species on
  plot(back[,3],back[,4], pch=".", col=1,xlab=envnames[1],ylab=envnames[2],main="Environmental space")
  points(occ.sp[,3],occ.sp[,4], pch=19, col=sp.col)
  rect(xleft=tolran[1],xright=tolran[2],ybottom=tolran[3],ytop=tolran[4],border="gold",lwd=2)
  legend("topleft",legend=c("Species presences","Tolerance ranges"),pch=c(19,NA),
         bty="n",lty=c(0,1),col=c(sp.col,"gold"),lwd=2)
}
# TDL## wow, there is alot going on here.  May not want to specific the columns by position.
# Column names may work better for this types of funciton

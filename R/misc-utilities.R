#' Min-Max Normalization of a Vector of Numbers
#'
#' I have typically used this when evaluating between choosing a particular reference segment on the basis of multiple metrics, e.g. distance, bearing similarity, length overlap.
#'
#' @param x Numeric vector
#'
#' @return Normalized vector
#' @export
#'
#' @examples mm_norm(c(1,2,3,4,5,6,7,8,9,10))
mm_norm = function(x){

  x_max = max(x,na.rm = TRUE)
  x_min = min(x,na.rm = TRUE)

  if(x_max == x_min){
    return(rep(1,length(x)))
  }else{
    normed = (x - x_min)/(x_max-x_min)
    normed[is.na(normed)] = 1
    return(normed)
  }

}

#' Get sharepoint directory for user running this function
#'
#' @return Your sharepoint directory for appending further paths to
#' @export
get_sharepoint_dir  = function(){
  renv = Sys.getenv()
  user_dir = stringr::str_replace_all(renv['USERPROFILE'],stringr::fixed('\\'),'/')
  sharepoint_dir = paste0(user_dir,'/Perkins and Will')
  return(sharepoint_dir)
}

#' Replance NaN with NA
#'
#' @param x Vector (can be used for a data frame column)
#'
#' @return Vector with NaNs replaced with NAs
#' @export
#'
#' @examples replace_nan(c(0, 1, 2, NaN))
replace_nan = function(x){
  new = x
  new[is.nan(new)]=NA
  return(new)
}

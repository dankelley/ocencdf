# vim:textwidth=80:expandtab:shiftwidth=4:softtabstop=4

#' ocencdf: Save `oce` Objects in NetCDF Files
#'
#' @description
#' The ocencdf package provides functions for saving objects
#' created by the oce package into NetCDF files.
#'
#' Objects of the `oce` have two slots, 'data' and 'metadata',
#' that are very significant to the `ocencdf` package.
#'
#' The 'data' contents take the form of vector and array data, and hence can be
#' stored in the DATA section of the NetCDF file.  Since these items can be
#' accessed easily by standard means of reading NetCDF files, nothing special need
#' be explained here.
#'
#' However, the 'metadata' contents of oce objects take the form of a tree-like
#' structure that cannot be stored in the DATA section of a NetCDF file, which is
#' designed for vectors and arrays.  For this reason, the 'metadata' contents are
#' stored in a global attribute of the NetCDF file, named "metadata".  It is in
#' JSON format, for ease of parsing in many languages. In R, for example, this can
#' be converted using [json2metadata()]. This
#' does more than just translate, however, because JSON format lacks the ability to
#' handle some R structures (notably, the 'expression' class).  The following
#' explains the changes that are involved in expanding the JSON contents into a
#' 'metadata' object that mimics that used in the 'oce' package.
#'
#' 1. In 'oce', the 'units' structure is broken down into variables (e.g.
#'    'temperature'), and for each variable there are two quantities stored, one
#'    called 'units' and the other called 'scale'.  Both are textual in the JSON
#'    representation, which does not match the 'oce' format, which uses the
#'    'expression' class for the first of these. In R, the expression may be
#'    recovered with the 'parse()' function; similar functions exist in
#'    other analysis languages, if there is a need to e.g. label plot
#'    axes with subscripts, etc.
#'
#' 2. For 'adp' data created from Teledyne-RDI files, the metadata contains a
#'    matrix named 'codes' that are in 'raw' (byte-level) format.  These are
#'    converted to integers for the JSON representation, and so if there is a need
#'    to get the original values, a conversion will be required.
#'
#' @docType package
#' @keywords internal
"_PACKAGE"
NULL

# vim:textwidth=80:expandtab:shiftwidth=4:softtabstop=4

#' ocencdf: A Package to save Oce objects to Netcdf files.
#'
#' @description
#' The ocencdf package provides functions for saving objects
#' created by the oce package into Netcdf files.
#'
#'
#' Oce objects have two main components, named 'data' and 'metadata'.
#'
#' The 'data' contents take the form of vector and array data, and hence can be
#' stored in the DATA section of the Netcdf file.
#'
#' However, the 'metadata' contents take the form of a tree-like structure that
#' cannot be stored in the DATA section of a Netcdf file, and are instead stored in
#' a global attribute named "metadata", in a JSON format. In R, this can be
#' converted using the json2metadata() of the ocencdf package. Most other analysis
#' languages will have a library for converting JSON text into native objects in
#' those languages, and this will recover most of the original contents.  However,
#' the following steps must also be undertaken, to get a full translation.
#'
#' 1. The 'units' structure is broken down into variables (e.g. 'temperature'), and
#'    for each variable there are two quantities stored, one called 'units' and the
#'    other called 'scale'.  Both are textual in the JSON, but the original
#'    contents in the oce objects stored 'units' as an R "expression".  In R, the
#'    original value is recovered by calling the 'parse()' function.  Other
#'    language elements may be available in other languages.  This conversion is
#'    only necessary if there is a desire to show units in a scientific
#'    representation, with superscripts, Greek letters, etc., as is done in Oce
#'    plotting functions.
#'
#' 2. For 'adp' data created from Teledyne-RDI files, the metadata contains a
#'    matrix of 'raw' (byte-level) values.  These have been converted to integers
#'    prior to storage, and so if there is a need to get the original values, a
#'    conversion will be required.  (Such a need seems unlikely unless the analyst
#'    is writing low-level code.)
#'
#' 3. Times are represented as character strings in JSON.  If there is a
#'    requirement that these be in native time format, a conversion will be
#'    required.  Examples of the names of such time items are: 'date', 'endTime',
#'    'startTime', and 'systemUploadTime'.
#' 
#' @docType package
#'
#' @name ocencdf
NULL

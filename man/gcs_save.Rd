% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/rsession.R
\name{gcs_save}
\alias{gcs_save}
\title{Save .RData objects to the Google Cloud}
\usage{
gcs_save(..., file, bucket = gcs_get_global_bucket(), envir = parent.frame())
}
\arguments{
\item{...}{The names of the objects to be saved (as symbols or character strings).}

\item{file}{The file name that will be uploaded (conventionally with file extension \code{.RData})}

\item{bucket}{Bucket to store objects in}

\item{envir}{Environment to search for objects to be saved}
}
\value{
The GCS object
}
\description{
Performs \link{save} then saves it to Google Cloud Storage.
}
\details{
For all session data use \link{gcs_save_image} instead.

\code{gcs_save(ob1, ob2, ob3, file = "mydata.RData")} will save the objects specified to an \code{.RData} file then save it to Cloud Storage, to be loaded later using \link{gcs_load}.

For any other use, its better to use \link{gcs_upload} and \link{gcs_get_object} instead.

Restore the R objects using \code{gcs_load(bucket = "your_bucket")}

This will overwrite any data within your local environment with the same name.
}
\seealso{
Other R session data functions: 
\code{\link{gcs_load}()},
\code{\link{gcs_save_all}()},
\code{\link{gcs_save_image}()},
\code{\link{gcs_source}()}
}
\concept{R session data functions}

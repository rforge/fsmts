% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utils.R
\name{fsSparsity}
\alias{fsSparsity}
\title{Calculating sparsity of a feature set}
\usage{
fsSparsity(feature.set)
}
\arguments{
\item{feature.set}{a matrix that contains feature weights.}
}
\value{
returns a share of non-zero components in the feature set
}
\description{
\code{fsSparsity} calculates the sparsity (share of non-zero components) of the feature set
}
\examples{

# Load traffic data
data(traffic.mini)

# Scaling is sometimes useful for feature selection
# Exclude the first column - it contains timestamps
data <- scale(traffic.mini$data[,-1])

mCCF<-fsMTS(data, max.lag=3, method="CCF")
fsSparsity(cutoff(mCCF,0.3))

}

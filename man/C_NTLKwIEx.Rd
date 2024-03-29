% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/C_NTLKwIEx.R
\name{C_NTLKwIEx}
\alias{C_NTLKwIEx}
\title{Cumulative Distribution Function (CDF) of the NTLKwIEx distribution}
\usage{
C_NTLKwIEx(x, teta, alpha, a, b, m)
}
\arguments{
\item{x}{Value up to which to calculate the CDF.}

\item{teta}{Parameter teta of the distribution representing the distribution of the inverse exponential component.}

\item{alpha}{Parameter alpha of the distribution representing the distribution of the new proposal component.}

\item{a}{Parameter a of the distribution representing the distribution of the Kumaraswamy component.}

\item{b}{Parameter b of the distribution representing the distribution of the Kumaraswamy component.}

\item{m}{Parameter m of the distribution representing the distribution of the Topp Leone component.}
}
\value{
Value of the CDF for the NTLKwIEx distribution evaluated at x
}
\description{
This function calculates the Cumulative density function (CDF) of the NTLKwIEx distribution.
}
\details{
It takes parameters x, teta, alpha, a, b, and m, and returns the CDF value at x based on these parameters.
The formula used for the calculation is provided in the documentation header.
The Cumulative Distribution Function (CDF) of the NTLKwIEx distribution is defined as:
\deqn{
  F(x;a,b,m,\alpha,\theta) = \left[ 1-\left(1-K(x,\xi)^{a \alpha^{K(x,\xi)}}\right)^{2b} \right]^{m}
}
where \eqn{\alpha , a , b, m, \theta > 0}.
}

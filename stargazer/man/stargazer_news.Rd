\name{stargazer_news}
\alias{stargazer news}

%- Also NEED an '\alias' for EACH other topic documented here.
\title{What's new?}
%%  ~~function to do ... ~~
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
This page summarizes the models, features, and bug fixes that were introduced in each release of \code{stargazer}. Please note that I am always looking for comments and suggestions. Do not hesitate to contact me at mhlavac [at] alumni.princeton.edu.
}

\section{What's new?}{
v. 5.2.3:\cr
\cr
- very minor update: logical coercion of length longer than one removed\cr
v. 5.2.2:\cr
\cr
- very minor update: removed instances in code where if/while statement is used with a condition of length greater than one\cr
\cr
v. 5.2.1:\cr
\cr
- very minor update: updated author contact information\cr
\cr
v. 5.2:\cr
\cr
- New models:\cr
-- arima (stats) \cr
-- censReg (censReg) \cr
-- garchFit (fGarch) \cr
-- glmrob (robustbase) \cr
-- lme (nlme) \cr
-- nlme (nlme) \cr
-- mnlogit (mnlogit) \cr
-- pgmm (plm) \cr
\cr
- New features:\cr
-- decimal mark (argument \code{decimal.mark}) now gets automatically updated in notes (argument \code{notes}) \cr
\cr
- Fixed bugs:\cr
-- felm (lfe) working again \cr
-- fixed formatting issues that led to an occasional inability to produce ASCII text output \cr
-- much faster output of multi-model tables with many omitted variables \cr
-- negative numbers are now properly displayed when \code{initial.zero} is set to \code{FALSE} \cr
-- fixed minor bug that occasionally led to a misalignment of additional lines (argument \code{add.lines}) \cr
-- lm() and glm() models now report AIC and BIC \cr
-- underscores are now properly formatted in column and dependent variable labels\cr
-- fixed an issue with the mlogit model that led LR tests to crash the output when no intercept was present\cr
\cr
v. 5.1:\cr
\cr
- New features:\cr
-- the CRAN description now lists packages whose usefulness is enhanced by \code{stargazer}\cr
\cr
- Fixed bugs\cr
-- labels that indicate whether variables have been omitted from the model are now correctly assigned (argument \code{omit.labels})\cr
-- argument \code{summary.stat} now functions properly\cr
-- \code{out.header} and \code{header} can both be set to false at the same time\cr
-- minor formatting changes\cr
\cr
v. 5.0: \cr
\cr
- New models: \cr
-- felm (lfe) \cr
\cr
- New features: \cr
-- ability to produce HTML/CSS output (argument \code{type}) \cr
-- vector and matrix objects are now acceptable (argument \code{...}) \cr
-- additional lines can be included in the tables (argument \code{add.lines}) \cr
-- table layout is now fully customizable (arguments \code{table.layout} and \code{omit.table.layout}) \cr
-- choice over which summary statistics should be reported (arguments \code{summary.stat} and \code{omit.summary.stat}) \cr
-- users can choose whether and the order in which to report coefficients, standard errors, confidence intervals, test statistics and p-values (argument \code{report}) \cr
-- object names can be reported above each column (argument \code{object.names}) \cr
-- option to toggle the reporting of dependent variables and model names across several columns (argument \code{multicolumn}) \cr
-- reporting of model statistics' degrees of freedom can now be suppressed (argument \code{df}) \cr
-- users can now include more than three statistical significance cutoffs (argument \code{star.cutoffs}) \cr
-- regression, summary statistic and data frame table can now be flipped (argument \code{flip}) \cr
-- colnames and rownames can be included in, or excluded from, data frame content tables (arguments \code{colnames} and \code{rownames}) \cr
-- argument \code{summary} can now have a different value for each table within a single \code{stargazer()} call \cr
-- if arguments \code{coef}, \code{se}, \code{t}, \code{p}, \code{ci.custom} and \code{add.lines} only contain a single vector, they will be accepted as though they were a list containing the same vector \cr
\cr
- Fixed bugs: \cr
-- fixed bug that caused the number of observations to be misreported in complicated models \cr
-- fixed bug that led to the omission of coefficients from output when multiple models had a single regressor \cr
-- \code{notes} now working well for summary statistics and data frame tables \cr
-- font size can now be change even if floating environment is not used \cr
-- fixed compatability issues with latest version of Zelig\cr
-- argument \code{apply.ci} now works properly \cr
-- minor formatting changes \cr
\cr
v. 4.5.3: \cr
\cr
- New features: \cr
-- model objects can now be passed to stargazer in a list (argument \code{...}) \cr
\cr
- Fixed bugs: \cr
-- fixed problem with printing negative numbers in data frame and summary statistics tables \cr
-- corrected formatting of column widths in ASCII text output \cr
-- minor bug with lme4 covariate names corrected \cr
\cr
v. 4.5.2: \cr
\cr
- Fixed bugs: \cr
-- much faster output \cr
-- now compatible with the updated lme4 package \cr
-- corrected a bug that prevented users from including multiple models with a single covariate + intercept \cr
-- underscores now print correctly in ASCII text output \cr
\cr
v. 4.5.1: \cr
\cr
- Fixed bugs: \cr
-- fixed a bug involving the calculation of t-statistics from user-given coefficients and standard errors \cr
\cr
v. 4.5: \cr
\cr
- New models: \cr
-- binaryChoice (sampleSelection) \cr
-- brglm (brglm) \cr
-- coeftest (lmtest) \cr
-- heckit (sampleSelection) \cr
-- maBina (erer) \cr
-- mclogit (mclogit) \cr
-- mlogit (mlogit) \cr
-- selection (sampleSelection) \cr
\cr
- New features: \cr
-- explanatory variables can be ordered in customizable ways (argument \code{order}) \cr
-- custom functions can now be applied to coefficients (argument \code{apply.coef}), standard errors (argument \code{apply.se}), test statistics (argument \code{apply.t}), p-values (argument \code{apply.p}), and confidence intervals (argument \code{apply.ci}) \cr
-- keep explanatory variables and statistics based on regular expressions (argument \code{keep} and \code{keep.stat}) \cr
-- users can customize confidence intervals (argument \code{custom.ci}) \cr
-- ability to have both standard errors and confidence intervals in the same table (argument \code{ci} is now a logical vector) \cr
-- different confidence levels for each individual column (argument \code{ci.level} is now a vector) \cr
-- ability to omit and keep variables based on their position in the table\cr
\cr
- Fixed bugs: \cr
-- citation and change log updates \cr
-- minor formatting changes \cr
\cr
v. 4.0: \cr
\cr
- New models: \cr
-- aftreg (eha) \cr
-- bj (rms) \cr
-- coxreg (eha) \cr
-- cph (rms) \cr
-- dynlm (dynlm) \cr
-- errorsarlm (spdep) \cr
-- Glm (rms) \cr
-- Gls (rms) \cr
-- gmm (gmm) \cr
-- hetglm (glmx) \cr
-- lrm (rms) \cr
-- mlreg (eha) \cr
-- lagsarlm (spdep) \cr
-- ols (rms) \cr
-- phreg (eha) \cr
-- psm (rms) \cr
-- rem.dyad (relevent) \cr
-- rq (quantreg) \cr
-- Rq (rms) \cr
-- weibreg (eha) \cr
\cr
- New features: \cr
-- the package can produce ASCII text output, in addition to LaTeX code (argument \code{type}) \cr
-- output directly to .tex or .txt files (argument \code{out}) \cr
-- column labels (arguments \code{column.labels} and \code{column.separate}) \cr
-- confidence intervals (arguments \code{ci}, \code{ci.level} and \code{ci.separator})\cr
-- coefficients and standard errors/confidence intervals can now be reported in the same row (argument \code{single.row}) \cr
-- users can choose to omit all empty lines in a table (argument \code{no.space}) \cr
-- notes can now be appended to, rather than always replace, the default notes for a given style (argument \code{notes.append}) \cr
-- ability to customize the dependent variable caption (argument \code{dep.var.caption}) \cr
-- font size can now be changed (argument \code{font.size}) \cr
-- comments header (with package and author name, version, date and time) can now be suppressed (argument \code{header}) \cr
-- ability to change or disable the floating environment (arguments \code{float} and \code{float.env}) \cr
-- table placement settings (argument \code{table.placement}) \cr
-- customization of column spacing (argument \code{column.sep.width}) \cr
-- perl-compatible regular expressions (argument \code{perl}) \cr
\cr
- Fixed bugs: \cr
-- all columns are now displayed for \code{multinom} objects \cr
-- better positioning of tables in the LaTeX document\cr
-- minor formatting changes \cr
\cr
v. 3.0.1: \cr
\cr
- Fixed bugs: \cr
-- corrected an issue that led to problems when printing data frames with multiple decimal places\cr
-- some reporting and formatting changes to summary statistics and data frame tables \cr
\cr
v. 3.0: \cr
\cr
- New models: \cr
-- clm (ordinal) \cr
-- clogit (survival) \cr
-- ergm (ergm) \cr
-- glmer (lme4) \cr
-- gls (nlme) \cr
-- lmer (lme4) \cr
-- lmrob (robustbase) \cr
-- nlmer (lme4) \cr
-- pmg (plm) \cr
-- rlm (MASS) \cr
\cr
- New features: \cr
-- users can customize coefficients (argument \code{coef}), standard errors (\code{se}), test statistics (\code{t}), and p-values (\code{p}) \cr
-- automatic calculation of z-scores and p-values when the user supplies custom standard errors (can be toggled using arguments \code{t.auto} and \code{p.auto}) \cr
-- ability to set \code{\\label{}} markers in TeX for each table (using argument \code{label}) \cr
-- summary statistics table can now report logical (i.e., dummy, indicator) variables as if they had values of 0 (corresponding to \code{FALSE}) and 1 (\code{TRUE}). See argument \code{summary.logical}. \cr
-- user can choose between rounding to a set number of decimal places (argument \code{digits}) and reporting all available decimal places (by setting \code{digits} equal to \code{NA}) \cr
-- can omit all test statistics \cr
\cr
- Fixed bugs: \cr
-- package runs much faster \cr
-- corrected p-values for polr() and zelig oprobit, ologit models \cr
-- coxph now, by default, reports robust standard errors, for consistency with its summary() output \cr
-- multinom models now report the first set of coefficients (when multiple sets are present), instead of producing an error\cr
-- singularities in model regression no longer disrupt the output \cr
-- model formulas can now be symbols \cr
-- no more warnings when creating summary statistics tables \cr
-- minor formatting changes \cr
\cr
v. 2.0.1: \cr
\cr
- Fixed bugs: \cr
-- stargazer now reports the correct number of observations for plm() models \cr
\cr
v. 2.0: \cr
\cr
- New models: \cr
-- betareg (betareg) \cr
-- hurdle (pscl) \cr
-- ivreg (AER) \cr
-- multinom (nnet) \cr
-- plm (plm) \cr
-- tobit (AER) \cr
-- zeroinfl (pscl) \cr
    \cr
- New features: \cr
-- direct output of data frames into LaTeX (\code{summary = FALSE}) \cr
-- ability to omit the reporting of selected statistics (using argument \code{omit.stat})
-- alignment of columns at the decimal mark (\code{align = TRUE}) \cr
-- automatic coordination of star cutoffs and values in regression table notes \cr
\cr
- Fixed bugs: \cr
-- argument \code{digits} now works properly \cr
-- \code{stargazer} updated to reflect, and work properly with, recent changes to \code{Zelig} \cr
-- variable names can now contain dollar signs and underscores \cr
-- some minor formatting fixes \cr
}

% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ package }
\keyword{ models }
\keyword{ regression }
\keyword{ nonlinear }
\keyword{ univar }
\keyword{ interface }
\keyword{ programming }
\keyword{ IO }% __ONLY ONE__ keyword per line

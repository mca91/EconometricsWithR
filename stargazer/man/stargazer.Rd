\name{stargazer}
\alias{stargazer}
\alias{stargazer-package}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{stargazer
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
The \code{stargazer} command produces LaTeX code, HTML code and ASCII text for well-formatted tables that hold regression analysis results from several models side-by-side. It can also output summary statistics and data frame content. \code{stargazer} supports a large number model objects from a variety of packages. Please see \link{stargazer models}.
}
\usage{
stargazer(  ..., 
            type = "latex", title = "", style = "default", 
            summary = NULL, out = NULL, out.header = FALSE,
            column.labels = NULL, column.separate = NULL,
            covariate.labels = NULL, dep.var.caption = NULL,
            dep.var.labels = NULL, dep.var.labels.include = TRUE,
            align = FALSE, 
            coef = NULL, se = NULL, t = NULL, p = NULL,
            t.auto = TRUE, p.auto = TRUE,
            ci = FALSE, ci.custom = NULL,
            ci.level = 0.95, ci.separator = NULL,
            add.lines = NULL, 
            apply.coef = NULL, apply.se = NULL, 
            apply.t = NULL, apply.p = NULL, apply.ci = NULL,
            colnames = NULL,
            column.sep.width = "5pt",
            decimal.mark = NULL, df = TRUE,
            digit.separate = NULL, digit.separator = NULL,
            digits = NULL, digits.extra = NULL, flip = FALSE,
            float = TRUE, float.env="table",
            font.size = NULL, header = TRUE,
            initial.zero = NULL, 
            intercept.bottom = TRUE, intercept.top = FALSE, 
            keep = NULL, keep.stat = NULL,
            label = "", model.names = NULL, 
            model.numbers = NULL, multicolumn = TRUE,
            no.space = NULL,
            notes = NULL, notes.align = NULL, 
            notes.append = TRUE, notes.label = NULL, 
            object.names = FALSE,
            omit = NULL, omit.labels = NULL, 
            omit.stat = NULL, omit.summary.stat = NULL,
            omit.table.layout = NULL,
            omit.yes.no = c("Yes", "No"), 
            order = NULL, ord.intercepts = FALSE, 
            perl = FALSE, report = NULL, rownames = NULL,
            rq.se = "nid", selection.equation = FALSE, 
            single.row = FALSE,
            star.char = NULL, star.cutoffs = NULL, 
            suppress.errors = FALSE, 
            table.layout = NULL, table.placement = "!htbp",
            zero.component = FALSE, 
            summary.logical = TRUE, summary.stat = NULL,
            nobs = TRUE, mean.sd = TRUE, min.max = TRUE, 
            median = FALSE, iqr = FALSE )
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{\dots}{one or more model objects (for regression analysis tables) or data frames/vectors/matrices (for summary statistics, or direct output of content). They can also be included as lists (or even lists within lists).
%%     ~~Describe \code{\dots} here~~
}
  \item{type}{a character vector that specifies what type of output the command should produce. The possible values are \code{"latex"} (default) for LaTeX code, \code{"html"} for HTML/CSS code, \code{"text"} for ASCII text output. 
%%     ~~Describe \code{type} here~~
}
  \item{title}{a character vector with titles for the tables.
%%     ~~Describe \code{title} here~~
}
  \item{style}{a character string that specifies what style, typically designed to resemble an existing academic journal, should be used in producing the tables. This argument is not case-sensitive. See \link{list of supported styles}. 
%%     ~~Describe \code{style} here~~
}

  \item{summary}{a logical value indicating whether the package should output a summary statistics table when given a data frame. If \code{FALSE}, the package will instead output the contents of the data frame.}
  
  \item{out}{a character vector that contains the path(s) of output files. Depending on the file extension (.tex, .txt, .htm or .html), either a LaTeX/HTML source file or an ASCII text output file will be produced. For any other file extension, the value of the \code{type} argument will determine the type of output file.}
  
  \item{out.header}{a logical value that indicates whether the LaTeX or HTML file output should contain a code header (if TRUE) or just the chunk of code that creates the output (if FALSE).}

\item{column.labels}{a character vector of labels for columns in regression tables. Their layout, in terms of the number of columns associated with each label, is given by the argument \code{column.separate}.}

\item{column.separate}{a numeric vector that specifies how \code{column.labels} should be laid out across regression table columns. A value of \code{c(2, 1, 3)}, for instance, will apply the first label to the two first columns, the second label to the third column, and the third label will apply to the following three columns (i.e., columns number four, five and six). If the argument's value is \code{NULL} or the regression table contains more columns than are referred to in \code{column.separate}, a value of 1 is assumed for each "excess" column label.}

\item{covariate.labels}{a character vector of labels for covariates in regression tables. A value of \code{NA} for any element means that \code{stargazer} will print the corresponding variable name. In the default case of \code{NULL}, variable names are printed.%%     ~~Describe \code{covariate.labels} here~~
}

\item{dep.var.caption}{a character vector that specifies the caption to appear above dependent variable labels. A value of \code{NULL} denotes the default caption for the chosen \code{style}. An empty string (i.e., \code{""}) will lead \code{stargazer} to omit the caption.
%%     ~~Describe \code{dep.var.labels} here~~
}

\item{dep.var.labels}{a character vector of labels for the dependent variables in regression tables. A value of \code{NA} for any element means that \code{stargazer} will print the corresponding variable name. In the default case of \code{NULL}, variable names are printed.%%     ~~Describe \code{dep.var.labels} here~~
}

\item{dep.var.labels.include}{a logical value that toggles whether dependent variable labels will be included in the regression table.}

  \item{align}{a logical value indicating whether numeric values in the same column should be aligned at the decimal mark in LaTeX output. Requires \code{\\usepackage{dcolumn}} in LaTeX preamble.}
  
    \item{coef}{a list of numeric vectors that will replace the default coefficient values for each model. Element names will be used to match coefficients to individual covariates, and should therefore match covariate names. A \code{NULL} vector indicates that, for a given model, the default set of coefficients should be used. By contrast, an \code{NA} vector means that all of the model's coefficients should be left blank.}
    
    \item{se}{a list of numeric vectors that will replace the default coefficient values for each model. Behaves exactly like the argument \code{coef}.}
    
    \item{t}{a list of numeric vectors that will replace the default test statistics (e.g., t-scores, or z-scores) for each model. Like \code{coef} and \code{se}, test statistics are matched to covariates by their element names.}
    
    \item{p}{a list of numeric vectors that will replace the default p-values for each model. Matched by element names. These will form the basis of decisions about significance stars.}
    
  \item{t.auto}{a logical value that indicates whether \code{stargazer} should calculate the test statistics (i.e., the z-scores) automatically if coefficients or standard errors are supplied by the user (from arguments \code{coef} and \code{se}) or modified by a function (from arguments \code{apply.coef} or \code{apply.se}). If \code{FALSE}, the package will use model's default values if \code{t} is \code{NULL}. }
 
  \item{p.auto}{a logical value that indicates whether \code{stargazer} should calculate the p-values, using the standard normal distribution, if coefficients or standard errors are supplied by the user (from arguments \code{coef} and \code{se}) or modified by a function (from arguments \code{apply.coef} or \code{apply.se}). If \code{FALSE}, the package will use model's default values if \code{p} is \code{NULL}. }
  
  \item{ci}{a logical vector that indicates, for each column, whether \code{stargazer} should, in regression tables, replace standard errors by confidence intervals. If the value is \code{NA} or unspecified, then the value from the last preceding specified column is used.}
  
    \item{ci.custom}{a list of two-column numeric matrices that will replace the default confidence intervals for each model. The first and second columns represent the lower and the upper bounds, respectively. Matched by element names.}
    
  \item{ci.level}{a numeric vector that specifies, for each column, the confidence level to be used in regression tables when argument \code{ci} is set to \code{TRUE}. By default, \code{stargazer} will report 95 percent confidence intervals. If the value is \code{NA} or unspecified, then the value from the last preceding specified column is used.}
    
  \item{ci.separator}{a character string that will serve as the separator between the lower and upper bounds of reported confidence intervals.}
  
  \item{add.lines}{a list of vectors (one vector per line) containing additional lines to be included in the table. Each element of the listed vectors will be put into a separate column.}
  
  \item{apply.coef}{a function that will be applied to the coefficients.}
  \item{apply.se}{a function that will be applied to the standard errors.}
  \item{apply.t}{a function that will be applied to the test statistics.}
  \item{apply.p}{a function that will be applied to the p-values.}
  \item{apply.ci}{a function that will be applied to the lower and upper bounds of the confidence intervals.}
  
  \item{colnames}{a logical value that toggles column names on or off when printing data frames, vectors or matrices.}
  
  \item{column.sep.width}{a character string that specifies, in LaTeX code, the width of the space that separates columns in LaTeX tables. The default value is \code{"5pt"}.}
  
  \item{decimal.mark}{a character string that will serve as the decimal mark. For instance, the string \code{","} will represent decimal commas, while \code{"."} means tables will use decimal points.}

  \item{df}{a logical value that indicates whether the degrees of freedom of model statistics should be reported.}

  \item{digit.separate}{a numeric vector that indicates where digit separators should be placed. The first element of the vector indicates the number of digits (counted from the decimal mark to the left) that will be separated. The second element indicates the number of digits that will be separated from that 'first' separator, and so on. A value of \code{3} corresponds to a thousands separator, while a value of \code{0} indicates no separation. Alternatively, \code{digit.separate} can be one of the following character strings: \code{"lakh"} (equivalent to \code{c(4,3)}), \code{"china"} or \code{"japan"} (both equivalent to a value of \code{4}).
%%     ~~Describe \code{digit.separate} here~~
}
  \item{digit.separator}{a character string that will serve as the digit (e.g., thousands) separator. Commonly used strings include \code{","} for a comma separator, \code{" "} for a single space separator, and \code{""} for no separation.
%%     ~~Describe \code{digit.separator} here~~
}
  \item{digits}{an integer that indicates how many decimal places should be used. A value of \code{NA} indicates that no rounding should be done at all, and that all available decimal places should be reported.
%%     ~~Describe \code{digits} here~~
}
  \item{digits.extra}{an integer indicating the maximum number of additional decimal places to be used if a number, rounded to \code{digits} decimal places, is equal to zero.
%%     ~~Describe \code{digits.extra} here~~
}
  \item{flip}{a logical value that flips the vertical and horizontal axes when printing summary statistic tables or vector, matrix and data frame content.}
  
  \item{float}{a logical value that indicates whether the resulting table will be a floating table (set off, for instance, by \code{\\begin{table}} and \code{\\end{table}}).}
  
  \item{float.env}{a character string that specifies the floating environment of the resulting LaTeX table (when argument \code{float} is set to \code{TRUE}). Possible values are \code{"table"} (default), \code{"table*"} and \code{"sidewaystable"} (requires \code{\\usepackage{dcolumn}} in LaTeX preamble). }

  \item{font.size}{a character string that specifies the font size used in the table. The font can be one of the following: \code{"tiny"}, \code{"scriptsize"}, \code{"footnotesize"}, \code{"small"}, \code{"normalsize"}, \code{"large"}, \code{"Large"}, \code{"LARGE"}, \code{"huge"}, \code{"Huge"}. If \code{NULL} (default), no particular font is imposed.
%%     ~~Describe \code{font.size} here~~
}
  \item{header}{a logical value indicating whether a header (containing the name and version of the package, the author's name and contact information, and the date and time of table creation) should appear in comments at the beginning of the LaTeX code.
%%     ~~Describe \code{header} here~~
}
  \item{initial.zero}{a logical value indicating whether an initial zero should be printed before the decimal mark if a number is between 0 and 1. 
%%     ~~Describe \code{initial.zero} here~~
}
  \item{intercept.bottom}{a logical value indicating whether the intercept (or constant) coefficients should be on the bottom of the table.
%%     ~~Describe \code{intercept.top} here~~
}
  \item{intercept.top}{a logical value indicating whether the intercept (or constant) coefficients should be on the top of the table.
%%     ~~Describe \code{intercept.top} here~~
}
  \item{keep}{a vector of regular expressions that specifies which of the explanatory variables should be kept in the table. Alternatively, this argument can be a numeric vector whose elements indicate which variables (from top to bottom, or left to right) should be kept. The default value of \code{NULL} means that all variables will be kept.
%%     ~~Describe \code{omit} here~~
}
  \item{keep.stat}{a character vector that specifies which model statistics should be kept in the regression table output. For instance \code{keep.stat = c("n","ll")} will produce a table that only includes statistics for the number of observations and log likelihood. See the \link{list of statistic codes}. This argument is not case-sensitive.%%     ~~Describe \code{omit.stat} here~~
} 
  \item{label}{a character string containing the \code{\\label{}} TeX markers for the tables.
%%     ~~Describe \code{intercept.top} here~~
}
 
 \item{model.names}{a logical value indicating whether model names (e.g., "OLS" or "probit") should be included in the table.
%%     ~~Describe \code{model.names} here~~
}
 \item{model.numbers}{a logical value indicating whether models should be numbered. No number is used whenever a regression table includes only one model.
%%     ~~Describe \code{model.numbers} here~~
}
 \item{multicolumn}{a logical value indicating whether dependent variables and model names (e.g., "OLS" or "probit") should be reported across several columns if they remain identical.
%%     ~~Describe \code{model.numbers} here~~
}

  \item{no.space}{a logical value indicating whether all empty lines should be removed from the table.
%%     ~~Describe \code{no.space} here~~
}

  \item{notes}{a character vector containing notes to be included below the table. The character strings can include special substrings that will be replaced by the corresponding cutoffs for statistical significance 'stars': \code{[*]}, \code{[**]}, and \code{[***]} will be replaced by the cutoffs, in percentage terms, for one, two and three 'stars,' respectively (e.g., 10, 5, and 1). Similarly, \code{[0.*]}, \code{[0.**]} and \code{[0.***]} will be replaced by the numeric value of cutoffs for one, two and three 'stars' (e.g., 0.1, 0.05, and 0.01). \code{[.*]}, \code{[.**]} and \code{[.***]} will omit the leading zeros (e.g., .1, .05, .01).
%%     ~~Describe \code{notes} here~~
}

  \item{notes.align}{a character string that specifies how notes should be aligned under the table. One of three strings can be used: \code{"l"} for left alignment, \code{"r"} for right alignment, and \code{"c"} for centering. This argument is not case-sensitive.
%%     ~~Describe \code{notes.align} here~~
}
  \item{notes.append}{a logical value that indicates whether \code{notes} should be appended to the standard note(s) associated with the table's \code{style} (typically an explanation of significance cutoffs). If the argument's value is set to \code{FALSE}, the character strings provided in \code{notes} will replace any existing/default notes.
}
  \item{notes.label}{a character string containing a label for the notes section of the table.
%%     ~~Describe \code{notes.label} here~~
}
 \item{object.names}{a logical value indicating whether object names should be included in the table.
%%     ~~Describe \code{object.names} here~~
}
  \item{omit}{a vector of regular expressions that specifies which of the explanatory variables should be omitted from presentation in the table. Alternatively, this argument can be a numeric vector whose elements indicate which variables (from top to bottom, or left to right) should be omitted. This argument might be used, for instance, to exclude fixed effects dummies from being presented. The default value of \code{NULL} means that no variables will be excluded.
%%     ~~Describe \code{omit} here~~
}
  \item{omit.labels}{a character vector of labels that correspond to each of the regular expressions in \code{omit}, and that will be used in a sub-table that indicates whether variables have been omitted from a given model. \code{omit} and \code{omit.labels} must be equal in length.%%     ~~Describe \code{omit.labels} here~~
}
  \item{omit.stat}{a character vector that specifies which model statistics should be omitted from regression table output. For instance \code{omit.stat = c("ll","rsq")} will omit the log-likelihood and the R-squared statistics. See the \link{list of statistic codes}. This argument is not case-sensitive.%%     ~~Describe \code{omit.stat} here~~
}
  \item{omit.summary.stat}{a character vector that specifies which summary statistics should be omitted from summary statistics table output. See the \link{list of summary statistic codes}. This argument is not case-sensitive.%%     ~~Describe \code{omit.stat} here~~
}

  \item{omit.table.layout}{a character string that specifies which parts of the table should be omitted from the output. Each letter in the string indicates a particular part of the table, as specified by the \link{table layout characters}. For instance, \code{omit.table.layout = "sn"} will omit the model statistics and notes.} 
  \item{omit.yes.no}{a character vector of length 2 that contains the 'yes' and 'no' strings to indicate whether, in any specific model, variables were omitted from the table, as specified by \code{"omit"}.
%%     ~~Describe \code{omit.yes.no} here~~
}
 \item{order}{a vector of regular expressions (or of numerical indexes) that indicates the order in which variables will appear in the output.
%%     ~~Describe \code{order} here~~
}
 \item{ord.intercepts}{a logical value indicating whether intercepts for models with ordered dependent variables (such as ordered probit, or ordered logit) are included in the table.
%%     ~~Describe \code{ord.intercepts} here~~
}
 \item{perl}{a logical value indicating whether perl-compatible regular expressions should be used. If \code{FALSE}, the package will assume the default extended regular expressions.
%%     ~~Describe \code{perl} here~~
}
  \item{report}{a character string containing only elements of {\code{"v"}, \code{"c"}, \code{"s"},\code{"t"}, \code{"p"}, \code{"*"}} that determines whether, and in which order, variable names (\code{"v"}), coefficients (\code{"c"}), standard errors/confidence intervals (\code{"s"}), test statistics (\code{"t"}) and p-values (\code{"p"}) should be reported in regression tables. If one of the aforementioned letters is followed by an asterisk (\code{"*"}), significance stars will be reported next to the corresponding statistic.}
  \item{rownames}{a logical value that toggles row names on or off when printing data frames, vectors or matrices.}
  
  \item{rq.se}{a character string that specifies the method used to compute standard errors for \code{rq} (quantile regression) objects. Possible values are \code{"iid"}, \code{"nid"}, \code{"ker"} and \code{"boot"}. }
  
    \item{single.row}{a logical value that indicates whether regression and standard errors (or confidence intervals) should be reported on the same row. For convenience in formatting the resulting table, argument \code{no.space} is automatically set to \code{TRUE} when \code{single.row} is \code{TRUE}. }
  
    \item{selection.equation}{a logical value that indicates whether the selection equation (when argument is set to \code{TRUE}) or the outcome equation (default) will be reported for \code{heckit} and \code{selection} models from the package sampleSelection.}
    
  \item{star.char}{a character string to be used as the 'star' to denote statistical significance.
%%     ~~Describe \code{star.char} here~~
}
  \item{star.cutoffs}{a numeric vector that indicates the statistical signficance cutoffs for the statistical significance 'stars.' For elements with \code{NA} values, the corresponding 'star' will not be used.
%%     ~~Describe \code{star.cutoffs} here~~
}
  \item{suppress.errors}{a logical value that indicates whether \code{stargazer} should suppress the output of its error messages.}
  \item{table.layout}{a character string that specifies which parts of the table should be included in the output, in the order provided by the user. Each letter in the string indicates a particular part of the table, as specified by the \link{table layout characters}. For instance, \code{table.layout = "#tn"} will report the model numbers, coefficient table and notes only.}  
  \item{table.placement}{a character string containing only elements of {\code{"h"}, \code{"t"},\code{"b"}, \code{"p"}, \code{"!"}, \code{"H"}} that determines the table placement in its LaTeX floating environment.}
  \item{zero.component}{a logical value indicating whether to report coefficients for the \code{zero} component of \code{zeroinfl} and \code{hurdle} estimation results. If \code{FALSE}, the \code{count} component is displayed.
%%     ~~Describe \code{zero.component} here~~
}
 \item{summary.logical}{a logical value indicating whether logical variables should be reported in summary statistics table. If so, they will be treated as if  they had values of 0 (corresponding to \code{FALSE}) and 1 (\code{TRUE}). 
}
 \item{summary.stat}{a character vector that specifies which summary statistics should be included in the summary statistics table output. See the \link{list of summary statistic codes}. This argument is not case-sensitive.). 
}
  \item{nobs}{a logical value that toggles whether the number of observations (N) for each variable is shown in summary statistics tables.}
  \item{mean.sd}{a logical value that toggles whether variable means and standard deviations are shown in summary statistics tables.}
  \item{min.max}{a logical value that toggles whether variable minima and maxima are shown in summary statistics tables.}
  \item{median}{a logical value that toggles whether variable medians are shown in summary statistics tables.}
  \item{iqr}{a logical value that toggles whether the 25th and 75th percentiles for each variable are shown in summary statistics tables. ('iqr' stands for interquartile range.)}

}
\details{Arguments with a value of \code{NULL} will use the default settings of the requested \code{style}.
%%  ~~ If necessary, more details than the description above ~~
}
\value{
\code{stargazer} uses \code{cat()} to output LaTeX/HTML code or ASCII text for the table. To allow for further processing of this output, \code{stargazer} also returns the same output invisibly as a character vector. You can include the produced tables in your paper by inserting \code{stargazer} LaTeX output into your publication's TeX source. Alternatively, you can use the \code{out} argument to save the output in a .tex or .txt file.

To include \code{stargazer} tables in Microsoft Word documents (e.g., .doc or .docx), please follow the following procedure: Use the \code{out} argument to save output into an .htm or .html file. Open the resulting file in your web browser. Copy and paste the table from the web browser to your Microsoft Word document.
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\author{
%%  ~~who you are~~
Dr. Marek Hlavac < marek.hlavac at gmail.com > \cr
Social Policy Institute, Bratislava, Slovakia \cr
}
\section{Acknowledgments and New Features}{
I would like to thank everyone who has tested this package, or provided useful comments and suggestions. Please see \link{stargazer package acknowledgments}. \cr
\cr
See \link{stargazer news} for a list of new models and features in each release of \code{stargazer}.
}

\section{Please cite as:}{
Hlavac, Marek (2022). stargazer: Well-Formatted Regression and Summary Statistics Tables. R package version 5.2.3. https://CRAN.R-project.org/package=stargazer 
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\examples{
## create summary statistics table for 'attitude' data frame
stargazer(attitude)

## list the content of the data frame 'attitude'
stargazer(attitude, summary=FALSE)

##  2 OLS models
linear.1 <- lm(rating ~ complaints + privileges + learning 
                        + raises + critical, data=attitude)

linear.2 <- lm(rating ~ complaints + privileges + learning, data=attitude)

## create an indicator dependent variable, and run a probit model
 
attitude$high.rating <- (attitude$rating > 70)
probit.model <- glm(high.rating ~ learning + critical + advance, data=attitude,
                    family = binomial(link = "probit"))
 
stargazer(linear.1, linear.2, probit.model, title="Regression Results")

## report ASCII text for a table with 90 percent confidence
## intervals reported on the same row as coefficients
## and omitting F statistics and the residual standard error

stargazer(linear.1, linear.2, probit.model, type="text",
          title="Regression Results", single.row=TRUE,
          ci=TRUE, ci.level=0.9, omit.stat=c("f", "ser"))
          
### re-order the models and only keep explanatory
### variables that contain "complaints", "learning", 
### "raises" and "critical"; report these with standard
### errors, and put "learning" and "raises" before
### the other explanatory variables; of the summary
### statistics, only keep the number of observations

stargazer(probit.model, linear.1, linear.2, type="text",
          keep=c("complaints","learning","raises","critical"),
          keep.stat="n", order=c("learning", "raises"))

### apply a function to the coefficients and standard errors
### that will multiply them by ten; you can think of this
### as a change in units

multiply.by.10 <- function(x) (x * 10)

stargazer(probit.model, linear.1, linear.2,
          apply.coef=multiply.by.10, apply.se=multiply.by.10)
          
### print out HTML code for a correlation matrix

correlation.matrix <- cor(attitude)
stargazer(correlation.matrix, type="html")

% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
}
\keyword{ package }
\keyword{ models }
\keyword{ regression }
\keyword{ nonlinear }
\keyword{ univar }
\keyword{ interface }
\keyword{ programming }
\keyword{ IO }% __ONLY ONE__ keyword per line

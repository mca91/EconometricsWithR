<p align="center"><img align="center" src="https://github.com/mca91/EconometricsWithR/blob/master/docs/images/cover.png" width="30%" height="30%"></p>

## 📖 About the book

<p><img src="https://github.com/mca91/EconometricsWithR/blob/master/images/logo.png" alt="logo" align="right" width="20%" height="20%"> Beginners with little background in statistics and econometrics often have a hard time understanding the benefits of having programming skills for learning and applying Econometrics. <i>Introduction to Econometrics with R</i> is an interactive companion to the well-received textbook <i>Introduction to Econometrics</i> by James H. Stock and Mark W. Watson (2015). It gives a gentle introduction to the essentials of R programming and guides students in implementing the empirical applications presented throughout the textbook using the newly aquired skills. This is supported by interactive programming exercises generated with DataCamp Light and integration of interactive visualizations of central concepts which are based on the flexible JavaScript library D3.js.</p>

The book can be found here: [Introduction to Econometrics with R](https://www.econometrics-with-r.org/)

### 📦 How to download materials using the itewrpkg R package

It is straightforward to download and install the `itewrpkg` [metapackage](https://github.com/mca91/itewrpkg/) for the companion using `install_github()` from the `devtools` package. Run `install.packages("devtools")` if you are not sure whether `devtools` is installed. 

The following one-liner installs `itewrpkg`:

```
# install `itewrpkg`
devtools::install_github("mca91/itewrpkg")
```

Running the above command will also install all R packages which are required for reproducing the code examples presented throughout the book. Running `library(itewrpkg)` will load the package and all dependencies which makes it unnecessary to individually attach the packages introduced at the beginning of each chapter. This may take a few seconds but may be convenient if you are playing around with code chunks from various chapters.

The function `get_materials_itewr()` is intented as a convenience function for students working with the companion. It downloads up-to-date versions of all supplements to the book such as datasets and R codes from the [GitHub repository](https://github.com/mca91/EconometricsWithR) of the book and saves them to the current working directory (or a location of choice provided to the argument `dir`) according to the following structure:

- `<your_working_directory>/ITEWR/Rmds/` (.Rmd files)

- `<your_working_directory>/ITEWR/Data/` (datasets)

- `<your_working_directory>/ITEWR/Rcodes/` (R scripts, numbered by chapter)

Make sure to check your working directory using `getwd()`!
___

<a rel="license" href="http://creativecommons.org/licenses/by-nc-sa/4.0/"><img alt="Creative Commons License" style="border-width:0" src="https://mirrors.creativecommons.org/presskit/buttons/88x31/svg/by-nc-sa.eu.svg"/></a><br/>This work is licensed under a <a rel="license" href="http://creativecommons.org/licenses/by-nc-sa/4.0/">Creative Commons Attribution-NonCommercial-ShareAlike 4.0 International License</a>.

#### References
Stock, J., & Watson, M. (2015). *Introduction to Econometrics, Third Update, Global Edition*. Pearson Education Limited.

#### Notes

We are currently using a modified version of the `stargazer` package to compile LaTeX tables with long model names (there is an issue with the CRAN version for R>=4.2.0). The package can be found [here](https://gist.github.com/alexeyknorre/b0780836f4cec04d41a863a683f91b53).

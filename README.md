# Wearalyze <img src="app/static/logos/wearalyze.png" align="right" alt="Rhino logo" style="height: 140px;">
> _Analyze data from commonly used wearables: transparent, fast and without any data storage_

# About

Wearalyze is a Shiny app that supports patients, clinicians and researchers in the analysis of data from commonly used wearables. It is designed to be transparent, fast and without any data storage. 

⚠️ Wearalyze is currently in development and not yet ready for use.

# Running the app

This app is a [Rhino](https://github.com/Appsilon/rhino) project 🦏. Rhino makes use of [renv](https://rstudio.github.io/renv/index.html) to manage the project dependencies.

If you pull the project from GitHub, `renv` will automatically be downloaded and installed. 

If everything is ok, the output will look as follows:

```r
# Bootstrapping renv 1.0.2 ---------------------------------------------------
- Downloading renv ... OK
- Installing renv  ... OK

- Project '~/Documents/Hypebright BV/lab/wearalyze_test' loaded. [renv 1.0.2]
- One or more packages recorded in the lockfile are not installed.
- Use `renv::status()` for more details.
```

Once that's done, you can run the following code to install the required `wearalyze` R dependencies:

```r
renv::restore()
```

R dependencies will be installed with `install.packages()`.

After that (this can take some time!), you can run the app by executing the following code:

```r
runApp()
```

Or alternatively, you can head over to the `app.R` file and click the "Run App" button in RStudio. 

# For developers

## Using renv

Since this project is using `renv` to manage project dependencies, it is required to snapshot the project library after installing new packages. This can be done by running `renv::snapshot()` which will make changes to the `renv.lock` file. Make sure that the dependencies are recorded in the `dependencies.R` file! 

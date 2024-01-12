# Wearalyze <img src="app/static/logos/wearalyze.png" align="right" alt="Rhino logo" style="height: 140px;">
> _Analyze data from commonly used wearables: transparent, fast and without any data storage_

# About

Wearalyze is a Shiny app that supports patients, clinicians and researchers in the analysis of data from commonly used wearables. It is designed to be transparent, fast and without any data storage. 

⚠️ Wearalyze is currently in development and not yet ready for use.

# Running the app

This app is a [Rhino](https://github.com/Appsilon/rhino) project 🦏. Rhino makes use of [renv](https://rstudio.github.io/renv/index.html) to manage the project dependencies.

If you pull the project from GitHub, `renv` will automatically be downloaded and installed. 

It will also create a virtual environment for Python 🐍. This is required to run the Python scripts that are used to process the data for some devices. This also means that **Python needs to be installed** on your system. The version that is required is 3.12.0. You can check your Python version by running `python --version` in your terminal. You can also check your `PATH` with `Sys.getenv('PATH')` which would need to contain something liek `/Library/Frameworks/Python.framework/Versions/3.12/bin`. If you don't have Python installed, you can download it [here](https://www.python.org/downloads/).

If everything is ok, the output will look as follows:

```r
# Bootstrapping renv 1.0.2 ---------------------------------------------------
- Downloading renv ... OK
- Installing renv  ... OK

- Creating virtual environment 'renv-python-3.12' ... Done!
- Updating Python packages ... Done!
- Project '~/yourpath/wearalyze' loaded. [renv 1.0.2]
- One or more packages recorded in the lockfile are not installed.
- Use `renv::status()` for more details.
```

Once that's done, you can run the following code to install the required `wearalyze` R and Python dependencies:

```r
renv::restore()
```

R dependencies will be installed with `install.packages()` and Python dependencies will be installed with `pip install -r requirements.txt`.

After that (this can take some time!), you can run the app by executing the following code:

```r
runApp()
```

Or alternatively, you can head over to the app.R file and click the "Run App" button in RStudio. 

# For developers

## Using renv

Since this project is using `renv` to manage project dependencies, it is required to snapshot the project library after installing new packages. This can be done by running `renv::snapshot()` which will make changes to the `renv.lock` file. Make sure that the dependencies are recorded in the `dependencies.R` file! 

## Python dependencies

`renv` is also equipped to handle Python dependencies. These Python dependencies are stored in requirements.txt. This happens automatically when you call `renv::restore()`, so make sure to do so when you have worked on .py scripts. `renv` will automatically look for import statements in the .py files and add the required packages to the requirements.txt file.

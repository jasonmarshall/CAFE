# CAFE

The Continuum And Feature Extractor (aka, CAFE) is a software tool written in IDL for fitting astronomical SEDs comprised of spectral and photometric data.

CAFE has been tested to work with IDL 7.0.6 on macOS 10.13.5. Significant changes have been made to IDL since that release, so it may (or probably won't) run on a more modern version of IDL without some (hopefully) minor modifications.

## Requirements

The routine requires a number of IDL libraries to function:

- The IDL Astronomy User's Library (https://idlastro.gsfc.nasa.gov)
- The Coyote Library (http://www.idlcoyote.com/documents/programs.php)
- Markwardt IDL Library (http://www.physics.wisc.edu/~craigm/idl/idl.html)
- TeXtoIDL (http://physics.mnstate.edu/craig/textoidl)

Note 1: The versions of these libraries with which CAFE was originally written may be different than the versions that are currently available. As such, CAFE may call routines that have been retired from a library and are no longer included in its standard distribution (but are likely still available from the source website).

Note 2: At the heart of CAFE is the MPFit routine from the Markwardt IDL library (this is the least-squares fitting routine). For reference, this version of CAFE has been tested using MPFit version 1.75 (dated 2010/06/22).

## Setup

Files are saved to the output directory indicated in the system variable '!CAFE' set in the user's 'startup.pro' file. This system variable should be added before running CAFE by adding the following to 'startup.pro':

> DefSysV, '!CAFE', { In:<*insert input directory*>, Out:<*insert output dir*> }

Ensure that all libraries listed above as well as the CAFE distribution (and its included library sub-directories) are in your IDL path. And initialize the JAM and AstroLib IDL libraries from within your 'startup.pro' file via adding the following lines:

> JAM

> AstroLib

Once you've done this, you should see confirmation that each has been initialized during IDL startup at the command line.

Next, you need to create some data to fit. This is done by putting your spectral data into the correct CAFE-digestible format and locating it in the directory indicated by the system variable !CAFE.In (which you set earlier). Spectral data can be converted to the proper format via the routine 'sed_import.pro':

> SED_IMPORT, Object, z, Wave, Flux, EFlux

A full description of that routine and the input listed above (as well as additional optional input not listed above) is given in the 'sed_import.pro' file. 

Photometric data is read by CAFE from a ".sed.txt" file. An example photometry file for the galaxy NGC7714 is provided in the distribution (see "NGC7714.sed.txt"). Look at that file to get an idea for what data to include in the file and how it should be formatted. In fact, you would be wise to copy and alter the data from that file when making files for new sources to make sure you don't miss any of the important formatting characters (which are required by 'jam_readdata.pro' to parse the data). In that file, you'll find the columns:

- BAND: This can be any string name to identify the photometric data point. [Required]
- FILTER: This should be a string name of a photometric filter to perform synthetic photometry during the fit. A list of included filters can be found by looking in the "filters" sub-sub-directory of the distribution. [Optional]
- WAVE: Wavelength in microns. [Required]
- FLUX: Flux density in Jy. [Required]
- SIGMA: Uncertainty in Jy. [Required]
- LIMIT: 0 for a detection, 1 for an upper-limit, -1 for a lower-limit. [Required]
- FIT: Set to 0 to exclude the point from fits, or 1 to include. [Required]
- PLOT: Set to 0 to exclude the point from plots, or 1 to include (independent of FIT status). [Required]
- REFERENCE: Set to a string containing a reference for the data point. [Optional]

The object name and redshift should also be given as they are in NGC7714.sed.txt. All other data in that file is optional. Once spectral and/or photometric data is in the format described above and stored in the directory pointed to by !Cafe.In, you can read it using 'sed.pro' as follows:

>IDL> SED, Object, SED=SED, Phot=phot, Path_SED=!CAFE.In, Path_Phot=!CAFE.In
       
For example, to open and look at the data for NGC7714 (packaged with the distribution), you may enter:

>IDL> SED, 'NGC7714', SED=SED, Phot=phot, Path_SED=!CAFE.In, Path_Phot=!CAFE.In

In this case, 'SED' will contain a structure of the spectral data and 'phot' will contain a structure of photometric data. If you would like to produce a quick plot of the spectral and/or photometric data, you may do so using:

>IDL> SED, 'NGC7714', Path_SED=!CAFE.In, Path_Phot=!CAFE.In, /Plot_IRS, /Plot_Phot

Of course, you can also use your new shiny data files to perform a CAFE fit. But first, you need to open the file 'cafe.txt' in a text editor to set up CAFE for your fit. Details of what the various settings and parameters in that file do are provided in the file itself.

## How to Run

Each CAFE run is controlled by the set of parameters obtained from the file cafe.txt. This text file must be edited to set the components which will be used in the fitting model, the parameters for the fit, and information about the plots and other outputs of the fit. Once you've edited that file and have your data files in their correct format and location, you may run CAFE. For example, to run CAFE on NGC7714, enter:

>IDL> CAFE, 'NGC7714'

This will read the parameters and settings from 'cafe.txt' and run the fit. By default, plots showing the fits over both the IRS and full SED wavelength ranges will be shown on the display, and a number of files (including postscript versions of these plots) will be saved to the directory !CAFE.Out.

## Note

I've tried to remove all hard-coded paths and refer only to paths set via system variables (as described above). Alas, I may not have found every last one (laziness over the years has meant that a few hard-coded paths have slipped into the code).

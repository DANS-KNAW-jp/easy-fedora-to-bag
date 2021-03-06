easy-fedora2vault
==============
[![Build Status](https://travis-ci.org/DANS-KNAW/easy-fedora2vault.png?branch=master)](https://travis-ci.org/DANS-KNAW/easy-fedora2vault)

Index a bag store

SYNOPSIS
--------

    easy-fedora2vault {-d <dataset-id> | -i <dataset-ids-file>} [-o <staged-AIP-dir>] [-f <file-metadata>] [-u <depositor>] [-s] [-l <log-file>] <transformation>
    easy-fedora2vault -d <dataset-id> -o <staged-AIP-dir> <transformation>
    easy-fedora2vault -s -u <depositor> -i <dataset-ids-file> -o <staged-AIP-dir> -l <log-file> <transformation>

DESCRIPTION
-----------
Tool for exporting datasets from Fedora and constructing AIP-bags to be stored in the bag stores

ARGUMENTS
---------

     -d, --datasetId  <arg>    A single easy-dataset-id to be transformed. Use either this or the input-file
                               argument
     -u, --depositor  <arg>    The depositor for these datasets. If provided, only datasets from this depositor
                               are transformed.
     -f, --file-list  <arg>    A csv-file with metadata about extra files to be added to the AIP bag
     -i, --input-file  <arg>   File containing a newline-separated list of easy-dataset-ids to be transformed.
                               Use either this or the dataset-id argument
     -l, --log-file  <arg>     The name of the logfile in csv format. If not provided a file
                               easy-fedora2vault-<timestamp>.csv will be created in the home-dir of the user.
                               (default = /home/vagrant/easy-fedora2vault-2020-02-02T20:20:02.000Z.csv)
     -o, --output-dir  <arg>   Empty directory in which to stage the created AIP bags. It will be created if it
                               doesn't exist.
     -s, --strict              If provided, the transformation will check whether the datasets adhere to the
                               requirements of the chosen transformation.
     -h, --help                Show help message
     -v, --version             Show version of this program

    trailing arguments:
     transformation (required)   The type of transformation used. Only 'simple' is implemented yet.

EXAMPLES
--------

    $ easy-fedora2vault -d easy-dataset:1001 -o ~/stagedAIPs simple
        creates a directory in '~/stagedAIPs'. This directory is an AIP bag, it has the UUID as the directory name, and contains all relevant information from 'easy-dataset:1001' using the 'simple' transformation.
    
    $ easy-fedora2vault -d easy-dataset:1001 -s -o ~/stagedAIPs simple
        easy-dataset:1001 is transformed according to the simple transformation, but only if it fulfils the requirements. The AIP bag is generated in directory '~/stagedAIPs'.
    
    $ easy-fedora2vault -s -u testDepositor -i dataset_ids.txt -o ./stagedAIPs -l ./outputLogfile.csv -f file-metadata.csv simple
        creates a bag in './stagedAIPs' for each dataset in 'dataset_ids.txt' from depositor 'testDepositor' using the 'simple' transformation. If a dataset does not adhere to the 'simple' requirements, or is not deposited by 'testDepositor', it will not be considered and an explanation will be recorded in 'outputLogfile.csv'. If the dataset-id is present in the 'file-metadata.csv' the metadata will be added to the files.xml and the referenced file will be added to the bag's payload.


INSTALLATION AND CONFIGURATION
------------------------------
Currently this project is build only as an RPM package for RHEL7/CentOS7 and later. The RPM will install the binaries to
`/opt/dans.knaw.nl/easy-fedora2vault` and the configuration files to `/etc/opt/dans.knaw.nl/easy-fedora2vault`.

BUILDING FROM SOURCE
--------------------

Prerequisites:

* Java 8 or higher
* Maven 3.3.3 or higher
* RPM

Steps:

        git clone https://github.com/DANS-KNAW/easy-fedora2vault.git
        cd easy-fedora2vault
        mvn clean install

If the `rpm` executable is found at `/usr/local/bin/rpm`, the build profile that includes the RPM
packaging will be activated. If `rpm` is available, but at a different path, then activate it by using
Maven's `-P` switch: `mvn -Pprm install`.

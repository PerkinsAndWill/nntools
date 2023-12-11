# nntools

## Purpose
A place to put commonly used functions for NNers. 

[File an issue](https://github.com/PerkinsAndWill/nntools/issues) for future development ideas. 

If you have your own function  you want incorporated, please create a new branch, drop your function into a new script in the `/R` directory, and then file a pull request!

## Installation
You can install this package using the below code, and filling in the NN access token. The token is in a spreadsheet bookmarked in the private #r-python-core-users Slack channel. Reach out to Bryan Blanc if you have trouble finding the token. 

`devtools::install_github("PerkinsAndWill/nntools",
                          ref = "master",
                          auth_token = "<NN_ACCESS_TOKEN>")`

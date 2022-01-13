# nntools

## Purpose
A place to put commonly used functions for NNers. 

[File an issue](https://github.com/PerkinsAndWill/nntools/issues) or contact Bryan via [Slack](https://nelsonnygaard.slack.com/app_redirect?channel=D1LJ6JERH) or [email](mailto:bblanc@nelsonnygaard.com) if you want Bryan to add functions. 

If you have your own function  you want incorporated, please create a new branch, drop your function into a new script in the `/R` directory, and then file a pull request!

## Installation
You can install this package using the below code, and filling in the NN access token. The token is in a spreadsheet bookmarked in the #r-users channel. 
`devtools::install_github("PerkinsAndWill/nntools",
                          ref = "master",
                          auth_token = "<NN_ACCESS_TOKEN>")`

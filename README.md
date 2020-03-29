# Exemple of Firefox extension with PureScript

Basically just a stripped out version of : https://github.com/Kazy/PureTabs

Tests can be run with `spago test` [TODO : for some reason setting npm "test" script to this value won't work]

Javascript source maps won't work out-of-the-box if JS source files are in e.g. `extension/src` instead of `extension`. This can surely be solved using this issue : https://github.com/parcel-bundler/parcel/issues/3750

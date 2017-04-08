echo 'Formating source code'

find src -name \*.hs -exec hindent {} \;

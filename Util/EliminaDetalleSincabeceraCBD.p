/*
FOR EACH cb-dmov  WHERE     cb-dmov.codcia  = s-codcia     AND 
                            cb-dmov.periodo = s-periodo        AND
                            cb-dmov.nromes  = I                                 
                  BREAK BY  codope 
                        BY  nroast :

*/
for each cb-dmov where
    codcia = 1 and
    periodo = 2007 and
    nromes = 10 and /* 
    codope = "060" and */
    nroast = "110772" :
    display 
    codcia
    periodo
    nromes
    codope
    nroast.
    delete cb-dmov.
end.
    

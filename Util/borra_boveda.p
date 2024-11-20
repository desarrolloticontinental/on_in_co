for each CcbPenDep where
    CcbPenDep.codcia = 1 AND 
    CcbPenDep.coddoc = "bov" AND 
    CcbPenDep.FlgEst = "P":
    if lookup(nroref,"011070052,011070054,011070055" ) > 0 then
    display codref nroref .
    delete CcbPenDep.

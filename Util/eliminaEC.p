for each ccbccaja where
    codcia = 1 and
    coddoc = "e/c" and
    fchdoc = 01/24/08  and
    /*
    lookup(nrodoc,"011070054,011070055,011070049") > 0:
    */
    lookup(nrodoc,"011070052") > 0:
    display
        nrodoc
        fchdoc
        impnac[1]
        impusa[1]
        flgest.
    
    FOR EACH ccbdcaja OF ccbccaja:
        DELETE ccbdcaja.
    END.
    update ccbccaja.flgest.
    
end.

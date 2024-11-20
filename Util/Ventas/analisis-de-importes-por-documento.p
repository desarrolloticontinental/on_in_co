DEF VAR f-ImpBrt    AS DEC.
DEF VAR f-ImpDto    AS DEC.
DEF VAR f-ImpDto2   AS DEC.
DEF VAR f-ImpIgv    AS DEC.
DEF VAR f-ImpIsc    AS DEC.
DEF VAR f-ImpTot    AS DEC.
DEF VAR f-ImpExo    AS DEC.
DEF VAR f-ImpVta    AS DEC.
DEF VAR f-Libre_d01 AS DEC.
DEF VAR f-Libre_d02 AS DEC.
DEF VAR f-igv       AS DEC.
DEF VAR f-isc       AS DEC.
DEF VAR f-ImpLin AS DEC.

FIND ccbcdocu WHERE codcia = 001
    AND coddoc = 'tck'
    AND nrodoc = '512057677'
    NO-LOCK.
ASSIGN
        f-ImpBrt = 0
        f-ImpDto = 0
        f-ImpDto2 = 0
        f-ImpIgv = 0
        f-ImpIsc = 0
        f-ImpTot = 0
        f-ImpExo = 0
        f-ImpVta = 0
        f-Libre_d01 = 0  /* Descuento SIN IGV por Encartes y Otros */
        f-Libre_d02 = 0
    f-ImpLin = 0
        f-igv = 0
        f-isc = 0.
    FOR EACH Ccbddocu OF Ccbcdocu NO-LOCK: 
        f-ImpLin = f-ImpLin + ccbddocu.implin.
        f-ImpTot = f-ImpTot + (Ccbddocu.ImpLin - Ccbddocu.ImpDto2).
        f-ImpDto2 = f-ImpDto2 + Ccbddocu.ImpDto2.
        f-Libre_d02 = f-Libre_d02 + Ccbddocu.ImpDto.
        IF Ccbddocu.AftIgv = YES THEN DO:
            f-Igv = f-Igv + (Ccbddocu.ImpLin - Ccbddocu.ImpDto2) / ( 1 + CcbCDocu.PorIgv / 100) * CcbCDocu.PorIgv / 100.
            /*f-ImpDto = f-ImpDto + Ccbddocu.ImpDto / ( 1 + CcbCDocu.PorIgv / 100).*/
            f-Libre_d01 = f-Libre_d01 + Ccbddocu.ImpDto2 / ( 1 + CcbCDocu.PorIgv / 100).
        END.
        ELSE DO:
            f-ImpExo = f-ImpExo + (Ccbddocu.ImpLin - Ccbddocu.ImpDto2).
            /*f-ImpDto = f-ImpDto + Ccbddocu.ImpDto.*/
            f-Libre_d01 = f-Libre_d01 + Ccbddocu.ImpDto2.
        END.
        IF ccbddocu.aftigv = NO THEN MESSAGE ccbddocu.codmat ccbddocu.implin ccbddocu.impdto2.
    END.
    ASSIGN
        f-ImpIgv = ROUND(f-Igv, 2)
        f-ImpDto = ROUND(f-ImpDto, 2)
        f-Libre_d01 = ROUND(f-Libre_d01, 2)
        f-ImpVta = IF f-ImpIgv > 0 THEN f-ImpTot - f-ImpExo - f-ImpIgv ELSE 0.
    MESSAGE 'resultado parcial' SKIP
        'Total linea:' f-implin SKIP
        'Total General:' f-ImpTot SKIP
        'Total Exonerado:' f-ImpExo SKIP
        'Total Dcto x Vales (con igv):' f-ImpDto2 SKIP
        'Total Dcto x Vales (sin igv):' f-Libre_d01 SKIP
        'Total Dcto x Linea (con IGV):' f-Libre_d02 SKIP
        'Valor Venta:' f-ImpVta SKIP
        'IGV:' f-ImpIgv.
    ASSIGN
        f-ImpBrt = f-ImpVta + (f-ImpDto + f-Libre_d01) + f-ImpExo.
    IF CcbCDocu.PorIgv = 0.00     /* VENTA INAFECTA */
        THEN ASSIGN
            f-ImpIgv = 0
            f-ImpVta = f-ImpExo
            f-ImpBrt = f-ImpExo.

    /* ************************************************** */

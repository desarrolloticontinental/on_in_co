DEF VAR x-linea AS CHAR FORMAT 'x(100)'.
DEF VAR x-codcli AS CHAR FORMAT 'x(11)'.
DEF VAR x-rebate AS DEC FORMAT '>>.9999' DECIMALS 4.
DEF VAR x-imptot AS DEC.

DEF BUFFER b-cdocu FOR ccbcdocu.

INPUT FROM c:\tmp\provincias.prn.
OUTPUT TO c:\tmp\rebadeprovincias.prn.
REPEAT :
    IMPORT UNFORMATTED x-linea.
    IF x-linea <> '' THEN DO:
        ASSIGN
            x-codcli = SUBSTRING(x-linea,1,11) 
            x-rebate = DECIMAL(SUBSTRING(x-linea,12)).
        FOR EACH ccbcdocu NO-LOCK WHERE ccbcdocu.codcia = 1
            AND LOOKUP(ccbcdocu.coddoc, 'fac,bol,n/c') > 0
            AND flgest <> 'a'
            AND coddiv = '00000'
            AND fchdoc >= 11/01/2010
            AND fchdoc <= 03/31/2011
            AND nroped BEGINS '040'
            AND codcli = x-codcli:
            
            IF LOOKUP(ccbcdocu.coddoc, 'fac,bol') > 0 THEN DO:
                IF LOOKUP(CcbCdocu.TpoFac, 'A,S') > 0 THEN NEXT.
                IF LOOKUP(ccbcdocu.fmapgo, '102,103,104') = 0 THEN NEXT.
            END.
            IF ccbcdocu.coddoc = 'n/c' THEN DO:
                IF ccbcdocu.cndcre <> 'D' THEN NEXT.
                FIND b-cdocu WHERE b-cdocu.codcia = 1
                    AND b-cdocu.coddoc = ccbcdocu.codref
                    AND b-cdocu.nrodoc = ccbcdocu.nroref
                    NO-LOCK NO-ERROR.
                IF NOT AVAILABLE b-cdocu THEN NEXT.
                IF LOOKUP(b-cdocu.TpoFac, 'A,S') > 0 THEN NEXT.
                IF LOOKUP(b-cdocu.fmapgo, '102,103,104') = 0 THEN NEXT.
            END.
            x-imptot = ccbcdocu.imptot * ( IF ccbcdocu.coddoc = 'n/c' THEN -1 ELSE 1 ).
            DISPLAY 
                ccbcdocu.coddoc 
                ccbcdocu.nrodoc 
                ccbcdocu.fmapgo
                ccbcdocu.codcli 
                ccbcdocu.nomcli 
                ccbcdocu.codped 
                ccbcdocu.nroped 
                ccbcdocu.fchdoc 
                x-imptot 
                x-imptot * x-rebate
                WITH STREAM-IO NO-BOX WIDTH 320.
        END.
    END.
END.
INPUT CLOSE.
OUTPUT CLOSE.


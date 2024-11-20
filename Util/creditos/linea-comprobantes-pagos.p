DEF VAR x-codcli AS CHAR NO-UNDO.
DEF VAR pCodCia AS INT INIT 000 NO-UNDO.
DEF VAR pMonLC AS INT NO-UNDO.
DEF VAR pImpLCred AS DEC NO-UNDO.
DEF VAR pFchIni AS DATE NO-UNDO.
DEF VAR pFchFin AS DATE NO-UNDO.

DEF STREAM lineacred.

INPUT FROM d:\tmp\clientes.txt.
OUTPUT STREAM lineacred TO d:\tmp\cliente-linea.txt.
REPEAT:
    IMPORT UNFORMATTED x-codcli.
    IF x-codcli = '' THEN LEAVE.
    FIND gn-clie WHERE gn-clie.codcia = pcodcia
        AND gn-clie.codcli = x-codcli
        NO-LOCK NO-ERROR.
    IF NOT AVAILABLE gn-clie THEN NEXT.
    ASSIGN
        pFchIni = ?
        pFchFin = ?
        pMonLC = 0
        pImpLCred = 0.
    FOR EACH Gn-ClieL NO-LOCK WHERE Gn-ClieL.CodCia = pcodcia 
        AND Gn-ClieL.CodCli = gn-clie.codcli 
        AND Gn-ClieL.FchIni <> ? 
        AND Gn-ClieL.FchFin <> ? 
        BY gn-cliel.fchini BY gn-cliel.fchfin:
        pFchIni = gn-cliel.fchini.
        pfchfin = gn-cliel.fchfin.
        pMonLC    = gn-cliel.monlc.
        pImpLCred = Gn-ClieL.ImpLC.
    END.
    PUT STREAM lineacred UNFORMATTED
        gn-clie.codcli '|'
        gn-clie.nomcli '|'
        pFchIni '|'
        pFchFin '|'
        pMonLC '|'
        pImpLCred
        SKIP.
END.
OUTPUT STREAM lineacred CLOSE.
INPUT CLOSE.

DEF STREAM comprobantes.
DEF STREAM pagos.
DEF VAR x-imptot AS DEC NO-UNDO.

INPUT FROM d:\tmp\clientes.txt.
OUTPUT STREAM comprobantes TO d:\tmp\clientes-comprobantes.txt.
OUTPUT STREAM pagos TO d:\tmp\clientes-pagos.txt.
REPEAT:
    IMPORT UNFORMATTED x-codcli.
    IF x-codcli = '' THEN LEAVE.
    FIND gn-clie WHERE gn-clie.codcia = pcodcia
        AND gn-clie.codcli = x-codcli
        NO-LOCK NO-ERROR.
    IF NOT AVAILABLE gn-clie THEN NEXT.
    DISPLAY gn-clie.codcli WITH STREAM-IO.
    PAUSE 0.
    FOR EACH ccbcdocu NO-LOCK WHERE ccbcdocu.codcia = 001
        AND ccbcdocu.codcli = gn-clie.codcli
        AND LOOKUP(ccbcdocu.coddoc, 'FAC,BOL,TCK,LET,N/C,N/D,BD') > 0
        AND LOOKUP(ccbcdocu.flgest, 'P,C') > 0
        AND ccbcdocu.fchdoc >= 01/01/2014:
        x-imptot = ccbcdocu.imptot.
        IF ccbcdocu.codmon = 2 THEN x-imptot = x-imptot * ccbcdocu.tpocmb.
        PUT STREAM comprobantes UNFORMATTED
            ccbcdocu.codcli '|'
            ccbcdocu.coddoc '|'
            ccbcdocu.nrodoc '|'
            ccbcdocu.fchdoc '|'
            ccbcdocu.codven '|'
            ccbcdocu.fmapgo '|'
            x-imptot
            SKIP.
        FOR EACH ccbdcaja NO-LOCK WHERE ccbdcaja.codcia = 001
            AND ccbdcaja.codref = ccbcdocu.coddoc
            AND ccbdcaja.nroref = ccbcdocu.nrodoc:
            x-imptot = ccbdcaja.imptot.
            IF ccbdcaja.codmon = 2 THEN x-imptot = x-imptot * ccbdcaja.tpocmb.
            PUT STREAM pagos UNFORMATTED
                ccbdcaja.codcli '|'
                ccbdcaja.codref '|'
                ccbdcaja.nroref '|'
                ccbdcaja.fchdoc '|'
                ccbdcaja.coddoc '|'
                ccbdcaja.nrodoc '|'
                x-imptot
                SKIP.
        END.
    END.
END.
INPUT CLOSE.



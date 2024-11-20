/* TEXTO SUNAT PERCEPCIONES AL CREDITO */
DEF VAR s-codcia AS INT INIT 001 NO-UNDO.
DEF VAR cl-codcia AS INT INIT 000 NO-UNDO.
DEF VAR x-TipCli AS CHAR FORMAT 'x(2)' NO-UNDO.
DEF VAR x-NroDocId AS CHAR NO-UNDO.
DEF VAR x-Importe AS DEC NO-UNDO.
DEF VAR FechaD AS DATE NO-UNDO.
DEF VAR FechaH AS DATE NO-UNDO.

ASSIGN
    FechaD = 02/01/2015
    FechaH = 02/28/2015.

DEF BUFFER BDMOV FOR ccbdmvto.

OUTPUT TO c:\tmp\sunat-percepciones-contado.txt.
FOR EACH ccbcmvto NO-LOCK WHERE codcia = 1
    AND coddoc = "PERC"     /* OJO: PER CONTADO */
    AND LOOKUP(SUBSTRING(nrodoc,1,3), "914,915,008") = 0
    AND libre_chr[3] = "MOSTRADOR"
    AND flgest <> "A"
    AND fchdoc >= FechaD
    AND fchdoc <= FechaH,
    EACH ccbdmvto NO-LOCK WHERE ccbdmvto.codcia = s-codcia
    AND ccbdmvto.coddoc = ccbcmvto.coddoc
    AND ccbdmvto.nrodoc = ccbcmvto.nrodoc
    AND ccbdmvto.coddiv = ccbcmvto.coddiv,
    FIRST ccbcdocu NO-LOCK WHERE ccbcdocu.codcia = s-codcia
    AND ccbcdocu.coddoc = ccbdmvto.codref
    AND ccbcdocu.nrodoc = ccbdmvto.nroref,
    FIRST facdocum OF ccbcdocu NO-LOCK,
    FIRST gn-clie NO-LOCK WHERE gn-clie.codcia = cl-codcia
    AND gn-clie.codcli = ccbcdocu.codcli
    BY ccbcmvto.nrodoc:
/*     x-Importe = 0.                                             */
/*     FOR EACH BDMOV NO-LOCK WHERE BDMOV.codcia = s-codcia       */
/*         AND BDMOV.coddoc = ccbcmvto.coddoc                     */
/*         AND BDMOV.nrodoc = ccbcmvto.nrodoc                     */
/*         AND BDMOV.coddiv = ccbcmvto.coddiv:                    */
/*         x-Importe = x-Importe + (BDMOV.imptot + BDMOV.impdoc). */
/*     END.                                                       */
    x-Importe = Ccbdmvto.imptot + Ccbdmvto.impdoc.
    x-TipCli = (IF LOOKUP(SUBSTRING(ccbcdocu.codcli,1,2), '20,10') > 0
        THEN "06" ELSE "01").
    x-NroDocId = (IF x-TipCli = "06" THEN Ccbcdocu.RucCli ELSE CcbCDocu.CodAnt).
    PUT UNFORMATTED
        x-TipCli '|'
        x-NroDocId '|'
        (IF SUBSTRING(ccbcdocu.codcli,1,2) = "20" THEN gn-clie.nomcli ELSE "") '|'
        (IF SUBSTRING(ccbcdocu.codcli,1,2) <> "20" THEN gn-clie.apepat ELSE "") '|'
        (IF SUBSTRING(ccbcdocu.codcli,1,2) <> "20" THEN gn-clie.apepat ELSE "") '|'
        (IF SUBSTRING(ccbcdocu.codcli,1,2) <> "20" THEN gn-clie.nombre ELSE "") '|'
        SUBSTRING(Ccbcdocu.nrodoc,1,3) '|'
        SUBSTRING(Ccbcdocu.nrodoc,4) '|'
        STRING(Ccbcdocu.fchdoc, '99/99/9999') '|'
        ( IF Ccbcdocu.coddoc = 'FAC' THEN '1' ELSE 
            (IF Ccbcdocu.coddoc = "TCK" AND Ccbcdocu.Libre_C04 = 'FAC' 
             THEN '1' ELSE '0') ) '|'
        "0" '|'
        (IF Ccbcdocu.acubon[4] = 0.5 THEN "1" ELSE "0") '|'
        TRIM(STRING(x-Importe, '>>>>>>>>>>>9.99')) '|'
        FacDocum.CodCbd '|'
/*         SUBSTRING(Ccbcdocu.nrodoc,1,3) '|'                                        */
/*         SUBSTRING(Ccbcdocu.nrodoc,4) '|'                                          */
/*         STRING(Ccbcdocu.fchdoc, '99/99/9999') '|'                                 */
/*         TRIM(STRING(Ccbcdocu.imptot - Ccbcdocu.acubon[5], '>>>>>>>>>>>9.99')) '|' */
        SKIP.
END.
OUTPUT CLOSE.

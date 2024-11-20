DEF VAR x-CodFchF AS DATE NO-UNDO.
DEF VAR x-CodFchI AS DATE NO-UNDO.
DEF VAR s-CodCia AS INT INIT 001 NO-UNDO.

DEF TEMP-TABLE t-ccbdcaja LIKE ccbdcaja
    FIELD fchvto AS DATE FORMAT '99/99/9999'.

ASSIGN
    x-CodFchF = TODAY - 1
    x-CodFchI = DATE(01,01,2016).

DEF VAR x-Archivo AS CHAR NO-UNDO.
/* ******************************************************************************* */
/* NOTAS BANCARIAS */
/* ******************************************************************************* */
x-Archivo = "d:\" + "ccbcmvto" + STRING(DAY(x-CodFchI),'99') + 
    STRING(MONTH(x-CodFchI),'99') + STRING(YEAR(x-CodFchI),'9999') + ".txt".
OUTPUT TO VALUE(x-archivo) KEEP-MESSAGES.
FOR EACH ccbcmvto NO-LOCK WHERE ccbcmvto.CodCia = s-codcia AND
    ccbcmvto.CodDoc = "N/B" AND
    ccbcmvto.FchCbd >= x-CodFchI AND
    ccbcmvto.FchCbd <= x-CodFchF AND
    ccbcmvto.FlgEst <> "A":
    EXPORT DELIMITER "~029" ccbcmvto.
END.
OUTPUT CLOSE.
/* Cargamos temporal */
EMPTY TEMP-TABLE t-ccbdcaja.
FOR EACH ccbcmvto NO-LOCK WHERE ccbcmvto.CodCia = s-codcia AND
    ccbcmvto.CodDoc = "N/B" AND
    ccbcmvto.FchCbd >= x-CodFchI AND
    ccbcmvto.FchCbd <= x-CodFchF AND
    ccbcmvto.FlgEst <> "A",
    EACH ccbdcaja NO-LOCK WHERE ccbdcaja.codcia = ccbcmvto.codcia AND
    ccbdcaja.coddoc = ccbcmvto.coddoc AND
    ccbdcaja.nrodoc = ccbcmvto.nrodoc:
    CREATE t-ccbdcaja.
    BUFFER-COPY ccbdcaja TO t-ccbdcaja.
    FIND ccbcdocu WHERE ccbcdocu.codcia = ccbdcaja.codcia AND
        ccbcdocu.coddoc = ccbdcaja.codref AND
        ccbcdocu.nrodoc = ccbdcaja.nroref NO-LOCK NO-ERROR.
    IF AVAILABLE ccbcdocu THEN t-ccbdcaja.fchvto = ccbcdocu.fchvto.
END.
x-Archivo = "d:\" + "ccbdmvto" + STRING(DAY(x-CodFchI),'99') +
    STRING(MONTH(x-CodFchI),'99') + STRING(YEAR(x-CodFchI),'9999') + ".txt".
OUTPUT TO VALUE(x-archivo) KEEP-MESSAGES.
FOR EACH t-ccbdcaja NO-LOCK:
    EXPORT DELIMITER "~029" t-ccbdcaja.
END.
OUTPUT CLOSE.

/* ******************************************************************************* */
/* CANJE RENOVACION y REFINANCIACION DE LETRAS */
/* ******************************************************************************* */
x-Archivo = "d:\" + "ccbcmvto" + STRING(DAY(x-CodFchI),'99') + 
    STRING(MONTH(x-CodFchI),'99') + STRING(YEAR(x-CodFchI),'9999') + ".txt".
OUTPUT TO VALUE(x-archivo) APPEND KEEP-MESSAGES.
FOR EACH ccbcmvto NO-LOCK WHERE ccbcmvto.CodCia = s-codcia AND
    LOOKUP(ccbcmvto.CodDoc, "CJE,REF,RNV") > 0 AND
    ccbcmvto.FchDoc >= x-CodFchI AND
    ccbcmvto.FchDoc <= x-CodFchF AND
    ccbcmvto.FlgEst = "E":
    EXPORT DELIMITER "~029" ccbcmvto.
END.
OUTPUT CLOSE.
/* Cargamos temporal */
EMPTY TEMP-TABLE t-ccbdcaja.
FOR EACH ccbcmvto NO-LOCK WHERE ccbcmvto.CodCia = s-codcia AND
    LOOKUP(ccbcmvto.CodDoc, "CJE,REF,RNV") > 0 AND
    ccbcmvto.FchDoc >= x-CodFchI AND
    ccbcmvto.FchDoc <= x-CodFchF AND
    ccbcmvto.FlgEst = "E",
    EACH ccbdcaja NO-LOCK WHERE ccbdcaja.codcia = ccbcmvto.codcia AND
    ccbdcaja.coddoc = ccbcmvto.coddoc AND
    ccbdcaja.nrodoc = ccbcmvto.nrodoc:
    CREATE t-ccbdcaja.
    BUFFER-COPY ccbdcaja TO t-ccbdcaja.
    FIND ccbcdocu WHERE ccbcdocu.codcia = ccbdcaja.codcia AND
        ccbcdocu.coddoc = ccbdcaja.codref AND
        ccbcdocu.nrodoc = ccbdcaja.nroref NO-LOCK NO-ERROR.
    IF AVAILABLE ccbcdocu THEN t-ccbdcaja.fchvto = ccbcdocu.fchvto.
END.
x-Archivo = "d:\" + "ccbdmvto" + STRING(DAY(x-CodFchI),'99') +
    STRING(MONTH(x-CodFchI),'99') + STRING(YEAR(x-CodFchI),'9999') + ".txt".
OUTPUT TO VALUE(x-archivo) APPEND KEEP-MESSAGES.
FOR EACH t-ccbdcaja NO-LOCK:
    EXPORT DELIMITER "~029" t-ccbdcaja.
END.
OUTPUT CLOSE.

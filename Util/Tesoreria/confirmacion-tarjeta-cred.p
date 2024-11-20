DEF VAR s-CodCia AS INT INIT 001.
DEF VAR cTpoRef AS CHARACTER.
DEF VAR cFlgEst AS CHARACTER.
DEF VAR rpta AS LOGICAL NO-UNDO.
DEF VAR moneda AS CHARACTER NO-UNDO.
DEF VAR importe AS DECIMAL NO-UNDO.
DEF VAR TarjCre AS CHARACTER NO-UNDO.
DEF VAR NroTarj AS CHARACTER NO-UNDO.

DEFINE BUFFER b_DMvtO FOR CcbDMvto.
    

cTpoRef = "TCR".
cFlgEst = "P".

&SCOPED-DEFINE Condicion ( CcbDMvto.CodCia = S-CodCia ~
AND CcbDMvto.TpoRef = cTpoRef ~
AND CcbDMvto.FchEmi < 03/15/2012 ~
AND CcbDMvto.FlgEst BEGINS cFlgEst )

FOR EACH CcbDMvto WHERE {&Condicion} NO-LOCK:
    RUN confirmar-deposito.
END.



PROCEDURE confirmar-deposito:

    FIND b_DMvtO WHERE ROWID(b_DMvtO) = ROWID(CcbDMvto)
        EXCLUSIVE-LOCK NO-ERROR.
    IF AVAILABLE b_DMvto THEN ASSIGN b_DMvto.FlgEst = "C".
END.

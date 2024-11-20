DEF NEW SHARED VAR s-codcia AS INT INIT 001.
DEF NEW SHARED VAR s-codalm AS CHAR INIT '11'.
DEF NEW SHARED VAR s-user-id AS CHAR INIT 'SYSTEM'.
DEF NEW SHARED VAR s-coddiv AS CHAR INIT '00000'.

DEFINE BUFFER B-MATE FOR Almmmate.
DEFINE BUFFER b-ooMoviAlmacen FOR OOMoviAlmacen.
DEFINE TEMP-TABLE T-CMOV LIKE Almcmov.

DEF VAR pMensaje AS CHAR.

/* RHC 14/07/2020 NO compras por Drop Shipping */
&SCOPED-DEFINE Condicion ( OOMoviAlmacen.codcia = s-codcia ~
    AND OOMoviAlmacen.codalm = s-codalm ~
    AND OOMoviAlmacen.FlagMigracion = "N" ~
    AND OOMoviAlmacen.TipMov = "I" ~
    AND OOMoviAlmacen.CodMov = 09 ~
    AND OOMoviAlmacen.UseInDropShipment = "no" )

FOR EACH OOMoviAlmacen WHERE {&Condicion} NO-LOCK
    BREAK BY OOMoviAlmacen.CodAlm 
    BY OOMoviAlmacen.TipMov
    BY OOMoviAlmacen.CodMov
    BY OOMoviAlmacen.NroSer 
    BY OOMoviAlmacen.NroDoc:
    IF FIRST-OF(OOMoviAlmacen.CodAlm) 
        OR FIRST-OF(OOMoviAlmacen.TipMov) 
        OR FIRST-OF(OOMoviAlmacen.CodMov)
        OR FIRST-OF(OOMoviAlmacen.NroSer)
        OR FIRST-OF(OOMoviAlmacen.NroDoc)
        THEN DO:
        {lib/lock-genericov3.i ~
            &Tabla="Ccbcdocu" ~
            &Condicion="CcbCDocu.CodCia = S-CODCIA ~
            AND CcbCDocu.CodDoc = ooMoviAlmacen.CodRef ~
            AND CcbCDocu.NroDoc = ooMoviAlmacen.NroRef" ~
            &Bloqueo="EXCLUSIVE-LOCK NO-ERROR" ~
            &Accion="RETRY" ~
            &Mensaje="NO" ~
            &txtMensaje="pMensaje" ~
            &TipoError="UNDO, RETURN 'ADM-ERROR'" }
        ASSIGN 
            CcbCDocu.FlgCon = "P".
    END.
    FIND Ccbddocu WHERE Ccbddocu.codcia = s-codcia AND
        Ccbddocu.coddoc = ooMoviAlmacen.CodRef AND
        Ccbddocu.nrodoc = ooMoviAlmacen.NroRef AND
        Ccbddocu.codmat = OOMoviAlmacen.CodMat
        EXCLUSIVE-LOCK NO-ERROR.
    IF AVAILABLE Ccbddocu THEN DO:
        Ccbddocu.candev = CcbDDocu.CanDev - ooMoviAlmacen.CanDes.
        IF Ccbddocu.candev < 0 THEN Ccbddocu.candev = 0.
    END.
END.


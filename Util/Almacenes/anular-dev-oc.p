DEF VAR s-codcia AS INT INIT 001 NO-UNDO.
DEF VAR s-codalm AS CHAR NO-UNDO.
DEF VAR s-tipmov AS CHAR INIT 'S' NO-UNDO.
DEF VAR s-codmov AS INT INIT 09 NO-UNDO.
DEF VAR s-user-id AS CHAR INIT 'CONTAB' NO-UNDO.
DEFINE        VAR R-ROWID  AS ROWID   NO-UNDO.

DEF BUFFER CMOV FOR almcmov.

ASSIGN s-codalm = '21'.

FOR EACH almcmov WHERE almcmov.codcia = s-codcia
    AND almcmov.codalm = s-codalm
    AND almcmov.tipmov = s-tipmov
    AND almcmov.codmov = s-codmov
    AND almcmov.nroser = 229
    AND almcmov.nrodoc >= 3779 AND almcmov.nrodoc <= 3852
    TRANSACTION ON ERROR UNDO, RETURN ON STOP UNDO, RETURN:
    DISPLAY almcmov.nrodoc. PAUSE 0.
    RUN Actualiza-Detalle-Orden-Compra(1).
    IF RETURN-VALUE = 'ADM-ERROR' THEN UNDO, RETURN.
    RUN Borra-Detalle.
    IF RETURN-VALUE = 'ADM-ERROR' THEN UNDO, RETURN.
    /* Solo marcamos el FlgEst como Anulado */
    FIND CMOV WHERE CMOV.CodCia = Almcmov.CodCia AND
        CMOV.CodAlm = Almcmov.CodAlm AND
        CMOV.TipMov = Almcmov.TipMov AND
        CMOV.CodMov = Almcmov.CodMov AND
        CMOV.NroSer = Almcmov.NroSer AND
        CMOV.NroDoc = Almcmov.NroDoc EXCLUSIVE-LOCK NO-ERROR.
    IF ERROR-STATUS:ERROR THEN UNDO, RETURN.
    ASSIGN CMOV.FlgEst = 'A'
        CMOV.Observ = "      A   N   U   L   A   D   O       "
        CMOV.Usuario = S-USER-ID.
    RELEASE CMOV.
END.



PROCEDURE Actualiza-Detalle-Orden-Compra:

  DEFINE INPUT PARAMETER I-Factor AS INTEGER.
  
  FOR EACH Almdmov NO-LOCK WHERE 
           Almdmov.CodCia = Almcmov.CodCia AND
           Almdmov.CodAlm = Almcmov.CodAlm AND
           Almdmov.TipMov = Almcmov.TipMov AND
           Almdmov.CodMov = Almcmov.CodMov AND
           Almdmov.NroDoc = Almcmov.NroDoc
      ON ERROR UNDO, RETURN "ADM-ERROR":
      
    FIND LG-DOCmp WHERE 
           LG-DOCmp.CodCia = Almdmov.CodCia AND
           LG-DOCmp.TpoDoc = "N"            AND
           LG-DOCmp.NroDoc = INTEGER(Almcmov.NroRf1) AND
           LG-DOCmp.Codmat = Almdmov.CodMat EXCLUSIVE-LOCK NO-ERROR.
    IF ERROR-STATUS:ERROR THEN NEXT.
    LG-DOCmp.CanAten = LG-DOCmp.CanAten + (Almdmov.CanDes * I-Factor).
    RELEASE LG-DOCmp.
  END.

END PROCEDURE.

PROCEDURE Borra-Detalle:

  FOR EACH Almdmov OF Almcmov EXCLUSIVE-LOCK ON ERROR UNDO, RETURN "ADM-ERROR":

    ASSIGN R-ROWID = ROWID(Almdmov).
    RUN alm/almacstk (R-ROWID).
    IF RETURN-VALUE = 'ADM-ERROR' THEN UNDO, RETURN 'ADM-ERROR'.

    RUN ALM\ALMACPR1 (R-ROWID,"D").
    IF RETURN-VALUE = 'ADM-ERROR' THEN UNDO, RETURN 'ADM-ERROR'.

    DELETE Almdmov.
  END.


END PROCEDURE.

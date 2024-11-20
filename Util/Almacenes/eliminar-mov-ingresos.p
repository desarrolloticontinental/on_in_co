DEF BUFFER CMOV FOR almcmov.
DEF VAR R-ROWID AS ROWID.
DEF VAR s-codcia AS INT INIT 001.

DEFINE VAR cAlm AS CHAR.
DEFINE VAR cTipMov AS CHAR.
DEFINE VAR iCodMov AS INT.
DEFINE VAR iNroDesde AS INT.
DEFINE VAR iNroHasta AS INT.

cAlm = "11".
cTipMov = 'I'.
iCodMov = 3.
iNroDesde =  103325.
iNroHasta =  103325.

FOR EACH almcmov WHERE codcia = s-codcia
    AND codalm = cAlm       /*'11'*/
    AND tipmov = cTipMov    /*'i'*/
    AND nrodoc >= iNroDesde /*045850*/
    AND nrodoc <= iNroHasta /*045958*/
    /*AND codcli <> '20100128056'*/
    AND codmov = iCodMov:        /*02:  /* COMPRAS */*/

/*      RUN Actualiza-Detalle-Orden-Compra(-1).                      */
/*      IF RETURN-VALUE = 'ADM-ERROR' THEN UNDO, RETURN 'ADM-ERROR'. */
     RUN Borra-Detalle.
     IF RETURN-VALUE = 'ADM-ERROR' THEN UNDO, RETURN 'ADM-ERROR'.
/*      RUN Cerrar-Orden-Compra.                                     */
/*      IF RETURN-VALUE = 'ADM-ERROR' THEN UNDO, RETURN 'ADM-ERROR'. */
     /* Solo marcamos el FlgEst como Anulado */
     FIND CMOV WHERE 
          CMOV.CodCia = Almcmov.CodCia AND 
          CMOV.CodAlm = Almcmov.CodAlm AND 
          CMOV.TipMov = Almcmov.TipMov AND 
          CMOV.CodMov = Almcmov.CodMov AND 
          CMOV.NroSer = Almcmov.NroSer AND 
          CMOV.NroDoc = Almcmov.NroDoc EXCLUSIVE-LOCK NO-ERROR.
     IF ERROR-STATUS:ERROR THEN UNDO, RETURN 'ADM-ERROR'.
     ASSIGN CMOV.FlgEst = 'A'
            CMOV.Observ = "      A   N   U   L   A   D   O       "
            CMOV.Usuario = 'MRC-00'.
     RELEASE CMOV.
    
END.

MESSAGE "Termino".

RETURN.

PROCEDURE Actualiza-Detalle-Orden-Compra:
  DEFINE INPUT PARAMETER I-Factor AS INTEGER.

  FOR EACH Almdmov OF Almcmov NO-LOCK ON ERROR UNDO, RETURN "ADM-ERROR"
        ON STOP  UNDO, RETURN "ADM-ERROR":
    FIND LG-DOCmp WHERE 
        LG-DOCmp.CodCia = Almdmov.CodCia AND
        LG-DOCmp.TpoDoc = "N" AND
        LG-DOCmp.NroDoc = INTEGER(Almcmov.NroRf1) AND
        LG-DOCmp.Codmat = Almdmov.CodMat EXCLUSIVE-LOCK NO-ERROR.
    IF AVAILABLE lg-docmp
    THEN LG-DOCmp.CanAten = LG-DOCmp.CanAten + (Almdmov.CanDes * I-Factor).
    RELEASE LG-DOCmp.
  END.

END.

PROCEDURE Borra-Detalle:
  FOR EACH Almdmov OF Almcmov EXCLUSIVE-LOCK ON ERROR UNDO, RETURN "ADM-ERROR"
        ON STOP UNDO, RETURN 'ADM-ERROR':
    ASSIGN R-ROWID = ROWID(Almdmov).
    RUN ALM\ALMDCSTK (R-ROWID). /* Descarga del Almacen */
    IF RETURN-VALUE = 'ADM-ERROR' THEN UNDO, RETURN 'ADM-ERROR'.

    RUN ALM\ALMACPR1 (R-ROWID,"D").
    IF RETURN-VALUE = 'ADM-ERROR' THEN UNDO, RETURN 'ADM-ERROR'.

    /* RHC 03.04.04 BLOQUEADO, SE ACTUALIZA EN LA NOCHE
    RUN ALM\ALMACPR2 (R-ROWID,"D").
    *************************************************** */
    DELETE Almdmov.
  END.

END.

PROCEDURE Cerrar-Orden-Compra:

  DEFINE VAR I-NRO AS INTEGER INIT 0 NO-UNDO.

  FOR EACH LG-DOCmp NO-LOCK WHERE LG-DOCmp.CodCia = s-CodCia 
        AND LG-DOCmp.TpoDoc = "N" 
        AND LG-DOCmp.NroDoc = INTEGER(Almcmov.NroRf1):
    IF (LG-DOCmp.CanPedi - LG-DOCmp.CanAten) > 0 THEN DO:
        I-NRO = 1.
        LEAVE.
    END.
  END.

  DO TRANSACTION ON ERROR UNDO, RETURN 'ADM-ERROR' ON STOP UNDO, RETURN 'ADM-ERROR':
      FIND LG-COCmp WHERE 
            LG-COCmp.CodCia = Almcmov.CodCia AND
            LG-COCmp.TpoDoc = "N" AND
            LG-COCmp.NroDoc = INTEGER(Almcmov.NroRf1) EXCLUSIVE-LOCK NO-ERROR.
      IF NOT AVAILABLE LG-cocmp THEN RETURN 'ADM-ERROR'.
      IF I-NRO = 0 THEN LG-COCmp.FlgSit = "T".
      ELSE              LG-COCmp.FlgSit = "P".
      LG-COCmp.FchAte = Almcmov.FchDoc.
      RELEASE LG-COCmp.
  END.

END.

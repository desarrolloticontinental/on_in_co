DEF NEW SHARED VAR s-codcia AS INT INIT 001.
DEF NEW SHARED VAR s-user-id AS CHAR INIT 'ADMIN'.

/* Temporal de Reclasificacón */
DEF TEMP-TABLE Detalle
    FIELD CodAnt AS CHAR
    FIELD CodNew AS CHAR
    FIELD Factor AS DEC DECIMALS 6.

/* Cargamos temporal */
DEF VAR x-linea AS CHAR NO-UNDO.
INPUT FROM d:\tmp\yiseli.prn.
REPEAT:
    IMPORT UNFORMATTED x-linea.
    IF x-linea = '' THEN LEAVE.
    CREATE detalle.
    ASSIGN
        detalle.codant = SUBSTRING(x-linea,1,6)
        detalle.codnew = SUBSTRING(x-linea,11,6)
        detalle.factor = DECIMAL(SUBSTRING(x-linea,21)).
END.
FOR EACH detalle:
    IF codant = '' THEN DELETE detalle.
END.

DEF TEMP-TABLE ITEM   LIKE almdmov.     /* INGRESOS */
DEF TEMP-TABLE ITEM-1 LIKE Almdmov.     /* SALIDAS */
DEF VAR N-Itm AS INT NO-UNDO.
DEF BUFFER B-CMOV FOR Almcmov.
DEF VAR pMensaje AS CHAR NO-UNDO.
DEF VAR x-ImpCto LIKE Almdmov.ImpCto NO-UNDO.

/* Verificación */
FOR EACH Detalle NO-LOCK:
    DISPLAY 'Verificando' detalle.codant detalle.codnew 
        WITH STREAM-IO NO-BOX.
    PAUSE 0.
    FIND Almmmatg WHERE Almmmatg.codcia = s-codcia
        AND Almmmatg.codmat = Detalle.codant
        NO-LOCK NO-ERROR.
    IF NOT AVAILABLE Almmmatg THEN DO:
        MESSAGE 'Código anterior' Detalle.codant 'NO REGISTRADO' SKIP
            'Proceso Abortado' VIEW-AS ALERT-BOX WARNING.
        RETURN.
    END.
    IF Almmmatg.tpoart = "D" THEN DO:
        MESSAGE 'Código anterior' Detalle.codant 'NO ACTIVADO' SKIP
            'Proceso Abortado' VIEW-AS ALERT-BOX WARNING.
        RETURN.
    END.

    FIND Almmmatg WHERE Almmmatg.codcia = s-codcia
        AND Almmmatg.codmat = Detalle.codnew
        NO-LOCK NO-ERROR.
    IF NOT AVAILABLE Almmmatg THEN DO:
        MESSAGE 'Código nuevo' Detalle.codnew 'NO REGISTRADO' SKIP
            'Proceso Abortado' VIEW-AS ALERT-BOX WARNING.
        RETURN.
    END.
    IF Almmmatg.tpoart = "D" THEN DO:
        MESSAGE 'Código nuevo' Detalle.codnew 'NO ACTIVADO' SKIP
            'Proceso Abortado' VIEW-AS ALERT-BOX WARNING.
        RETURN.
    END.
END.
/* Regeneramos Kardex de ambos códigos */
FOR EACH Detalle NO-LOCK:
    DISPLAY 'Recalculando costo promedio' detalle.codant detalle.codnew
        WITH STREAM-IO NO-BOX.
    PAUSE 0.
    RUN aplic\alm\calc-costo-promedio (Detalle.codant, DATE(01,01,2016)).
    RUN aplic\alm\calc-costo-promedio (Detalle.codnew, DATE(01,01,2016)).
END.
DISPLAY 'Kardex OK' WITH STREAM-IO NO-BOX.
PAUSE 0.
/* Reclasificacion */
FOR EACH Detalle NO-LOCK:
    /* Barremos los almacenes que tengan stock */
    FOR EACH Almmmate NO-LOCK WHERE Almmmate.codcia = s-codcia
        AND Almmmate.codmat = Detalle.CodAnt
        AND Almmmate.stkact <> 0:
        DISPLAY 'Carga Temporal de Reclasificaciones' detalle.codant detalle.codnew 
            WITH STREAM-IO NO-BOX.
        PAUSE 0.
        /* Cargamos el temporal con todos los movimientos */
        RUN Reclasificacion.
    END.
END.
DISPLAY 'Carga temporal OK' WITH STREAM-IO NO-BOX.
PAUSE 0.
/* Verificamos si el movimiento esta activo en almacén */
FOR EACH ITEM NO-LOCK WHERE ITEM.codcia = s-codcia BREAK BY ITEM.CodAlm:
    IF FIRST-OF(ITEM.CodAlm) THEN DO:
        FIND FIRST Almtdocm WHERE Almtdocm.CodCia = s-codcia
            AND Almtdocm.CodAlm = ITEM.codalm
            AND Almtdocm.TipMov = "I"
            AND Almtdocm.CodMov = 13
            NO-LOCK NO-ERROR.
        IF NOT AVAILABLE Almtdocm THEN DO:
            CREATE Almtdocm.
            ASSIGN
                Almtdocm.CodAlm = ITEM.codalm
                Almtdocm.CodCia = s-codcia
                Almtdocm.CodMov = 13
                Almtdocm.NroDoc = 1
                Almtdocm.TipMov = "I".
            MESSAGE 'Movimiento I-13 NO registrado en el almacén' ITEM.CodAlm SKIP
                'Se está configurando automáticamente' VIEW-AS ALERT-BOX WARNING.
        END.
    END.
END.
/* Actualizamos materiales por almacen */
DISPLAY 'Artículos por almacén' WITH STREAM-IO NO-BOX.
PAUSE 0.
RUN Articulos-por-Almacen.
IF RETURN-VALUE = 'ADM-ERROR' THEN DO:
    IF pMensaje = "" THEN pMensaje = 'ERROR EN EL PROCESO ARTICLOS POR ALMACEN: PROCESO ABORTADO'.
    MESSAGE pMensaje VIEW-AS ALERT-BOX WARNING.
    RETURN.
END.
/* Generamos la Cabecera y el Detalle */
DISPLAY 'Generando movimientos de reclasificaciones' WITH STREAM-IO NO-BOX.
PAUSE 0.
DO TRANSACTION ON ERROR UNDO, RETURN 'ADM-ERROR':
    pMensaje = "".
    RUN Cabecera-Detalle.
    IF RETURN-VALUE = 'ADM-ERROR' THEN DO:
        IF pMensaje = "" THEN pMensaje = 'ERROR EN EL PROCESO: SE EXTORNAN TODOS LOS MOVIMIENTOS'.
        MESSAGE pMensaje VIEW-AS ALERT-BOX WARNING.
        UNDO, RETURN.
    END.
END.
DISPLAY 'Regenerando Kardex Final' WITH STREAM-IO NO-BOX.
PAUSE 0.
/* Regeneramos Kardex de ambos códigos */
FOR EACH Detalle NO-LOCK:
    DISPLAY 'Recalculando costo promedio 2' detalle.codant detalle.codnew 
        WITH STREAM-IO NO-BOX.
    PAUSE 0.
    RUN aplic\alm\calc-costo-promedio (Detalle.codant, TODAY).
    RUN aplic\alm\calc-costo-promedio (Detalle.codnew, TODAY).
END.

PROCEDURE Reclasificacion:
/* ********************** */
/* De acuerdo al stock es INGRESO (> 0) o SALIDA (<0) */
DEF VAR x-StkAct AS DEC NO-UNDO.
DEF VAR x-StockComprometido AS DEC NO-UNDO.

x-StkAct = Almmmate.stkact.
x-ImpCto = 0.

/* Descargamos el STOCK COMPROMETIDO */
RUN vta2/stock-comprometido-v2 (Detalle.CodAnt, Almmmate.CodAlm, OUTPUT x-StockComprometido).
x-StkAct = x-StkAct - x-StockComprometido.
IF x-StkAct = 0 THEN RETURN.

/* El Costo Unitario se toma del código ORIGEN */
FIND LAST Almstkge WHERE AlmStkge.CodCia = s-codcia
    AND AlmStkge.codmat = Detalle.CodAnt
    AND AlmStkge.Fecha <= TODAY
    NO-LOCK NO-ERROR.
IF AVAILABLE Almstkge THEN x-ImpCto = ABS(x-StkAct) * AlmStkge.CtoUni.

IF x-StkAct < 0 THEN DO:
    RUN Ingreso (Detalle.CodAnt, x-StkAct, Almmmate.CodAlm, 1).
    RUN Salida  (Detalle.CodNew, x-StkAct, Almmmate.CodAlm, Detalle.Factor).
END.
ELSE DO: 
    RUN Salida  (Detalle.CodAnt, x-StkAct, Almmmate.CodAlm, 1).
    RUN Ingreso (Detalle.CodNew, x-StkAct, Almmmate.CodAlm, Detalle.Factor).
END.

END PROCEDURE.

PROCEDURE Ingreso:
/* ************** */
  DEF INPUT PARAMETER pCodMat AS CHAR.
  DEF INPUT PARAMETER pStkAct AS DEC.
  DEF INPUT PARAMETER pCodAlm AS CHAR.
  DEF INPUT PARAMETER pFactor LIKE Detalle.Factor.

  DEF BUFFER B-ITEM FOR ITEM.

  DEF VAR f-Factor-1 AS DEC INIT 1 NO-UNDO.

  DEF VAR x-Item AS INT INIT 1 NO-UNDO.
  FOR EACH B-ITEM BY B-ITEM.NroItm:
      x-Item = B-ITEM.NroItm + 1.
  END.
  /* buscamos el precio promedio */
  FIND Almmmatg WHERE Almmmatg.codcia = s-codcia
      AND Almmmatg.codmat = pCodMat NO-LOCK.
  CREATE ITEM.
  ASSIGN
      ITEM.CodCia = s-CodCia
      ITEM.TipMov = "I"
      ITEM.CodMov = 13
      ITEM.NroItm = x-Item
      ITEM.CodAlm = pCodAlm
      ITEM.CodMat = pCodMat
      ITEM.Factor = f-Factor-1
      ITEM.CodCia = s-codcia
      ITEM.CodUnd = Almmmatg.UndStk
      ITEM.CanDes = ABSOLUTE(pStkAct) * pFactor
      ITEM.ImpCto = x-ImpCto
      ITEM.PreUni = ITEM.ImpCto / ITEM.CanDes.

END PROCEDURE.

PROCEDURE Salida:
/* ************* */
  DEF INPUT PARAMETER pCodMat AS CHAR.
  DEF INPUT PARAMETER pStkAct AS DEC.
  DEF INPUT PARAMETER pCodAlm AS CHAR.
  DEF INPUT PARAMETER pFactor LIKE Detalle.Factor.

  DEF BUFFER B-ITEM FOR ITEM-1.

  DEF VAR f-Factor-1 AS DEC INIT 1 NO-UNDO.

  DEF VAR x-Item AS INT INIT 1 NO-UNDO.
  FOR EACH B-ITEM BY B-ITEM.NroItm:
      x-Item = B-ITEM.NroItm + 1.
  END.
  /* buscamos el precio promedio */
  FIND Almmmatg WHERE Almmmatg.codcia = s-codcia
      AND Almmmatg.codmat = pCodMat NO-LOCK.
  CREATE ITEM-1.
  ASSIGN
      ITEM-1.CodCia = s-CodCia
      ITEM-1.TipMov = "S"
      ITEM-1.CodMov = 13
      ITEM-1.NroItm = x-Item
      ITEM-1.CodAlm = pCodAlm
      ITEM-1.CodMat = pCodMat
      ITEM-1.Factor = f-Factor-1
      ITEM-1.CodCia = s-codcia
      ITEM-1.CodUnd = Almmmatg.UndStk
      ITEM-1.CanDes = ABSOLUTE(pStkAct) * pFactor
      ITEM-1.ImpCto = x-ImpCto
      ITEM-1.PreUni = ITEM-1.ImpCto / ITEM-1.CanDes.

END PROCEDURE.

PROCEDURE Cabecera-Detalle:
/* *********************** */
  DEF VAR x-NroDoc AS INT NO-UNDO.
  DEF VAR s-NroSer AS INT INIT 000 NO-UNDO.
  DEF VAR F-TPOCMB AS DECIMAL NO-UNDO.

  FIND LAST gn-tcmb NO-LOCK.
  f-TPOCMB = gn-tcmb.compra.

  /* Quebramos por Almacén */
  FOR EACH Almacen WHERE Almacen.codcia = s-codcia,
      FIRST Almtdocm NO-LOCK WHERE Almtdocm.CodCia = s-codcia
          AND Almtdocm.CodAlm = Almacen.codalm
          AND Almtdocm.TipMov = "I"
          AND Almtdocm.CodMov = 13:
      IF NOT CAN-FIND(FIRST ITEM WHERE ITEM.codcia = s-codcia AND ITEM.codalm = Almacen.codalm NO-LOCK) 
          THEN NEXT.
      DISPLAY 'Procesando almacén' Almacen.CodAlm WITH STREAM-IO NO-BOX.
      PAUSE 0.
      /* Cabecera */
      CREATE Almcmov.
      ASSIGN 
          x-Nrodoc  = Almacen.CorrIng.
      REPEAT:
          IF NOT CAN-FIND(FIRST Almcmov WHERE Almcmov.codcia = Almtdocm.CodCia
                          AND Almcmov.CodAlm = Almtdocm.CodAlm 
                          AND Almcmov.TipMov = Almtdocm.TipMov
                          AND Almcmov.CodMov = Almtdocm.CodMov
                          AND Almcmov.NroSer = s-NroSer
                          AND Almcmov.NroDoc = x-NroDoc
                          NO-LOCK)
              THEN LEAVE.
          ASSIGN
              x-NroDoc = x-NroDoc + 1.
      END.
      /* MOVIMIENTO ORIGEN */
      ASSIGN 
          Almcmov.CodCia = Almtdocm.CodCia 
          Almcmov.CodAlm = Almtdocm.CodAlm 
          Almcmov.TipMov = Almtdocm.TipMov
          Almcmov.CodMov = Almtdocm.CodMov
          Almcmov.NroSer = S-NROSER
          Almcmov.Nrodoc = x-NroDoc
          Almcmov.FchDoc = TODAY
          Almcmov.HorSal = STRING(TIME,"HH:MM:SS")
          Almcmov.CodMon = 1        /* SOLES */
          Almcmov.TpoCmb  = F-TPOCMB.
      ASSIGN
          Almacen.CorrIng = x-NroDoc + 1.
      /* MOVIMIENTO REFLEJO */
      ASSIGN 
          x-Nrodoc  = Almacen.CorrSal.
      REPEAT:
          IF NOT CAN-FIND(FIRST Almcmov WHERE Almcmov.codcia = Almtdocm.CodCia
                          AND Almcmov.CodAlm = Almtdocm.CodAlm 
                          AND Almcmov.TipMov = "S"
                          AND Almcmov.CodMov = Almtdocm.CodMov
                          AND Almcmov.NroSer = s-NroSer
                          AND Almcmov.NroDoc = x-NroDoc
                          NO-LOCK)
              THEN LEAVE.
          ASSIGN
              x-NroDoc = x-NroDoc + 1.
      END.
      CREATE B-CMOV.
      BUFFER-COPY Almcmov TO B-CMOV
          ASSIGN
          B-CMOV.TipMov = "S"
          B-CMOV.Nrodoc = x-NroDoc
          B-CMOV.NroRef = STRING(Almcmov.nroser, '999') + STRING(Almcmov.nrodoc, '9999999')
          B-CMOV.NroRf1 = Almcmov.NroRf1
          B-CMOV.NroRf2 = STRING(Almcmov.nroser, '999') + STRING(Almcmov.nrodoc, '9999999').
      ASSIGN
          Almacen.CorrSal = x-NroDoc + 1.
      ASSIGN 
          Almcmov.NroRef = STRING(B-CMOV.nroser, '999') + STRING(B-CMOV.nrodoc, '9999999').
      ASSIGN 
          Almcmov.usuario = S-USER-ID.
      N-Itm = 0.
      RUN Genera-Detalle.
      IF RETURN-VALUE = 'ADM-ERROR' THEN UNDO, RETURN 'ADM-ERROR'.
  END.
  /* Actualizamos el Catálogo */
  DEF BUFFER B-MATG FOR Almmmatg.
  DEF BUFFER B-MATG1 FOR Almmmat1.
  FOR EACH Detalle NO-LOCK:
      DISPLAY 'Cerrando Catálogo' Detalle.CodAnt Detalle.CodNew WITH STREAM-IO NO-BOX.
      PAUSE 0.
      FIND Almmmatg WHERE Almmmatg.codcia = s-codcia
          AND Almmmatg.codmat = Detalle.CodAnt
          EXCLUSIVE-LOCK NO-ERROR.
      IF NOT AVAILABLE Almmmatg THEN UNDO, RETURN 'ADM-ERROR'.
      FIND B-MATG WHERE B-MATG.codcia = s-codcia
          AND B-MATG.codmat = Detalle.CodNew
          EXCLUSIVE-LOCK NO-ERROR.
      IF NOT AVAILABLE B-MATG THEN UNDO, RETURN 'ADM-ERROR'.
      ASSIGN
          Almmmatg.TpoArt = "D".
/*       ASSIGN                                                                 */
/*           B-MATG.CodBrr = Almmmatg.CodBrr.                                   */
/*       FIND Almmmat1 OF Almmmatg NO-LOCK NO-ERROR.                            */
/*       IF NOT AVAILABLE Almmmat1 THEN NEXT.                                   */
/*       FIND B-MATG1 OF B-MATG NO-LOCK NO-ERROR.                               */
/*       IF AVAILABLE B-MATG1 THEN DO:                                          */
/*           FIND B-MATG1 EXCLUSIVE-LOCK NO-ERROR.                              */
/*           IF NOT AVAILABLE B-MATG1 THEN UNDO, RETURN 'ADM-ERROR'.            */
/*           DELETE B-MATG1.                                                    */
/*       END.                                                                   */
/*       CREATE B-MATG1.                                                        */
/*       BUFFER-COPY Almmmat1 TO B-MATG1 ASSIGN B-MATG1.CodMat = B-MATG.CodMat. */
/*       FIND CURRENT Almmmat1 EXCLUSIVE-LOCK NO-ERROR.                         */
/*       IF NOT AVAILABLE Almmmat1 THEN UNDO, RETURN 'ADM-ERROR'.               */
/*       DELETE Almmmat1.                                                       */
/*       ASSIGN                                                                 */
/*           Almmmatg.CodBrr = ''.                                              */
  END.

END PROCEDURE.

PROCEDURE Genera-Detalle:
/* ********************* */
    DEF VAR r-Rowid AS ROWID NO-UNDO.

    /* INGRESO */
    FOR EACH ITEM WHERE ITEM.codcia = s-codcia AND ITEM.codalm = Almacen.codalm:
        N-Itm = N-Itm + 1.
        CREATE almdmov.
        BUFFER-COPY ITEM TO Almdmov
            ASSIGN 
            Almdmov.CodCia = Almcmov.CodCia 
            Almdmov.CodAlm = Almcmov.CodAlm 
            Almdmov.TipMov = Almcmov.TipMov 
            Almdmov.CodMov = Almcmov.CodMov 
            Almdmov.NroSer = Almcmov.NroSer 
            Almdmov.NroDoc = Almcmov.NroDoc 
            Almdmov.CodMon = Almcmov.CodMon 
            Almdmov.FchDoc = Almcmov.FchDoc 
            Almdmov.TpoCmb = Almcmov.TpoCmb
            Almdmov.NroItm = N-Itm
            Almdmov.CodAjt = ''
            Almdmov.HraDoc = almcmov.HorSal
            R-ROWID = ROWID(Almdmov).
        FIND Almmmatg OF Almdmov NO-LOCK.
        ASSIGN
            Almdmov.CodUnd = Almmmatg.UndStk.
        FIND FIRST Almtmovm WHERE Almtmovm.CodCia = Almdmov.CodCia 
            AND  Almtmovm.Tipmov = Almdmov.TipMov 
            AND  Almtmovm.Codmov = Almdmov.CodMov 
            NO-LOCK NO-ERROR.
        IF AVAILABLE Almtmovm AND Almtmovm.PidPCo AND Almtmovm.TipMov = "I"
            THEN ASSIGN Almdmov.CodAjt = "A".
        ELSE ASSIGN Almdmov.CodAjt = ''.

        /* ************************************************ */
        RUN ALM\ALMACSTK (R-ROWID).
        IF RETURN-VALUE = 'ADM-ERROR' THEN DO:
            pMensaje = 'ERROR INGRESO almacstk almacen: ' + almacen.codalm  + ' codigo: ' + almdmov.codmat.
            UNDO, RETURN 'ADM-ERROR'.
        END.
        RUN ALM\ALMACPR1 (R-ROWID,"U").
        IF RETURN-VALUE = 'ADM-ERROR' THEN DO:
            pMensaje = 'ERROR INGRESO almacpr1 almacen: ' + almacen.codalm  + ' codigo: ' + almdmov.codmat.
            UNDO, RETURN 'ADM-ERROR'.
        END.
    END.
    
    /* SALIDA */
    FOR EACH ITEM-1 WHERE ITEM-1.codcia = s-codcia AND ITEM-1.codalm = Almacen.codalm:
        CREATE almdmov.
        BUFFER-COPY ITEM-1 TO Almdmov
            ASSIGN 
            Almdmov.CodCia = B-CMOV.CodCia 
            Almdmov.CodAlm = B-CMOV.CodAlm 
            Almdmov.TipMov = B-CMOV.TipMov 
            Almdmov.CodMov = B-CMOV.CodMov 
            Almdmov.NroSer = B-CMOV.NroSer 
            Almdmov.NroDoc = B-CMOV.NroDoc 
            Almdmov.CodMon = B-CMOV.CodMon 
            Almdmov.FchDoc = B-CMOV.FchDoc 
            Almdmov.TpoCmb = B-CMOV.TpoCmb
            Almdmov.NroItm = N-Itm
            Almdmov.CodAjt = ''
            Almdmov.HraDoc = B-CMOV.HorRcp
            R-ROWID = ROWID(Almdmov).
        FIND Almmmatg OF Almdmov NO-LOCK.
        ASSIGN
            Almdmov.CodUnd = Almmmatg.UndStk.
        FIND FIRST Almtmovm WHERE Almtmovm.CodCia = Almdmov.CodCia 
            AND  Almtmovm.Tipmov = Almdmov.TipMov 
            AND  Almtmovm.Codmov = Almdmov.CodMov 
            NO-LOCK NO-ERROR.
        IF AVAILABLE Almtmovm AND Almtmovm.PidPCo AND Almtmovm.TipMov = "I"
            THEN ASSIGN Almdmov.CodAjt = "A".
        ELSE ASSIGN Almdmov.CodAjt = ''.
        RUN alm/almdcstk (R-ROWID).
        IF RETURN-VALUE = 'ADM-ERROR' THEN UNDO, RETURN 'ADM-ERROR'.
        RUN ALM\ALMACPR1 (R-ROWID,"U").
        IF RETURN-VALUE = 'ADM-ERROR' THEN UNDO, RETURN 'ADM-ERROR'.
    END.

    RETURN 'OK'.

END PROCEDURE.


PROCEDURE Articulos-por-Almacen:
/* **************************** */

FOR EACH ITEM NO-LOCK:
    IF NOT CAN-FIND(FIRST Almmmate WHERE Almmmate.codcia = s-codcia
                    AND Almmmate.codalm = ITEM.codalm
                    AND Almmmate.codmat = ITEM.codmat NO-LOCK)
        THEN DO:
        CREATE Almmmate.
        ASSIGN
            Almmmate.codcia = s-codcia
            Almmmate.codalm = ITEM.codalm
            Almmmate.codmat = ITEM.codmat
            Almmmate.FchIng = TODAY.
    END.

END.
FOR EACH ITEM-1 NO-LOCK:
    IF NOT CAN-FIND(FIRST Almmmate WHERE Almmmate.codcia = s-codcia
                    AND Almmmate.codalm = ITEM-1.codalm
                    AND Almmmate.codmat = ITEM-1.codmat NO-LOCK)
        THEN DO:
        CREATE Almmmate.
        ASSIGN
            Almmmate.codcia = s-codcia
            Almmmate.codalm = ITEM-1.codalm
            Almmmate.codmat = ITEM-1.codmat
            Almmmate.FchIng = TODAY.
    END.

END.



END PROCEDURE.

DEF VAR s-codcia AS INT INIT 001.
DEF VAR s-coddiv AS CHAR INIT '00000'.
DEF VAR s-nroser AS INT INIT 0000.
DEF VAR s-codalm AS CHAR INIT '21'.        /* <<< OJO <<< */
DEF VAR s-tipmov AS CHAR INIT 'S'.
DEF VAR s-codmov AS INT INIT 03.
DEF VAR s-codref AS CHAR.
DEF VAR s-user-id AS CHAR INIT 'SISTEMAS'.

DEF VAR x-NroDoc LIKE Almcmov.nrodoc INIT 0 NO-UNDO.

DEF TEMP-TABLE ITEM LIKE almdmov.
DEF TEMP-TABLE T-ITEM LIKE almdmov.
DEF BUFFER CMOV FOR Almcmov.

FIND Almtdocm WHERE  Almtdocm.CodCia = s-codcia
    AND Almtdocm.CodAlm = s-codalm
    AND Almtdocm.TipMov = s-tipmov
    AND Almtdocm.CodMov = s-codmov
    NO-LOCK.

EMPTY TEMP-TABLE ITEM.
EMPTY TEMP-TABLE T-ITEM.
RUN Carga-Detalle.

DO TRANSACTION ON ERROR UNDO, RETURN "ADM-ERROR" ON STOP UNDO, RETURN "ADM-ERROR":
    /* Buscamos el correlativo de almacenes */
    FIND Almacen WHERE Almacen.CodCia = S-CODCIA 
        AND Almacen.CodAlm = s-CodAlm 
        EXCLUSIVE-LOCK NO-ERROR.
    IF NOT AVAILABLE Almacen THEN DO: 
        MESSAGE 'NO se pudo bloquer el correlativo por almacen' VIEW-AS ALERT-BOX ERROR.
        UNDO, RETURN 'ADM-ERROR'.
    END.
    ASSIGN 
        x-Nrodoc  = Almacen.CorrSal
        Almacen.CorrSal = Almacen.CorrSal + 1.
    CREATE Almcmov.
    ASSIGN 
          Almcmov.CodCia = s-CodCia 
          Almcmov.CodAlm = s-CodAlm 
          Almcmov.AlmDes = '21e'     /* <<< OJO <<< */
          Almcmov.TipMov = s-TipMov
          Almcmov.CodMov = s-CodMov
          Almcmov.FlgSit = "T"
          Almcmov.FchDoc = TODAY
          Almcmov.HorSal = STRING(TIME,"HH:MM")
          Almcmov.HraDoc = STRING(TIME,"HH:MM")
          Almcmov.NroSer = s-nroser
          Almcmov.NroDoc = x-NroDoc
          Almcmov.CodRef = s-CodRef
          Almcmov.usuario = 'SISTEMAS'
          Almcmov.codmon = 1
          Almcmov.tpocmb = 2.8.

    RUN Genera-Detalle.
    IF RETURN-VALUE = 'ADM-ERROR' THEN UNDO, RETURN 'ADM-ERROR'.

END.
/* DESBLOQUEAMOS LOS CORRELATIVOS */
RELEASE Faccorre.
RELEASE Almacen.


PROCEDURE Carga-Detalle:
/* ******************** */

DEF VAR x-Item AS INT.
DEF VAR x-linea AS CHAR.
DEF VAR pComprometido AS DEC.

INPUT FROM c:\tmp\transferencias.prn.
REPEAT:
    IMPORT UNFORMATTED x-linea.
    IF x-linea <> '' THEN DO:
        x-Item = x-Item + 1.
        CREATE ITEM.
        ASSIGN
            ITEM.nroitm = x-item
            ITEM.codcia = s-codcia
            ITEM.codmat = SUBSTRING(x-linea,1,6)
            ITEM.candes = DEC(SUBSTRING(x-linea,11))
            NO-ERROR.
        IF ITEM.candes <= 0 THEN DELETE ITEM.
    END.
END.
INPUT CLOSE.

FOR EACH ITEM, FIRST Almmmatg OF ITEM NO-LOCK:
    ASSIGN
        ITEM.factor = 1
        ITEM.codund = almmmatg.undbas.
END.

END PROCEDURE.

PROCEDURE Genera-Detalle:
/* ********************* */

  DEF VAR r-Rowid AS ROWID NO-UNDO.
  DEF VAR pComprometido AS DEC.
  DEF VAR x-Item AS INT INIT 0 NO-UNDO.

  DO TRANSACTION ON ERROR UNDO, RETURN "ADM-ERROR" ON STOP UNDO, RETURN "ADM-ERROR":
      FOR EACH ITEM BY ITEM.NroItm:
          CREATE almdmov.
          ASSIGN Almdmov.CodCia = Almcmov.CodCia 
                 Almdmov.CodAlm = Almcmov.CodAlm 
                 Almdmov.TipMov = Almcmov.TipMov 
                 Almdmov.CodMov = Almcmov.CodMov 
                 Almdmov.NroSer = Almcmov.NroSer
                 Almdmov.NroDoc = Almcmov.NroDoc 
                 Almdmov.CodMon = Almcmov.CodMon 
                 Almdmov.FchDoc = Almcmov.FchDoc 
                 Almdmov.HraDoc = Almcmov.HraDoc
                 Almdmov.TpoCmb = Almcmov.TpoCmb
                 Almdmov.codmat = ITEM.codmat
                 Almdmov.CanDes = ITEM.CanDes
                 Almdmov.CodUnd = ITEM.CodUnd
                 Almdmov.Factor = ITEM.Factor
                 Almdmov.ImpCto = ITEM.ImpCto
                 Almdmov.PreUni = ITEM.PreUni
                 Almdmov.AlmOri = Almcmov.AlmDes 
                 Almdmov.CodAjt = ''
                 Almdmov.HraDoc = HorSal
                 Almdmov.NroItm = x-Item
                 R-ROWID = ROWID(Almdmov).
          x-Item = x-Item + 1.
          RUN alm/almdcstk (R-ROWID).
          IF RETURN-VALUE = 'ADM-ERROR' THEN UNDO, RETURN 'ADM-ERROR'.
          RUN alm/almacpr1 (R-ROWID, "U").
          IF RETURN-VALUE = 'ADM-ERROR' THEN UNDO, RETURN 'ADM-ERROR'.
          /* Se anulan los items que se pueden descargar */
          DELETE ITEM.
      END.
  END.

  END PROCEDURE.

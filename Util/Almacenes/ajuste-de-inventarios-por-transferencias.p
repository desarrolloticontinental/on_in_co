DEF VAR s-codcia AS INT INIT 001.
DEF VAR x-NroDoc AS INT INIT 000001.
DEF VAR x-nroItm AS INT.
DEF VAR r-Rowid AS ROWID NO-UNDO.

DEF TEMP-TABLE t-cmov LIKE almcmov.
DEF TEMP-TABLE t-dmov LIKE almdmov.

DEF TEMP-TABLE detalle
    FIELD codalm AS CHAR
    FIELD almdes AS CHAR
    FIELD codmat AS CHAR
    FIELD candes AS DEC
    INDEX llave01 AS PRIMARY codalm almdes codmat.

DEF VAR x-linea AS CHAR FORMAT 'x(100)'.

/* ----------------------------------------------------- */
/*          NO TE OLVIDES CAMBIAR ALGUNAS VARIABLES      */
/* ----------------------------------------------------- */
INPUT FROM c:\tmp\ajuste-cissac.prn.     /* <<< OJO <<< */
REPEAT :
    IMPORT UNFORMATTED x-linea.
    IF x-linea <> '' THEN DO:
        FIND detalle WHERE codalm = SUBSTRING(x-linea,1,3)
            AND almdes = SUBSTRING(x-linea,11,3)
            AND codmat = SUBSTRING(x-linea,21,6)
            NO-ERROR.
        IF NOT AVAILABLE detalle THEN DO:
            CREATE detalle.
            ASSIGN
                codalm = SUBSTRING(x-linea,1,3)
                almdes = SUBSTRING(x-linea,11,3)
                codmat = SUBSTRING(x-linea,21,6).
            ASSIGN
                candes = candes + DECIMAL(SUBSTRING(x-linea,27)).
        END.
    END.
END.

FOR EACH detalle NO-LOCK,
    FIRST Almmmatg WHERE Almmmatg.codcia = s-codcia
    AND Almmmatg.codmat = detalle.codmat
    BREAK BY detalle.codalm BY detalle.almdes:
    IF FIRST-OF(detalle.codalm) OR FIRST-OF(detalle.almdes) THEN DO:
        CREATE t-cmov.
        ASSIGN
            t-cmov.CodCia = s-codcia
            t-cmov.CodAlm = detalle.codalm
            t-cmov.AlmDes = detalle.almdes
            t-cmov.FchDoc = TODAY
            t-cmov.FlgSit = "T"
            t-cmov.HorSal = STRING(TIME, 'HH:MM')
            t-cmov.HraDoc = STRING(TIME, 'HH:MM')
            t-cmov.NroDoc = x-NroDoc
            t-cmov.TipMov = "S"
            t-cmov.CodMov = 03
            t-cmov.usuario = "SISTEMAS".
        x-NroDoc = x-NroDoc + 1.
        x-NroItm = 1.
    END.
    CREATE t-dmov.
    BUFFER-COPY t-cmov 
        TO t-dmov
        ASSIGN
            t-dmov.almori = t-cmov.almdes
            t-dmov.codmat = detalle.codmat
            t-dmov.candes = detalle.candes
            t-dmov.factor = 1
            t-dmov.codund = Almmmatg.undstk
            t-dmov.nroitm = x-nroitm.
    x-nroitm = x-nroitm + 1.
END.
/* FOR EACH t-cmov:                                                     */
/*     DISPLAY t-cmov.tipmov t-cmov.codmov t-cmov.codalm t-cmov.almdes. */
/*     FOR EACH t-dmov OF t-cmov:                                       */
/*         DISPLAY t-dmov.codmat t-dmov.candes t-dmov.codund.           */
/*     END.                                                             */
/* END.                                                                 */
/* RETURN.                                                              */
FOR EACH t-cmov TRANSACTION ON ERROR UNDO, RETRY:
      /* Buscamos el correlativo de almacenes */
      FIND Almacen WHERE Almacen.CodCia = S-CODCIA 
          AND Almacen.CodAlm = t-cmov.CodAlm 
          EXCLUSIVE-LOCK NO-ERROR.
      IF NOT AVAILABLE Almacen THEN DO: 
          MESSAGE 'NO se pudo bloquer el correlativo por almacen' SKIP
              t-cmov.codalm
              VIEW-AS ALERT-BOX ERROR.
          UNDO, RETRY.
      END.
      CREATE almcmov.
      BUFFER-COPY t-cmov
          TO almcmov
          ASSIGN 
            almcmov.nrodoc = Almacen.CorrSal.
      ASSIGN
          Almacen.CorrSal = Almacen.CorrSal + 1.
      FOR EACH t-dmov OF t-cmov NO-LOCK:
          CREATE almdmov.
          BUFFER-COPY t-dmov
              TO almdmov
              ASSIGN
                almdmov.nrodoc = almcmov.nrodoc.
          R-ROWID = ROWID(Almdmov).
          RUN alm/almdcstk (R-ROWID).
          IF RETURN-VALUE = 'ADM-ERROR' THEN UNDO, RETRY.
          RUN alm/almacpr1 (R-ROWID, "U").
          IF RETURN-VALUE = 'ADM-ERROR' THEN UNDO, RETRY.
      END.
END.

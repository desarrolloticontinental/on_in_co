DEF TEMP-TABLE t-dmov LIKE almdmov.
DEF VAR r-Rowid AS ROWID NO-UNDO.
DEF VAR pComprometido AS DEC.
DEF VAR s-codcia AS INT INIT 001.
DEF BUFFER B-CMOV FOR almcmov.
DEF BUFFER B-DMOV FOR almdmov.
DEF BUFFER ITEM FOR almdmov.
DEFINE VARIABLE N-Itm AS INTEGER NO-UNDO.

FOR EACH almcmov NO-LOCK WHERE codcia = 1
    AND codalm = '10'
    AND tipmov = 's'
    AND codmov = 14:
    DISPLAY almcmov.fchdoc almcmov.nrodoc.
    PAUSE 0.
    FOR EACH t-dmov:
        DELETE t-dmov.
    END.
    /* buscamos la salida */
    FIND B-CMOV WHERE B-CMOV.codcia = Almcmov.codcia
          AND B-CMOV.codalm = Almcmov.codalm
          AND B-CMOV.tipmov = "I"
          AND B-CMOV.codmov = Almcmov.codmov
          AND B-CMOV.nroser = INTEGER(SUBSTRING(Almcmov.nroref,1,3))
          AND B-CMOV.nrodoc = INTEGER(SUBSTRING(Almcmov.nroref,4))
          NO-LOCK.
    /* cargamos el temporal */
    FOR EACH ITEM OF almcmov NO-LOCK:
          FIND t-dmov WHERE t-dmov.codcia = B-CMOV.codcia
              AND t-dmov.codalm = B-CMOV.codalm
              AND t-dmov.tipmov = B-CMOV.tipmov
              AND t-dmov.codmov = B-CMOV.codmov
              AND t-dmov.nroser = B-CMOV.nroser
              AND t-dmov.nrodoc = B-CMOV.nrodoc
              AND t-dmov.codmat = ITEM.codant
              NO-ERROR.
          IF NOT AVAILABLE t-dmov THEN DO:
              N-Itm = N-Itm + 1.
              CREATE t-dmov.
              ASSIGN 
                  t-dmov.CodCia = B-CMOV.CodCia 
                  t-dmov.CodAlm = B-CMOV.CodAlm 
                  t-dmov.TipMov = B-CMOV.TipMov 
                  t-dmov.CodMov = B-CMOV.CodMov 
                  t-dmov.NroSer = B-CMOV.NroSer 
                  t-dmov.NroDoc = B-CMOV.NroDoc 
                  t-dmov.CodMon = B-CMOV.CodMon 
                  t-dmov.FchDoc = B-CMOV.FchDoc 
                  t-dmov.TpoCmb = B-CMOV.TpoCmb
                  t-dmov.codmat = ITEM.codant      /* OJO */
                  t-dmov.codant = ITEM.codmat      /* OJO */
                  t-dmov.CodUnd = ITEM.CodUnd
                  t-dmov.Factor = ITEM.PreBas      /* OJO */
                  t-dmov.NroItm = N-Itm
                  t-dmov.CodAjt = 'A'
                  t-dmov.HraDoc = B-CMOV.HorRcp
                  R-ROWID = ROWID(t-dmov).
          END.
          /* RHC 28.01.11 Verificamos en las tablas de reclasificaciones */
          FIND Almdrecl WHERE Almdrecl.codcia = s-codcia
              AND Almdrecl.codmatr = ITEM.codant
              AND Almdrecl.codmat  = ITEM.codmat
              NO-LOCK NO-ERROR.
          IF NOT AVAILABLE Almdrecl THEN DO:
              ASSIGN 
                  t-dmov.CanDes = t-dmov.candes + ITEM.CanDes
                  t-dmov.ImpCto = ITEM.ImpCto
                  t-dmov.PreUni = (ITEM.ImpCto / ITEM.CanDes).
          END.
          ELSE DO:
              ASSIGN 
                  t-dmov.CanDes = t-dmov.candes + ITEM.CanDes / Almdrecl.Factor
                  t-dmov.ImpCto = t-dmov.ImpCto + ITEM.ImpCto
                  t-dmov.PreUni = t-dmov.ImpCto / t-dmov.CanDes.
          END.
          /* fin de valorizaciones */
          FIND Almmmatg OF t-dmov NO-LOCK.
          ASSIGN
              t-dmov.CodUnd = Almmmatg.UndStk.
          FIND FIRST Almtmovm WHERE Almtmovm.CodCia = B-CMOV.CodCia 
              AND  Almtmovm.Tipmov = B-CMOV.TipMov 
              AND  Almtmovm.Codmov = B-CMOV.CodMov 
              NO-LOCK NO-ERROR.
          IF AVAILABLE Almtmovm AND Almtmovm.PidPCo 
              THEN ASSIGN t-dmov.CodAjt = "A".
          ELSE ASSIGN t-dmov.CodAjt = ''.
    END.
    /* borramos info */
    FOR EACH B-DMOV OF B-CMOV:
        DELETE B-DMOV.
    END.
    /* cargamos temporal */
    FOR EACH t-dmov:
        CREATE B-DMOV.
        BUFFER-COPY t-dmov TO B-DMOV.
    END.
END.

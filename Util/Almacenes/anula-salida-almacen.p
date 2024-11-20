/* Anulación de Salidas del Almacén */
DEF NEW SHARED VAR s-codcia AS INT INIT 001.
DEF NEW SHARED VAR s-codalm AS CHAR INIT '11'.

DEF VAR s-tipmov AS CHAR INIT 'S'.
DEF VAR s-codmov AS INT INIT 03.

FIND almcmov WHERE almcmov.codcia = s-codcia 
    AND almcmov.codalm = s-codalm
    AND almcmov.tipmov = s-tipmov
    AND almcmov.codmov = s-codmov
    AND almcmov.nroser = 014
    AND almcmov.nrodoc = 112634
    NO-LOCK NO-ERROR.
IF NOT AVAILABLE almcmov THEN DO:
    MESSAGE 'No encontrado'.
    RETURN.
END.
RUN local-delete-record.
FIND CURRENT Almcmov NO-LOCK NO-ERROR.


PROCEDURE local-delete-record:

    IF Almcmov.FlgEst = 'A' THEN DO:
       MESSAGE "Documento Anulado" VIEW-AS ALERT-BOX WARNING.
       /*RETURN "ADM-ERROR".*/
    END.
    IF Almcmov.FlgSit  = "R" THEN DO:
       MESSAGE "Transferencia recepcionada, no puede se modificada" VIEW-AS ALERT-BOX ERROR.
       RETURN "ADM-ERROR".
    END.

    /* RHC 21/10/2013 Verificamos H/R */
    DEF VAR pHojRut   AS CHAR.
    DEF VAR pFlgEst-1 AS CHAR.
    DEF VAR pFlgEst-2 AS CHAR.
    DEF VAR pFchDoc   AS DATE.

    RUN dist/p-rut002 ( "TRF",
                        "",
                        "",
                        almcmov.CodAlm,
                        almcmov.TipMov,
                        almcmov.CodMov,
                        almcmov.NroSer,
                        almcmov.NroDoc,
                        OUTPUT pHojRut,
                        OUTPUT pFlgEst-1,     /* de Di-RutaC */
                        OUTPUT pFlgEst-2,     /* de Di-RutaG */
                        OUTPUT pFchDoc).
    IF pFlgEst-1 = "P" OR (pFlgEst-2 = "C" AND pFlgEst-2 <> "C")
        THEN DO:
        MESSAGE "NO se puede anular" SKIP
            "Revisar la Hoja de Ruta:" pHojRut
            VIEW-AS ALERT-BOX ERROR.
        RETURN "ADM-ERROR".
    END.

    /* Solo marcamos el FlgEst como Anulado */
    DO TRANSACTION ON ERROR UNDO, RETURN 'ADM-ERROR' ON STOP UNDO, RETURN 'ADM-ERROR':
       RUN Restaura-Pedido.
       IF RETURN-VALUE = 'ADM-ERROR' THEN UNDO, RETURN 'ADM-ERROR'.

       /*Valida almacenes*/
       IF almcmov.codalm <> '11T' THEN DO:
           IF almcmov.almdes <> '11T' THEN DO:
               RUN alm/ing-trf-vir-del (ROWID(Almcmov), '999').
               IF RETURN-VALUE = 'ADM-ERROR' THEN UNDO, RETURN 'ADM-ERROR'.
           END.
       END.

       RUN Borra-Detalle.
       IF RETURN-VALUE = 'ADM-ERROR' 
       THEN DO:
          MESSAGE 'No se pudo eliminar el detalle'
              VIEW-AS ALERT-BOX ERROR.
          UNDO, RETURN 'ADM-ERROR'.
       END.

       FIND CURRENT Almcmov EXCLUSIVE-LOCK NO-ERROR.
       IF NOT AVAILABLE Almcmov
       THEN DO:
           UNDO, RETURN "ADM-ERROR".
       END.
       ASSIGN 
           Almcmov.FlgEst = 'A'
           Almcmov.Observ = "      A   N   U   L   A   D   O       "
           Almcmov.usuario = 'ADMIN'
           Almcmov.FchAnu = TODAY.
    END.

END.


PROCEDURE Restaura-Pedido:

DO TRANSACTION ON ERROR UNDO, RETURN "ADM-ERROR" ON STOP UNDO, RETURN "ADM-ERROR":
    CASE Almcmov.codref:
        WHEN 'R/A' THEN DO:     /* Reposicion Automatica */
            FIND Almcrepo WHERE Almcrepo.codcia = s-codcia
                AND Almcrepo.nroser = INTEGER(SUBSTRING(Almcmov.nrorf1,1,3))
                AND Almcrepo.nrodoc = INTEGER(SUBSTRING(Almcmov.nrorf1,4))
                EXCLUSIVE-LOCK NO-ERROR.
            IF NOT AVAILABLE Almcrepo THEN DO:
                MESSAGE 'No se pudo bloquear el Pedido por Reposicion Automatica'
                    VIEW-AS ALERT-BOX ERROR.
                RETURN "ADM-ERROR".
            END.
            FOR EACH almdmov OF almcmov NO-LOCK:
                FIND Almdrepo OF Almcrepo WHERE Almdrepo.codmat = Almdmov.codmat
                    EXCLUSIVE-LOCK NO-ERROR.
                IF NOT AVAILABLE Almdrepo THEN DO:
                    MESSAGE 'No se pudo bloquear el detalle del Pedido por Reposicion Automatica'
                        VIEW-AS ALERT-BOX ERROR.
                    UNDO, RETURN "ADM-ERROR".
                END.
                Almdrepo.CanAten = Almdrepo.CanAten - Almdmov.candes.
                RELEASE Almdrepo.
            END.
            ASSIGN
                almcrepo.FlgEst = 'P'
                almcrepo.HorAct = STRING(TIME, 'HH:MM')
                almcrepo.FecAct = TODAY
                almcrepo.UsrAct = 'ADMIN'.
            RELEASE Almcrepo.
        END.
        WHEN 'PED' THEN DO:
            FIND Almcrequ WHERE Almcrequ.CodCia = Almcmov.codcia 
                AND Almcrequ.CodAlm = Almcmov.AlmDes 
                AND Almcrequ.NroSer = INTEGER(SUBSTRING(Almcmov.NroRf1,1,3)) 
                AND Almcrequ.NroDoc = INTEGER(SUBSTRING(Almcmov.NroRf1,4,6)) 
                EXCLUSIVE-LOCK NO-ERROR.
            IF NOT AVAILABLE Almcrequ THEN DO:
                MESSAGE "Registro de Referencia no DISPONIBLE" VIEW-AS ALERT-BOX.
                RETURN "ADM-ERROR".
            END.
            ASSIGN
                Almcrequ.FlgEst = "P".
            FOR EACH Almdmov OF Almcmov NO-LOCK:
                FIND Almdrequ WHERE Almdrequ.CodCia = Almdmov.CodCia 
                    AND Almdrequ.CodAlm = Almcmov.Almdes 
                    AND Almdrequ.NroSer = INTEGER(SUBSTRING(Almcmov.NroRf1,1,3))
                    AND Almdrequ.NroDoc = INTEGER(SUBSTRING(Almcmov.NroRf1,4,6))
                    AND Almdrequ.CodMat = almdmov.codmat EXCLUSIVE-LOCK NO-ERROR.
                IF NOT AVAILABLE Almdrequ THEN DO:
                    MESSAGE 'No se pudo bloquear el detalle del Pedido por Reposicion Manual'
                        VIEW-AS ALERT-BOX ERROR.
                    UNDO, RETURN "ADM-ERROR".
                END.
                Almdrequ.CanDes = Almdrequ.CanDes - Almdmov.candes.
                RELEASE Almdrequ.
            END.
            RELEASE Almcrequ.
        END.
        WHEN 'O/D' THEN DO:
            /* Actualiza Detalle de la Orden de Despacho */
            FOR EACH Almdmov OF Almcmov NO-LOCK:
                FIND Facdpedi WHERE Facdpedi.codcia = s-codcia
                    AND Facdpedi.coddoc = Almcmov.codref
                    AND Facdpedi.nroped = Almcmov.nrorf3
                    AND Facdpedi.codmat = Almdmov.codmat
                    EXCLUSIVE-LOCK NO-WAIT NO-ERROR.
                IF NOT AVAILABLE Facdpedi THEN DO:
                    MESSAGE "NO se pudo bloquear el producto" almdmov.codmat "en la orden de despacho"
                        VIEW-AS ALERT-BOX ERROR.
                    UNDO, RETURN "ADM-ERROR".
                END.
                ASSIGN 
                    FacDPedi.CanAte = FacDPedi.CanAte - Almdmov.CanDes.
                IF (FacDPedi.CanPed - FacDPedi.CanAte) <= 0 THEN FacDPedi.FlgEst = "C".
                ELSE FacDPedi.FlgEst = "P".
            END.
            /* Abre la O/D */
            FIND Faccpedi WHERE Faccpedi.codcia = s-codcia
                AND Faccpedi.coddoc = Almcmov.codref
                AND Faccpedi.nroped = Almcmov.nrorf3
                EXCLUSIVE-LOCK NO-ERROR.
            IF NOT AVAILABLE Faccpedi THEN DO:
                MESSAGE 'No se pudo actualizar la cabecera de Ordenes de Despacho:' ALmcmov.codref Almcmov.nroref
                    VIEW-AS ALERT-BOX ERROR.
                UNDO, RETURN "ADM-ERROR".
            END.
            FIND FIRST Facdpedi OF Faccpedi WHERE (Facdpedi.CanPed > Facdpedi.CanAte) NO-LOCK NO-ERROR.
            IF AVAILABLE Facdpedi THEN Faccpedi.FlgEst = "P".
            ELSE Faccpedi.FlgEst = "C".
        END.
    END CASE.
END.



END PROCEDURE.



PROCEDURE Borra-Detalle:

    DEF VAR r-Rowid AS ROWID NO-UNDO.

    DO TRANSACTION ON ERROR UNDO, RETURN "ADM-ERROR" ON STOP UNDO, RETURN "ADM-ERROR":
        /* Eliminamos el detalle para el almacen de Origen */
        FOR EACH Almdmov OF Almcmov:
          ASSIGN R-ROWID = ROWID(Almdmov).
          RUN alm/almacstk (R-ROWID).
          IF RETURN-VALUE = 'ADM-ERROR' THEN UNDO, RETURN 'ADM-ERROR'.
          /* RHC 30.03.04 REACTIVAMOS RUTINA */
          RUN alm/almacpr1 (R-ROWID, "D").
          IF RETURN-VALUE = 'ADM-ERROR' THEN UNDO, RETURN 'ADM-ERROR'.
          DELETE Almdmov.
        END.
    END.

END PROCEDURE.

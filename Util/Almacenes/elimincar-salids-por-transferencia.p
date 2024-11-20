/* ELIMINAR SALIDAS POR TRANSFERENCIAS */

DEF VAR s-codcia AS INT INIT 001 NO-UNDO.
DEF VAR s-codalm AS CHAR INIT '60' NO-UNDO.
DEF VAR s-almdes AS CHAR INIT '60e' NO-UNDO.
DEF VAR s-fchdoc AS DATE NO-UNDO.
DEF VAR s-user-id AS CHAR INIT 'ADMIN' NO-UNDO.
DEF VAR s-tipmov AS CHAR INIT 'S' NO-UNDO.
DEF VAR s-codmov AS INT INIT 03 NO-UNDO.

ASSIGN
    s-fchdoc = TODAY.

FOR EACH almcmov WHERE codcia = s-codcia
    AND nroser = 000
    AND codalm = s-codalm
    AND almdes = s-almdes
    AND tipmov = s-tipmov
    AND codmov = s-codmov
    AND usuario = s-user-id
    AND fchdoc = s-fchdoc
    TRANSACTION ON ERROR UNDO, RETURN 'ADM-ERROR' ON STOP UNDO, RETURN 'ADM-ERROR':
    DISPLAY fchdoc nroser nrodoc usuario flgsit.
    PAUSE 0.
  
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

     ASSIGN 
         Almcmov.FlgEst = 'A'
         Almcmov.Observ = "      A   N   U   L   A   D   O       "
         Almcmov.usuario = S-USER-ID
         Almcmov.FchAnu = TODAY.
END.

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

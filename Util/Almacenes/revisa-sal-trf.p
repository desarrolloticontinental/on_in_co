DEF BUFFER CMOV FOR Almcmov.
DEF BUFFER DMOV FOR Almdmov.

DEF VAR r-Rowid AS ROWID.
DEF VAR pCodAlm AS CHAR INIT '999'.

FOR EACH almacen NO-LOCK WHERE codcia = 1 AND lookup(codalm, '11t,999') = 0:
    FOR EACH almcmov NO-LOCK WHERE codcia = 1
        AND codalm = almacen.codalm
        AND tipmov = 's'
        AND codmov = 03
        AND fchdoc >= 10/01/2011
        AND flgest = 'A':
        r-Rowid = ?.
        FOR EACH CMOV USE-INDEX Almc04 WHERE CMOV.codcia = Almcmov.codcia
            AND CMOV.codalm = pCodAlm
            AND CMOV.tipmov = "I"
            AND CMOV.codmov = Almcmov.codmov
            AND CMOV.nroser = 000
            AND CMOV.almdes = Almcmov.codalm
            AND CMOV.fchdoc >= almcmov.fchdoc
            BY CMOV.FchDoc DESC:
            IF INTEGER(SUBSTRING(CMOV.NroRf1,1,3)) = Almcmov.NroSer
                AND INTEGER(SUBSTRING(CMOV.NroRf1,4)) = Almcmov.NroDoc 
                THEN DO:
                r-Rowid = ROWID(CMOV).
                LEAVE.
            END.
        END.
        IF r-Rowid <> ? THEN DO:
            FIND CMOV WHERE ROWID(CMOV) = r-Rowid EXCLUSIVE-LOCK NO-ERROR.
            /*IF cmov.flgest <> 'a' THEN DISPLAY almcmov.codalm almcmov.nrodoc almcmov.fchdoc.*/
            IF CMOV.flgest <> 'a' THEN DO:
                ASSIGN
                    CMOV.FlgEst = 'A'
                    CMOV.Observ = "      A   N   U   L   A   D   O       "
                    CMOV.Usuario = almcmov.usuario.
                FOR EACH DMOV OF CMOV:
                    ASSIGN R-ROWID = ROWID(DMOV).
                    RUN ALM\ALMDCSTK (R-ROWID).   /* Descarga del Almacen POR INGRESOS */
                    IF RETURN-VALUE = 'ADM-ERROR' THEN UNDO, RETURN 'ADM-ERROR'.
                    /* RHC 03.04.04 REACTIVAMOS KARDEX POR ALMACEN */
                    RUN alm/almacpr1 (R-ROWID, 'D').
                    IF RETURN-VALUE = 'ADM-ERROR' THEN UNDO, RETURN 'ADM-ERROR'.
                    DELETE DMOV.
                END.
            END.
        END.
    END.
END.


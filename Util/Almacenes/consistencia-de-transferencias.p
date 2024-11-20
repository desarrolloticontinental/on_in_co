DEF BUFFER cmov FOR almcmov.
DEF BUFFER dmov FOR almdmov.
    
DEF BUFFER cmov9i FOR almcmov.
DEF BUFFER dmov9i FOR almdmov.
DEF BUFFER cmov9s FOR almcmov.
DEF BUFFER dmov9s FOR almdmov.

FOR EACH almacen NO-LOCK WHERE almacen.codcia = 001
    AND LOOKUP(almacen.codalm, '999,11T') = 0:
    FOR EACH almcmov NO-LOCK WHERE almcmov.codcia = 001
        AND almcmov.codalm = almacen.codalm
        AND almcmov.tipmov = 'I'
        AND almcmov.codmov = 03
        AND almcmov.fchdoc >= 01/01/2012
        AND almcmov.flgest <> 'A':
        /* buscamos el origen */
        FIND cmov WHERE cmov.codcia = 001
            AND cmov.codalm = almcmov.almdes
            AND cmov.tipmov = 'S'
            AND cmov.codmov = 03
            AND cmov.nroser = INTEGER(SUBSTRING(almcmov.nrorf1,1,3))
            AND cmov.nrodoc = INTEGER(SUBSTRING(almcmov.nrorf1,4))
            NO-LOCK.
        /* buscamos si hay movimientos de transferencia en el almacen 99 */
        FIND cmov9i WHERE cmov9i.codcia = 001
            AND cmov9i.codalm = '999'
            AND cmov9i.tipmov = 'I'
            AND cmov9i.codmov = 03
            AND cmov9i.almdes = cmov.codalm
            AND cmov9i.nrorf1 = STRING(cmov.nroser, '999') + STRING(cmov.nrodoc)
            NO-LOCK NO-ERROR.
        IF AVAILABLE cmov9i THEN DO:
            /* debería tener unsa salida */
            FIND cmov9s WHERE cmov9s.codcia = 001
                AND cmov9s.codalm = '999'
                AND cmov9s.tipmov = 'S'
                AND cmov9s.codmov = 03
                AND cmov9s.almdes = almcmov.codalm
                AND cmov9s.nrorf1 = STRING(almcmov.nroser, '999')+ STRING(almcmov.nrodoc)
                NO-LOCK NO-ERROR.
            /* ERROR -> No hay movimiento de salida */
            IF NOT AVAILABLE cmov9s THEN DO:
                DISPLAY cmov.tipmov cmov.nrodoc almcmov.tipmov almcmov.nrodoc.
            END.
        END.
    END.
END.


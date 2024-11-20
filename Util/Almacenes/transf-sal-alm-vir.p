DISABLE TRIGGERS FOR LOAD OF almcmov.
DISABLE TRIGGERS FOR LOAD OF almdmov.
DISABLE TRIGGERS FOR LOAD OF almmmate.
DISABLE TRIGGERS FOR LOAD OF almmmatg.

DEF VAR s-codcia AS INT INIT 001 NO-UNDO.
DEF VAR s-codmov AS INT INIT 03 NO-UNDO.        /* TRansferencia */

&SCOPED-DEFINE CONDICION (Almcmov.CodCia = S-CODCIA AND Almcmov.CodAlm = Almacen.codalm AND ~
Almcmov.TipMov = "S" AND Almcmov.CodMov = S-CODMOV AND Almcmov.FlgEst <> "A" AND ~
Almcmov.FlgSit = "T")

DEF BUFFER CMOV FOR almcmov.

    FOR EACH Almacen NO-LOCK WHERE Almacen.codcia = s-codcia AND Almacen.codalm <> '999':
        FOR EACH Almcmov NO-LOCK WHERE {&condicion}:
            FIND FIRST CMOV WHERE CMOV.codcia = almcmov.codcia
                AND CMOV.codalm = '999'
                AND CMOV.almdes = Almcmov.codalm
                AND CMOV.tipmov = 'I'
                AND CMOV.codmov = 03
                AND CMOV.nroser = 000
                AND CMOV.nrorf1 = STRING(Almcmov.NroSer,"999") + STRING(Almcmov.NroDoc,"999999")
                NO-LOCK NO-ERROR.
            /* RHC 24.03.2011 Generamos un movimiento de ingreso en el almacen 10 */
            IF NOT AVAILABLE CMOV THEN DO:
                DISPLAY almcmov.codalm almcmov.tipmov almcmov.codmov
                    almcmov.nroser almcmov.nrodoc.
                PAUSE 0.
                RUN alm/ing-trf-vir (ROWID(Almcmov), '999').
            END.
            /* ******************************************************************* */
        END.
    END.


DEF VAR s-codcia AS INT INIT 001 NO-UNDO.
DEF BUFFER B-MATE FOR Almmmate.

FOR EACH almmmatg NO-LOCK WHERE codcia = s-codcia:
    DISPLAY almmmatg.codmat.
    PAUSE 0.
    FOR EACH Almacen NO-LOCK WHERE Almacen.codcia = Almmmatg.codcia
        AND (Almmmatg.TpoMrg = "" OR Almacen.Campo-C[2] = Almmmatg.TpoMrg)  /* Tipo Almacén */
        AND Almacen.Campo-C[6] = "Si"       /* Comercial */
        AND Almacen.Campo-C[3] <> "Si"      /* No Remates */
        AND Almacen.Campo-C[9] <> "I"       /* No Inactivo */
        AND Almacen.AlmCsg = NO,            /* No de Consignación */
        EACH Almmmate NO-LOCK WHERE Almmmate.codcia = Almmmatg.codcia
        AND Almmmate.codalm = Almacen.codalm
        AND Almmmate.codmat = Almmmatg.codmat:
        FIND FIRST VtaAlmDiv WHERE VtaAlmDiv.CodCia = Almacen.codcia
            AND VtaAlmDiv.CodDiv = Almacen.coddiv
            NO-LOCK NO-ERROR.
        IF NOT AVAILABLE VtaAlmDiv THEN NEXT.
        IF VtaAlmDiv.codalm <> Almacen.codalm THEN NEXT.
        {lib/lock-generico.i &Tabla="B-MATE" ~
            &Condicion="ROWID(B-MATE) = ROWID(Almmmate)" ~
            &Bloqueo="EXCLUSIVE-LOCK" ~
            &Accion="RETRY" ~ 
            &Mensaje="YES" ~
            &TipoError=""ADM-ERROR"" }

        CASE Almacen.Campo-C[2]:
            WHEN "2" THEN DO:     /* Minorista */
                B-MATE.StkMax =  Almmmatg.CtoPrm[2].
            END.
            OTHERWISE DO:         /* El resto es Mayorista */
                B-MATE.StkMax = Almmmatg.CtoPrm[1].
            END.
        END CASE.
        RELEASE B-MATE.
    END.

END.

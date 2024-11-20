/* Se va a cambiar la MonVta del Almmmatg solo a la línea 010 */
DISABLE TRIGGERS FOR LOAD OF almmmatg.
DISABLE TRIGGERS FOR LOAD OF vtatabla.
DISABLE TRIGGERS FOR LOAD OF vtalistamay.

DEF VAR f-Factor AS DEC NO-UNDO.
DEF BUFFER BTABLA FOR VtaTabla.
DEF VAR F-PRECIO AS DEC NO-UNDO.

DEF VAR s-codcia AS INT INIT 001 NO-UNDO.

FOR EACH almmmatg WHERE almmmatg.codcia = s-codcia AND Almmmatg.CodFam = "010":
    IF Almmmatg.monvta = 2 THEN DO:
        ASSIGN
            Almmmatg.MonVta = 1.    /* <<< OJO <<< */
        /* Lista Mayorista General */
        ASSIGN
            Almmmatg.CtoLis = Almmmatg.CtoLis * Almmmatg.TpoCmb
            Almmmatg.CtoTot = Almmmatg.CtoTot * Almmmatg.TpoCmb
            Almmmatg.PreVta[1] = Almmmatg.PreVta[1] * Almmmatg.TpoCmb
            Almmmatg.PreVta[2] = Almmmatg.PreVta[2] * Almmmatg.TpoCmb
            Almmmatg.PreVta[3] = Almmmatg.PreVta[3] * Almmmatg.TpoCmb
            Almmmatg.PreVta[4] = Almmmatg.PreVta[4] * Almmmatg.TpoCmb
            Almmmatg.PreOfi = Almmmatg.PreOfi * Almmmatg.TpoCmb.
        RUN replica-almmmatg.
        /* Descuentos Promocionales */

        ASSIGN
            F-FACTOR = 1
            F-PRECIO = Almmmatg.Prevta[1].
        FOR EACH VtaTabla WHERE VtaTabla.CodCia = Almmmatg.CodCia
            AND VtaTabla.Llave_c1 = Almmmatg.codmat
            AND VtaTabla.Tabla = "DTOPROLIMA":
            f-Factor = 1.   /* x defecto */
            FIND BTABLA WHERE BTABLA.codcia = s-codcia
                AND BTABLA.llave_c1 = VtaTabla.Llave_c2
                AND BTABLA.tabla = "DIVFACXLIN"
                AND BTABLA.llave_c2 = Almmmatg.codfam
                NO-LOCK NO-ERROR.
            IF AVAILABLE BTABLA AND BTABLA.valor[1] > 0 THEN F-FACTOR = ( 1 + BTABLA.valor[1] / 100 ).
            ASSIGN
                VtaTabla.Valor[2] = ROUND( (F-PRECIO * F-FACTOR) * ( 1 - ( VtaTabla.Valor[1] / 100 ) ),4).
            RUN replica-vtatabla.
        END.
        /* Lista Mayorista por División */
        FOR EACH VtaListaMay WHERE VtaListaMay.codcia = Almmmatg.codcia
            AND VtaListaMay.codmat = Almmmatg.codmat:
            ASSIGN
                VtaListaMay.MonVta = 1
                VtaListaMay.PreOfi = VtaListaMay.PreOfi * Almmmatg.TpoCmb.
        END.
        /* Lista Minorista UTILEX */
        FOR EACH VtaListaMinGn WHERE VtaListaMinGn.CodCia = Almmmatg.CodCia
            AND VtaListaMinGn.codmat = Almmmatg.CodMat:
            ASSIGN
                VtaListaMinGn.MonVta = 1
                VtaListaMinGn.PreOfi = VtaListaMinGn.PreOfi * Almmmatg.TpoCmb.
            RUN replica-vtalistamingn.
        END.
        /* Lista TERCEROS */
        FOR EACH ListaTerceros WHERE ListaTerceros.CodCia = Almmmatg.CodCia
            AND ListaTerceros.CodMat = Almmmatg.CodMat:
            ASSIGN
                ListaTerceros.MonVta = 1
                ListaTerceros.PreOfi[1] = ListaTerceros.PreOfi[1] * Almmmatg.TpoCmb
                ListaTerceros.PreOfi[2] = ListaTerceros.PreOfi[2] * Almmmatg.TpoCmb
                ListaTerceros.PreOfi[3] = ListaTerceros.PreOfi[3] * Almmmatg.TpoCmb.
        END.
    END.
END.

PROCEDURE replica-almmmatg:

    {rpl/reptrig.i
&Table  = almmmatg
&Key    =  "string(almmmatg.codcia,'999') + string(almmmatg.codmat,'x(6)')"
&Prg    = r-almmmatg
&Event  = WRITE
&FlgDB0 = TRUE  /* Replicar de la sede remota a la base principal */
&FlgDB1 = NO    /* Plaza Lima Norte 00501 */
&FlgDB2 = NO    /* Surquillo 00023 */
&FlgDB3 = NO    /* Chorrillos 00027 */
&FlgDB4 = NO    /* San Borja 00502 */
&FlgDB5 = NO    /* La Molina 00503 */
&FlgDB6 = NO    /* Beneficiencia 00504 */
&FlgDB7 = NO    /* Feria Plaza Norte 00505 */
&FlgDB8 = NO    /* La Rambla 00507 */
&FlgDB9 = NO    /* San Isidro 00508 */
&FlgDB10 = NO   /* Chiclayo 00065 */
&FlgDB11 = NO   /* Trujillo 00067 */
&FlgDB12 = TRUE
&FlgDB13 = TRUE
&FlgDB14 = TRUE
&FlgDB15 = TRUE
&FlgDB16 = TRUE
&FlgDB17 = TRUE
&FlgDB18 = NO   /* Jesus Obrero 00005 */
&FlgDB19 = NO   /* CONTINENTAL PERU */
&FlgDB20 = NO   /* SERVIDOR CENTRAL */
}

END PROCEDURE.

PROCEDURE replica-vtatabla:

    {rpl/reptrig.i
    &Table  = vtatabla
    &Key    =  "string(vtatabla.codcia,'999') + string(vtatabla.tabla,'x(20)') + 
    string(vtatabla.llave_c1,'x(20)') + string(vtatabla.llave_c2,'x(20)') + string(vtatabla.llave_c3,'x(20)')"
    &Prg    = r-vtatabla
    &Event  = WRITE
    &FlgDB0 = TRUE  /* Replicar de la sede remota a la base principal */
    &FlgDB1 = NO    /* Plaza Lima Norte 00501 */
    &FlgDB2 = NO    /* Surquillo 00023 */
    &FlgDB3 = NO    /* Chorrillos 00027 */
    &FlgDB4 = NO    /* San Borja 00502 */
    &FlgDB5 = NO    /* La Molina 00503 */
    &FlgDB6 = NO    /* Beneficiencia 00504 */
    &FlgDB7 = NO    /* Plaza Norte 00505 */
    &FlgDB8 = NO    /* La Rambla 00507 */
    &FlgDB9 = NO    /* San Isidro 00508 */
    &FlgDB10 = NO   /* 00065 */
    &FlgDB11 = NO   /* Trujillo 00067 */
    &FlgDB12 = TRUE
    &FlgDB13 = TRUE
    &FlgDB14 = TRUE
    &FlgDB15 = TRUE
    &FlgDB16 = TRUE
    &FlgDB17 = TRUE
    &FlgDB18 = NO   /* Jesus Obrero 00005 */
    &FlgDB19 = NO   /* CONTINENTAL PERU */
    &FlgDB20 = NO   /* SERVIDOR CENTRAL */
    }

END PROCEDURE.

PROCEDURE replica-vtalistamingn:

    {rpl/reptrig.i
&Table  = vtalistamingn
&Key    =  "string(vtalistamingn.codcia,'999') + string(vtalistamingn.codmat,'x(6)')"
&Prg    = r-vtalistamingn
&Event  = WRITE
&FlgDB0 = TRUE  /* Replicar de la sede remota a la base principal */
&FlgDB1 = NO    /* Plaza Lima Norte 00501 */
&FlgDB2 = NO    /* Surquillo 00023 */
&FlgDB3 = NO    /* Chorrillos 00027 */
&FlgDB4 = NO    /* San Borja 00502 */
&FlgDB5 = NO    /* La Molina 00503 */
&FlgDB6 = NO    /* Beneficiencia 00504 */
&FlgDB7 = NO    /* Plaza Norte 00505 */
&FlgDB8 = NO    /* La Rambla 00507 */
&FlgDB9 = NO    /* San Isidro 00508 */
&FlgDB10 = NO   /* 00065 */
&FlgDB11 = NO   /* Trujillo 00067 */
&FlgDB12 = TRUE
&FlgDB13 = TRUE
&FlgDB14 = TRUE
&FlgDB15 = TRUE
&FlgDB16 = TRUE
&FlgDB17 = TRUE
&FlgDB18 = NO   /* Jesus Obrero 00005 */
&FlgDB19 = NO   /* CONTINENTAL PERU */
&FlgDB20 = NO   /* SERVIDOR CENTRAL */
}


END PROCEDURE.

DEFINE NEW SHARED TEMP-TABLE t-prev
    FIELD Tipo    AS CHAR
    FIELD Banco   LIKE cb-dmov.CodBco
    FIELD Periodo LIKE cb-dmov.periodo
    FIELD NroMes LIKE cb-dmov.nromes
    FIELD Codope LIKE cb-dmov.codope
    FIELD Codcta LIKE cb-dmov.codcta
    FIELD CodDiv LIKE cb-dmov.coddiv
    FIELD Codmon LIKE cb-dmov.codmon
    FIELD Fchdoc LIKE cb-dmov.fchdoc
    FIELD Fchvto LIKE cb-dmov.fchvto
    FIELD Coddoc LIKE cb-dmov.coddoc
    FIELD Nrodoc LIKE cb-dmov.nrodoc
    FIELD Codref LIKE cb-dmov.codref
    FIELD Nroref LIKE cb-dmov.nroref
    FIELD Glodoc LIKE cb-dmov.glodoc
    FIELD Tpocmb LIKE cb-dmov.tpocmb
    FIELD TpoMov LIKE cb-dmov.tpomov
    FIELD ImpMn1 LIKE cb-dmov.impmn1 
    FIELD ImpMn2 LIKE cb-dmov.impmn2
    FIELD clfaux LIKE cb-dmov.Clfaux
    FIELD codaux LIKE cb-dmov.Codaux
    INDEX IDX01 Tipo.

DEFINE BUFFER B-prev  FOR t-prev.

DEFINE VAR s-codcia  AS INTEGER INIT 001.
DEFINE VAR s-nomcia  AS CHAR.
DEFINE VAR s-user-id AS CHAR INIT 'ADMIN'.
DEFINE VAR s-coddiv  AS CHAR.
DEFINE VAR cb-codcia AS INT INIT 000.
DEFINE VAR f-division AS CHAR.
DEFINE VAR x-codope AS CHAR.
DEFINE VAR x-tpocmb AS DEC.
DEFINE VAR FILL-IN-fchast AS DATE.
DEFINE VAR x-tpomov  AS LOGICAL NO-UNDO.
DEFINE VAR x-codcbd  AS CHAR    NO-UNDO.
DEFINE VAR x-codcta  AS CHAR    NO-UNDO.
DEFINE VAR x-periodo AS INTEGER NO-UNDO.
DEFINE VAR x-nromes  AS INTEGER NO-UNDO.
DEFINE VAR x-ctagan  AS CHAR NO-UNDO.
DEFINE VAR x-ctaper  AS CHAR NO-UNDO.
DEFINE VAR x-rndgan  AS CHAR NO-UNDO.
DEFINE VAR x-rndper  AS CHAR NO-UNDO.
DEFINE VAR x-glodoc  AS CHAR    NO-UNDO.
DEFINE VAR FILL-IN-tpocmb AS DEC.
DEFINE VAR FILL-IN-TcCompra AS DEC.


/* ************************************************************************ */
/* *************************** VALORES INICIALES ************************** */
/* ************************************************************************ */
ASSIGN
    x-Periodo = 2014
    x-NroMes = 01.

FIND cb-cfgg WHERE cb-cfgg.CodCia = cb-codcia AND
   cb-cfgg.Codcfg = 'RND' NO-LOCK NO-ERROR.
IF AVAILABLE cb-Cfgg THEN
 ASSIGN
    x-rndgan = cb-cfgg.codcta[1] 
    x-rndper = cb-cfgg.codcta[2].
ELSE DO:
 MESSAGE 'Configuracion de Cuentas de Redondeo no existe' VIEW-AS ALERT-BOX ERROR.
 RETURN.
END.

FIND cb-cfgg WHERE cb-cfgg.CodCia = cb-codcia AND
   cb-cfgg.Codcfg = 'C01' NO-LOCK NO-ERROR.
IF AVAILABLE cb-Cfgg THEN
 ASSIGN
    x-ctagan = cb-cfgg.codcta[2] 
    x-ctaper = cb-cfgg.codcta[1].
ELSE DO:
 MESSAGE 'Configuracion de Cuentas de Diferencia de Cambio no existe' VIEW-AS ALERT-BOX ERROR.
 RETURN.
END.

FIND FacCfgGn WHERE FacCfgGn.codcia = s-codcia NO-LOCK NO-ERROR.

/* Verifico la configuracion de las cuentas de Ingreso a Caja */
FIND cb-cfgg WHERE cb-cfgg.CodCia = cb-codcia AND
   cb-cfgg.Codcfg = 'CJA' NO-LOCK NO-ERROR.
IF AVAILABLE cb-Cfgg THEN x-codope = cb-cfgg.Codope.
ELSE DO:
 MESSAGE 'Configuracion de Ctas de Ingreso a Caja no existe' VIEW-AS ALERT-BOX ERROR.
 RETURN.
END.
/* ************************************************************************ */

DEFINE VAR x-Linea AS CHAR.
INPUT FROM c:\tmp\boveda.prn.
REPEAT:
    IMPORT UNFORMATTED x-linea.
    IF x-linea = '' THEN LEAVE.
    ASSIGN
        f-division = SUBSTRING(x-linea,1,5)
        FILL-IN-fchast = DATE(SUBSTRING(x-linea,11)).
    FIND gn-tcmb WHERE gn-tcmb.fecha = FILL-IN-fchast NO-LOCK NO-ERROR.
    IF AVAILABLE gn-tcmb THEN DO:
        FILL-IN-tpocmb = gn-tcmb.venta.
        FILL-IN-TcCompra = gn-tcmb.compra.
    END.
    ELSE DO:
        MESSAGE "No se ha registrado el tipo de cambio" SKIP
            "para la fecha ingresada (" FILL-IN-fchast ")"
            VIEW-AS ALERT-BOX ERROR.
        NEXT.
    END.
    RUN proc_Carga-Temp.
END.

PROCEDURE proc_Carga-Temp:

    EMPTY TEMP-TABLE t-prev.

    FIND cb-cfgcja WHERE
        cb-cfgcja.Codcia = S-CODCIA AND
        cb-cfgcja.CodDiv = F-DIVISION
        NO-LOCK NO-ERROR.
    IF NOT AVAILABLE cb-cfgcja THEN DO:
        MESSAGE
            "Configuracion Cierre de Caja no existe." SKIP
            "Verifique y procese la generación de asientos"
           VIEW-AS ALERT-BOX ERROR.
        RETURN ERROR.
    END.

    x-codope = '001'.       /* <<< OJO <<< */

    /* EGRESOS A CAJA */
    RUN proc_Egresos.

    x-tpocmb = 0.
    RUN proc_Graba-Diferencia.


    RUN Transferir-asiento.


END PROCEDURE.

PROCEDURE proc_Graba-Diferencia:

    DEFINE VAR x-s AS DECIMAL NO-UNDO.
    DEFINE VAR x-d AS DECIMAL NO-UNDO.
    DEFINE BUFFER B-prev FOR t-prev.

    FOR EACH t-prev NO-LOCK BREAK BY t-prev.coddiv /*BY t-prev.tipo BY t-prev.nroref*/ BY t-prev.codcta:
        IF FIRST-OF(t-prev.coddiv) THEN DO:
            x-s = 0.
            x-d = 0.
        END.
        x-s = x-s + (t-prev.ImpMn1 * IF t-prev.tpomov THEN -1 ELSE 1).
        x-d = x-d + (t-prev.ImpMn2 * IF t-prev.tpomov THEN -1 ELSE 1).
        IF LAST-OF(t-prev.coddiv) THEN DO:
            IF x-s <> 0  THEN DO:
                x-tpomov = x-s > 0.
                x-codcta = IF x-s > 0 THEN x-ctagan ELSE x-ctaper.
                RUN Graba-Dif (1, ABS(x-s)).
            END.
        END.
    END.

END PROCEDURE.


PROCEDURE Graba-Dif:

    DEFINE INPUT PARAMETER x-codmon AS INTEGER.
    DEFINE INPUT PARAMETER x-import AS DECIMAL.

    CREATE B-prev.
    ASSIGN
        B-prev.tipo    = t-prev.tipo
        B-prev.periodo = x-periodo
        B-prev.nromes  = x-nromes
        B-prev.codope  = x-codope
        B-prev.coddiv  = t-prev.CodDiv 
        B-prev.codmon  = x-codmon
        B-prev.codcta  = x-codcta
        B-prev.fchdoc  = t-prev.fchdoc
        B-prev.tpomov  = x-tpomov
        B-prev.Tpocmb  = t-prev.TpoCmb
        B-prev.glodoc  = 'Diferencia de Cambio'
        B-prev.nroref  = '999999999'. /* Solo por presentacion */

    IF x-codmon = 1 THEN ASSIGN B-prev.impmn1 = x-import.
    ELSE ASSIGN B-prev.impmn2 = x-import.

END PROCEDURE.


PROCEDURE proc_Egresos:

    DEFINE VAR x-fchvto AS DATE NO-UNDO.
    DEFINE VAR x-imptot AS DECIMAL NO-UNDO.

    FOR EACH CcbDMvto WHERE CcbDMvto.CodCia = s-codcia 
        AND CcbDMvto.FlgEst = 'C' 
        AND CcbDMvto.TpoRef = 'BOV'
        AND CcbDMvto.FchEmi = FILL-IN-fchast 
        AND CcbDMvto.CodDiv = F-DIVISION 
        AND CcbDMvto.Coddoc = "E/C"
        NO-LOCK:
        x-codcbd = "36".
        x-codcta = ccbdmvto.codcta.
        x-tpocmb = 1.
        x-glodoc = "REMESA A BOVEDA No.".
        x-tpomov = FALSE.       /* DEBE */
        FIND Ccbccaja WHERE Ccbccaja.codcia = s-codcia
            AND Ccbccaja.coddoc = Ccbdmvto.coddoc
            AND Ccbccaja.nrodoc = CCbdmvto.nrodoc
            NO-LOCK NO-ERROR.
        IF AVAILABLE Ccbccaja THEN x-tpocmb = Ccbccaja.tpocmb.
        RUN proc_Graba-Caja(
            CcbDMvto.Coddiv,
            (IF Ccbdmvto.depnac[1] > 0 THEN 1 ELSE 2),
            CcbDMvto.DepNac[1] + CcbDMvto.DepUsa[1],
            '@CL',
            CcbDMvto.Codcli,
            CcbDMvto.NroDep,
            CcbDMvto.FchEmi,
            x-tpocmb).
        x-tpomov = TRUE.        /* HABER */
        x-codcta = IF Ccbdmvto.depnac[1] > 0 THEN cb-cfgcja.codcta_1[1] ELSE cb-cfgcja.codcta_2[1].
        RUN proc_Graba-Caja(
            CcbDMvto.Coddiv,
            (IF Ccbdmvto.depnac[1] > 0 THEN 1 ELSE 2),
            CcbDMvto.DepNac[1] + CcbDMvto.DepUsa[1],
            '@CL',
            CcbDMvto.Codcli,
            CcbDMvto.NroDoc,
            CcbDMvto.FchEmi,
            x-tpocmb).
    END.

END PROCEDURE.

PROCEDURE proc_Graba-Caja:

    DEFINE INPUT PARAMETER x-coddiv AS CHAR.
    DEFINE INPUT PARAMETER x-codmon AS INTEGER.
    DEFINE INPUT PARAMETER x-import AS DECIMAL.
    DEFINE INPUT PARAMETER x-clfaux AS CHAR.
    DEFINE INPUT PARAMETER x-codaux AS CHAR.
    DEFINE INPUT PARAMETER x-nrodoc AS CHAR.
    DEFINE INPUT PARAMETER x-fchvto AS DATE.
    DEFINE INPUT PARAMETER x-tpocmb AS DECIMAL.

    /* EN CASO DE LA CUENTA AL DEBE (104 o 108) SE ACUMULA */
    IF x-TpoMov = NO THEN DO:   /* DEBE */
        FIND t-prev WHERE t-prev.codcta = x-codcta
            AND t-prev.clfaux = x-clfaux
            AND t-prev.codaux = x-codaux
            AND t-prev.coddoc = x-codcbd
            AND t-prev.nrodoc = x-nrodoc
            NO-ERROR.
        IF NOT AVAILABLE t-prev THEN CREATE t-prev.
    END.
    ELSE CREATE t-prev.
    ASSIGN
        t-prev.tipo    = STRING(Ccbdmvto.FchEmi)
        t-prev.periodo = x-periodo
        t-prev.nromes  = x-nromes
        t-prev.codope  = x-codope
        t-prev.coddiv  = x-CodDiv 
        t-prev.codmon  = x-codmon
        t-prev.codcta  = x-codcta
        t-prev.fchdoc  = Ccbdmvto.FchEmi
        t-prev.fchvto  = x-fchvto
        t-prev.tpomov  = x-tpomov
        t-prev.clfaux  = x-clfaux
        t-prev.codaux  = x-codaux
        t-prev.coddoc  = x-Codcbd
        t-prev.nrodoc  = x-nrodoc
        t-prev.Tpocmb  = x-TpoCmb
        t-prev.glodoc  = x-glodoc.

    IF x-codmon = 1 THEN
        ASSIGN t-prev.impmn1  = t-prev.impmn1 + x-import.
    ELSE
        ASSIGN
            t-prev.impmn2  = t-prev.impmn2 + x-import
            t-prev.impmn1  = t-prev.impmn1 + ROUND((x-import * x-tpocmb),2).
    RELEASE t-prev.

END PROCEDURE.


PROCEDURE Transferir-asiento:
    
    DEFINE VAR p-codcia  AS INTE NO-UNDO.
    DEFINE VAR p-mes     AS INTE NO-UNDO.
    DEFINE VAR p-periodo AS INTE NO-UNDO.
    DEFINE VAR p-codope  AS CHAR NO-UNDO.
    DEFINE VAR p-nroast  AS CHAR NO-UNDO.
    DEFINE VAR x-nroast  AS INTE NO-UNDO.
    DEFINE VAR p-fchast  AS DATE NO-UNDO.
    DEFINE VAR d-uno     AS DECI NO-UNDO.
    DEFINE VAR d-dos     AS DECI NO-UNDO.
    DEFINE VAR h-uno     AS DECI NO-UNDO.
    DEFINE VAR h-dos     AS DECI NO-UNDO.
    DEFINE VAR x-clfaux  AS CHAR NO-UNDO.
    DEFINE VAR x-genaut  AS INTE NO-UNDO.
    DEFINE VAR I         AS INTE NO-UNDO.
    DEFINE VAR J         AS INTE NO-UNDO.
    DEFINE VAR x-coddoc  AS LOGI NO-UNDO.

    DEFINE BUFFER detalle FOR CB-DMOV.

    p-codcia  = s-codcia.
    p-periodo = x-periodo.
    p-mes     = x-nromes.

    FIND FIRST t-prev NO-ERROR.
    IF NOT AVAILABLE t-prev THEN DO:
       BELL.
       MESSAGE "No se ha generado " SKIP "ning£n preasiento" VIEW-AS ALERT-BOX ERROR.
       RETURN.
    END.

    FIND cb-cfga WHERE cb-cfga.codcia = cb-codcia NO-LOCK NO-ERROR.
    IF NOT AVAILABLE cb-cfga THEN DO:
       BELL.
       MESSAGE "Plan de cuentas no configurado" VIEW-AS ALERT-BOX ERROR.
       RETURN.
    END.
    FIND Gn-Divi WHERE Gn-Divi.Codcia = S-CODCIA AND
                       Gn-Divi.Coddiv = F-DIVISION NO-LOCK NO-ERROR.
    IF NOT AVAILABLE Gn-Divi THEN DO:
       BELL.
       MESSAGE "Division No existe" VIEW-AS ALERT-BOX ERROR.
       RETURN.
    END.

    FOR EACH t-prev BREAK BY t-prev.tipo :
        IF FIRST-OF(t-prev.tipo) THEN DO:
            p-codope = t-prev.codope.
            RUN cbd/cbdnast.p(cb-codcia,
                              p-codcia, 
                              p-periodo, 
                              p-Mes, 
                              p-codope, 
                              OUTPUT x-nroast). 
            p-nroast = STRING(x-nroast, '999999').
            d-uno  = 0.
            d-dos  = 0.
            h-uno  = 0.
            h-dos  = 0.
        END.
        FIND cb-ctas WHERE
             cb-ctas.codcia = cb-codcia AND
             cb-ctas.codcta = t-prev.codcta NO-LOCK NO-ERROR.
        IF NOT AVAILABLE cb-ctas THEN NEXT.
        x-clfaux = cb-ctas.clfaux.
        x-coddoc = cb-ctas.piddoc.
        IF t-prev.impmn1 > 0 OR t-prev.impmn2 > 0 THEN DO:
            J = J + 1.
            CREATE CB-DMOV.
            CB-DMOV.codcia  = p-codcia.
            CB-DMOV.PERIODO = p-periodo.
            CB-DMOV.NROMES  = p-mes.
            CB-DMOV.CODOPE  = p-codope.
            CB-DMOV.NROAST  = p-nroast.
            CB-DMOV.NROITM  = J.
            CB-DMOV.codcta  = t-prev.codcta.
            CB-DMOV.coddiv  = t-prev.coddiv.
            Cb-dmov.Coddoc  = IF x-coddoc THEN t-prev.coddoc ELSE ''.
            Cb-dmov.Nrodoc  = IF x-coddoc THEN t-prev.nrodoc ELSE ''.
            CB-DMOV.clfaux  = IF x-clfaux <> '' THEN x-clfaux ELSE ''.
            CB-DMOV.codaux  = IF x-clfaux <> '' THEN t-prev.codaux ELSE ''.
            CB-DMOV.GLODOC  = t-prev.glodoc.
            CB-DMOV.tpomov  = t-prev.tpomov.
            CB-DMOV.impmn1  = t-prev.impmn1.
            CB-DMOV.impmn2  = t-prev.impmn2.
            CB-DMOV.FCHDOC  = FILL-IN-fchast.
            CB-DMOV.FCHVTO  = IF t-prev.fchvto = ? THEN FILL-IN-fchast ELSE t-prev.fchvto.
            CB-DMOV.FLGACT  = TRUE.
            CB-DMOV.RELACION = 0.
            CB-DMOV.codmon  = t-prev.codmon.
            CB-DMOV.tpocmb  = t-prev.tpocmb.
            cb-dmov.C-FCaja = "12".
    /*MLR-1*/ cb-dmov.codref = t-prev.codref.
    /*MLR-1*/ cb-dmov.nroref = t-prev.nroref.
            RUN cbd/cb-acmd.p(RECID(CB-DMOV),YES,YES).
            IF CB-DMOV.tpomov THEN DO:
                h-uno = h-uno + CB-DMOV.impmn1.
                h-dos = h-dos + CB-DMOV.impmn2.
            END.
            ELSE DO:
                d-uno = d-uno + CB-DMOV.impmn1.
                d-dos = d-dos + CB-DMOV.impmn2.
            END.

            cb-dmov.C-FCaja = "12".

            x-GenAut = 0.
            /* Preparando para Autom ticas */
            /* Verificamos si la Cuenta genera automaticas de Clase 9 */
            DO i = 1 TO NUM-ENTRIES( cb-cfga.GenAut9 ):
                IF cb-dmov.CodCta BEGINS ENTRY( i, cb-cfga.GenAut9 ) THEN DO:
                    IF ENTRY( i, cb-cfga.GenAut9) <> "" THEN DO:
                        x-GenAut = 1.
                        LEAVE.
                    END.
                END.
            END.
            /* Verificamos si la Cuenta genera automaticas de Clase 6 */
            DO i = 1 TO NUM-ENTRIES( cb-cfga.GenAut6 ):
                IF cb-dmov.CodCta BEGINS ENTRY( i, cb-cfga.GenAut6 ) THEN DO:
                    IF ENTRY( i, cb-cfga.GenAut6) <> "" THEN DO:
                        x-GenAut = 2.
                        LEAVE.
                    END.
               END.
            END.
            /* Verificamos si la Cuenta genera automaticas de otro tipo */
            DO i = 1 TO NUM-ENTRIES( cb-cfga.GenAut ):
                IF cb-dmov.CodCta BEGINS ENTRY( i, cb-cfga.GenAut ) THEN DO:
                    IF ENTRY( i, cb-cfga.GenAut) <> "" THEN DO:
                        x-GenAut = 3.
                        LEAVE.
                    END.
               END.
            END.
            cb-dmov.CtaAut = "".
            cb-dmov.CtrCta = "".
            CASE x-GenAut:
                /* Genera Cuentas Clase 9 */
                WHEN 1 THEN DO:
                    cb-dmov.CtrCta = cb-ctas.Cc1Cta.
                    cb-dmov.CtaAut = cb-ctas.An1Cta.
                    IF cb-dmov.CtrCta = "" THEN cb-dmov.CtrCta = cb-cfga.Cc1Cta9.
                END.
                /* Genera Cuentas Clase 6 */
                WHEN 2 THEN DO:
                    cb-dmov.CtrCta = cb-ctas.Cc1Cta.
                    cb-dmov.CtaAut = cb-ctas.An1Cta.
                    IF cb-dmov.CtrCta = "" THEN
                        cb-dmov.CtrCta = cb-cfga.Cc1Cta6.
                END.
                WHEN 3 THEN DO:
                    cb-dmov.CtaAut = cb-ctas.An1Cta.
                    cb-dmov.CtrCta = cb-ctas.Cc1Cta.
                END.
            END CASE.
            /* Chequendo las cuentas a generar en forma autom tica */
            IF x-GenAut > 0 THEN DO:
                IF NOT CAN-FIND(FIRST cb-ctas WHERE
                    cb-ctas.CodCia = cb-codcia AND
                    cb-ctas.CodCta = cb-dmov.CtaAut) THEN DO:
                    BELL.
                    MESSAGE
                        "Cuentas Autom ticas a generar" SKIP
                        "Tienen mal registro, Cuenta" cb-dmov.CtaAut "no existe"
                        VIEW-AS ALERT-BOX ERROR.
                    cb-dmov.CtaAut = "".
                END.
                IF NOT CAN-FIND( cb-ctas WHERE
                    cb-ctas.CodCia = cb-codcia AND
                    cb-ctas.CodCta = cb-dmov.CtrCta ) THEN DO:
                    BELL.
                    MESSAGE
                        "Cuentas Autom ticas a generar" SKIP
                        "Tienen mal registro, Contra Cuenta" cb-dmov.CtrCta "no existe"
                        VIEW-AS ALERT-BOX ERROR.
                    cb-dmov.CtrCta = "".
                END.
            END. /*Fin del x-genaut > 0 */
            IF cb-dmov.CtaAut <> "" AND cb-dmov.CtrCta <> "" THEN DO:
                J = J + 1.
                CREATE detalle.
                detalle.CodCia   = cb-dmov.CodCia.
                detalle.Periodo  = cb-dmov.Periodo.
                detalle.NroMes   = cb-dmov.NroMes.
                detalle.CodOpe   = cb-dmov.CodOpe.
                detalle.NroAst   = cb-dmov.NroAst.
                detalle.TpoItm   = "A".
                detalle.Relacion = RECID(cb-dmov).
                detalle.CodMon   = cb-dmov.CodMon.
                detalle.TpoCmb   = cb-dmov.TpoCmb.
                detalle.NroItm   = cb-dmov.NroItm.
                detalle.Codcta   = cb-dmov.CtaAut.
                detalle.CodDiv   = cb-dmov.CodDiv.
                detalle.ClfAux   = cb-dmov.ClfAux.
                detalle.CodAux   = cb-dmov.CodCta.
                detalle.NroRuc   = cb-dmov.NroRuc.
                detalle.CodDoc   = cb-dmov.CodDoc.
                detalle.NroDoc   = cb-dmov.NroDoc.
                detalle.GloDoc   = cb-dmov.GloDoc.
                detalle.CodMon   = cb-dmov.CodMon.
                detalle.TpoCmb   = cb-dmov.TpoCmb.
                detalle.TpoMov   = cb-dmov.TpoMov.
    /*MLR-1*/   detalle.CodRef   = cb-dmov.CodRef.
                detalle.NroRef   = cb-dmov.NroRef.
                detalle.FchDoc   = cb-dmov.FchDoc.
                detalle.FchVto   = cb-dmov.FchVto.
                detalle.ImpMn1   = cb-dmov.ImpMn1.
                detalle.ImpMn2   = cb-dmov.ImpMn2.
                detalle.ImpMn3   = cb-dmov.ImpMn3.
                detalle.Tm       = cb-dmov.Tm.
                detalle.CCO      = cb-dmov.CCO.
                detalle.C-FCaja  = cb-dmov.C-FCaja.

                RUN cbd/cb-acmd.p(RECID(detalle), YES ,YES).
                IF detalle.tpomov THEN DO:
                    h-uno = h-uno + detalle.impmn1.
                    h-dos = h-dos + detalle.impmn2.
                END.
                ELSE DO:
                    d-uno = d-uno + CB-DMOV.impmn1.
                    d-dos = d-dos + CB-DMOV.impmn2.
                END.
                J = J + 1.
                CREATE detalle.
                detalle.CodCia   = cb-dmov.CodCia.
                detalle.Periodo  = cb-dmov.Periodo.
                detalle.NroMes   = cb-dmov.NroMes.
                detalle.CodOpe   = cb-dmov.CodOpe.
                detalle.NroAst   = cb-dmov.NroAst.
                detalle.TpoItm   = "A".
                detalle.Relacion = RECID(cb-dmov).
                detalle.CodMon   = cb-dmov.CodMon.
                detalle.TpoCmb   = cb-dmov.TpoCmb.
                detalle.NroItm   = cb-dmov.NroItm.
                detalle.Codcta   = cb-dmov.Ctrcta.
                detalle.CodDiv   = cb-dmov.CodDiv.
                detalle.ClfAux   = cb-dmov.ClfAux.
                detalle.CodAux   = cb-dmov.CodCta.
                detalle.NroRuc   = cb-dmov.NroRuc.
                detalle.CodDoc   = cb-dmov.CodDoc.
                detalle.NroDoc   = cb-dmov.NroDoc.
                detalle.GloDoc   = cb-dmov.GloDoc.
                detalle.CodMon   = cb-dmov.CodMon.
                detalle.TpoCmb   = cb-dmov.TpoCmb.
                detalle.TpoMov   = NOT cb-dmov.TpoMov.
                detalle.ImpMn1   = cb-dmov.ImpMn1.
                detalle.ImpMn2   = cb-dmov.ImpMn2.
                detalle.ImpMn3   = cb-dmov.ImpMn3.
    /*MLR-1*/   detalle.CodRef   = cb-dmov.CodRef.
                detalle.NroRef   = cb-dmov.NroRef.
                detalle.FchDoc   = cb-dmov.FchDoc.
                detalle.FchVto   = cb-dmov.FchVto.
                detalle.Tm       = cb-dmov.Tm.
                detalle.CCO      = cb-dmov.CCO.
                detalle.C-FCaja  = cb-dmov.C-FCaja.            
                RUN cbd/cb-acmd.p(RECID(detalle), YES ,YES).
                IF detalle.tpomov THEN DO:
                    h-uno = h-uno + detalle.impmn1.
                    h-dos = h-dos + detalle.impmn2.
                END.
                ELSE DO:
                    d-uno = d-uno + CB-DMOV.impmn1.
                    d-dos = d-dos + CB-DMOV.impmn2.
                END.
            END.
        END.
        IF LAST-OF(t-prev.tipo) THEN DO:
           FIND cb-cmov WHERE
               cb-cmov.codcia  = p-codcia AND
               cb-cmov.PERIODO = p-periodo AND
               cb-cmov.NROMES  = p-mes AND
               cb-cmov.CODOPE  = p-codope AND
               cb-cmov.NROAST  = p-nroast NO-ERROR.
           IF NOT AVAILABLE cb-cmov THEN DO:
               CREATE cb-cmov.
               cb-cmov.codcia  = p-codcia.
               cb-cmov.PERIODO = p-periodo.
               cb-cmov.NROMES  = p-mes.
               cb-cmov.CODOPE  = p-codope.
               cb-cmov.NROAST  = p-nroast. 
           END.
           cb-cmov.Coddiv = F-DIVISION.         /* t-prev.coddiv. */
           cb-cmov.Fchast = FILL-IN-fchast.
           cb-cmov.TOTITM = J.
           cb-cmov.CODMON = 1.
           cb-cmov.TPOCMB = x-Tpocmb.
           cb-cmov.DBEMN1 = d-uno.
           cb-cmov.DBEMN2 = d-dos.
           cb-cmov.HBEMN1 = h-uno.
           cb-cmov.HBEMN2 = h-dos.
           cb-cmov.NOTAST = 'CIERRE DE CAJA TIENDA ' + F-DIVISION + "-" + Gn-Divi.DesDiv.
           cb-cmov.GLOAST = 'CIERRE DE CAJA TIENDA ' + F-DIVISION + "-" + Gn-Divi.DesDiv.

        END.
    END.
/*     MESSAGE ' PROCESO CONCLUIDO  ' SKIP                                */
/*             'Periodo : ' + STRING(cb-control.periodo, '9999') SKIP     */
/*             'Mes     : ' + STRING(cb-control.nromes, '99') SKIP        */
/*             'Asiento : ' + cb-control.codope + '-' + cb-control.nroast */
/*             VIEW-AS ALERT-BOX INFORMATION.                             */

END PROCEDURE.

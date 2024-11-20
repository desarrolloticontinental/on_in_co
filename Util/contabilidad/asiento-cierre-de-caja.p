DEFINE NEW SHARED TEMP-TABLE t-prev LIKE cb-dmov
    FIELD Tipo   AS CHAR
    FIELD fchvta AS DATE
    FIELD fchast LIKE cb-cmov.fchast.

DEFINE BUFFER B-CDOCU FOR CcbCDocu.
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
DEFINE VARIABLE x-ctalet LIKE FacDocum.CodCta NO-UNDO.
DEFINE VAR x-tpomov  AS LOGICAL NO-UNDO.
DEFINE VAR x-codcbd  AS CHAR    NO-UNDO.
DEFINE VAR x-glodoc  AS CHAR    NO-UNDO.
DEFINE VAR x-resumen AS LOGICAL NO-UNDO.
x-resumen = FALSE.
DEFINE VAR x-codcta  AS CHAR    NO-UNDO.
DEFINE VAR x-periodo AS INTEGER NO-UNDO.
DEFINE VAR x-nromes  AS INTEGER NO-UNDO.
DEFINE VAR FILL-IN-tpocmb AS DEC.
DEFINE VAR FILL-IN-TcCompra AS DEC.
DEFINE VAR x-ctagan  AS CHAR NO-UNDO.
DEFINE VAR x-ctaper  AS CHAR NO-UNDO.
DEFINE VAR x-rndgan  AS CHAR NO-UNDO.
DEFINE VAR x-rndper  AS CHAR NO-UNDO.

/* ************************************************************************ */
/* *************************** VALORES INICIALES ************************** */
/* ************************************************************************ */
ASSIGN
    x-Periodo = 2014
    x-NroMes = 01.
FIND cb-cfgg WHERE cb-cfgg.CodCia = cb-codcia AND cb-cfgg.Codcfg = 'RND' NO-LOCK NO-ERROR.
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
INPUT FROM c:\tmp\cierres.prn.
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

    x-codope = cb-cfgcja.codope.

    /* INGRESOS A CAJA */
    RUN proc_Ingresos.

    /* EGRESOS A CAJA */
    RUN proc_Egresos.

    x-tpocmb = 0.
    RUN proc_Graba-Diferencia.

    RUN Transferir-asiento.


END PROCEDURE.

PROCEDURE Transferir-Asiento:

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

    FOR EACH t-prev,
        FIRST cb-ctas NO-LOCK WHERE cb-ctas.codcia = cb-codcia AND
        cb-ctas.codcta = t-prev.codcta 
        BREAK BY t-prev.tipo :
        IF FIRST-OF(t-prev.tipo) THEN DO:
           /* RHC 04.05.2012 LA OPERACION ES UNICA POR CADA DIVISION */
           p-CodOpe = x-CodOpe.
           /* ****************************************************** */
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
        ASSIGN
            x-clfaux = cb-ctas.clfaux
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
           MESSAGE '*** ASIENTO GENERADO ***' SKIP
                   'Periodo : ' + STRING(cb-cmov.periodo, '9999') SKIP
                   'Mes     : ' + STRING(cb-cmov.nromes, '99') SKIP 
                   'Asiento : ' + cb-cmov.codope + '-' + cb-cmov.nroast
                   VIEW-AS ALERT-BOX INFORMATION.
        END.
    END.


END PROCEDURE.


PROCEDURE proc_Egresos:

    DEFINE VAR x-entra  AS LOGICAL.
    DEFINE VAR x-fchvto AS DATE NO-UNDO.
    DEFINE VAR x-imptot AS DECIMAL NO-UNDO.

    FOR EACH CcbCCaja WHERE
        CcbCCaja.CodCia = s-codcia AND
        CcbCCaja.FlgCie = 'C' AND
        CcbCCaja.FchCie = FILL-IN-fchast AND
        CcbCCaja.CodDiv = F-DIVISION AND
        CcbCCaja.Coddoc = "E/C" AND
        CcbCCaja.FlgEst <> "A"
        NO-LOCK BREAK BY CcbCCaja.Usuario BY ccbCCaja.HorCie:

        x-entra = false.
        FOR EACH CcbDCaja OF CcbCCaja NO-LOCK:
            IF LOOKUP(CcbDCaja.CodRef, 'FAC,BOL,N/C,N/D') > 0 THEN DO:
                FIND FacDocum WHERE
                    FacDocum.CodCia = s-codcia AND
                    FacDocum.CodDoc = CcbDCaja.CodRef
                    NO-LOCK NO-ERROR.
                IF AVAILABLE FacDocum THEN DO:
                    x-tpomov = FacDocum.TpoDoc.
                    x-codcbd = FacDocum.CodCbd.
                    x-tpocmb = 0.
                END.
                x-entra = true.
                RUN Graba-Documento(
                    CcbCCaja.CodDiv,
                    CcbDCaja.CodMon,
                    CcbDCaja.ImpTot,
                    '@CL',
                    CcbDCaja.CodRef,
                    CcbDCaja.Nroref,
                    x-fchvto,
                    x-tpocmb).
            END.
        END.
        IF x-entra = YES THEN NEXT.
        CASE CcbCCaja.Tipo:
            WHEN "DEVONC" THEN DO:
                FIND FacDocum WHERE
                    FacDocum.CodCia = s-codcia AND
                    FacDocum.CodDoc = SUBSTRING(CcbCCaja.Voucher[1],1,3)
                    NO-LOCK NO-ERROR.
                IF AVAILABLE FacDocum THEN DO:
                    x-tpomov = FacDocum.TpoDoc.
                    x-codcbd = FacDocum.CodCbd.
                    x-tpocmb = 0.
                END.
                IF CcbCCaja.ImpNac[1] > 0 THEN DO:
                    x-imptot = CcbCCaja.ImpNac[1].
                    x-codcta = CcbCCaja.CodCta[1].
                    RUN Graba-N/C(
                        CcbCCaja.CodDiv, 
                        1, 
                        x-imptot, 
                        '@CL', 
                        "N/C" ,
                        SUBSTRING(CcbCCaja.Voucher[1],4), 
                        x-fchvto, 
                        x-tpocmb).
                END.
                IF CcbCCaja.ImpUsa[1] > 0 THEN DO:
                    x-imptot = CcbCCaja.ImpUsa[1].
                    x-codcta = CcbCCaja.CodCta[1].
                    RUN Graba-N/C(
                        CcbCCaja.CodDiv, 
                        2, 
                        x-imptot, 
                        '@CL', 
                        "N/C" ,
                        SUBSTRING(CcbCCaja.Voucher[1],4), 
                        x-fchvto, 
                        x-tpocmb).
                END.
            END.
            WHEN "DEVOBD" THEN DO:
                FIND FacDocum WHERE
                    FacDocum.CodCia = s-codcia AND
                    FacDocum.CodDoc = SUBSTRING(CcbCCaja.Voucher[1], 1, 2)
                    NO-LOCK NO-ERROR.
                IF AVAILABLE FacDocum THEN DO:
                    x-tpomov = FacDocum.TpoDoc.
                    x-codcbd = FacDocum.CodCbd.
                    x-tpocmb = 0.
                END.
                IF CcbCCaja.ImpNac[1] > 0 THEN DO:
                    x-imptot = CcbCCaja.ImpNac[1].
                    x-codcta = FacDocum.CodCta[1].
                    RUN Graba-BD(
                        CcbCCaja.CodDiv, 
                        1, 
                        x-imptot, 
                        '@CL', 
                        "BD" ,
                        SUBSTRING(CcbCCaja.Voucher[1], 4), 
                        x-fchvto, 
                        x-tpocmb).
                END.
                IF CcbCCaja.ImpUsa[1] > 0 THEN DO:
                    x-imptot = CcbCCaja.ImpUsa[1].
                    x-codcta = FacDocum.CodCta[2].
                    RUN Graba-BD(
                        CcbCCaja.CodDiv, 
                        2, 
                        x-imptot, 
                        '@CL', 
                        "BD" ,
                        SUBSTRING(CcbCCaja.Voucher[1], 4), 
                        x-fchvto, 
                        x-tpocmb).
                END.
            END.
            WHEN "ANTREC" THEN DO:
                IF CcbCCaja.ImpUsa[1] <> 0 THEN DO:
                    x-codcbd = "34".
                    x-codcta = cb-cfgcja.codcta2[7].
                    x-tpocmb = CcbCCaja.TpoCmb.
                    x-glodoc = "Devolución-A/R" + '-' + CcbCcaja.CodDoc + "-" + CcbCCaja.Nrodoc + ' ' + ' Cli.' + CcbCCaja.Codcli + ' ' + SUBSTRING(CcbCCaja.Glosa,1,14,"CHARACTER").
                    x-tpomov = FALSE.
                    RUN proc_Graba-Caja(
                        CcbCCaja.Coddiv,
                        2,
                        CcbCCaja.ImpUsa[1],
                        '@CL',
                        CcbCCaja.Codcli,
                        SUBSTRING(CcbCCaja.Voucher[1],4,15),
                        CcbCCaja.FchVto[3],
                        x-tpocmb).
                    x-tpomov = TRUE.
                    x-codcta = cb-cfgcja.codcta2[1].
                    RUN proc_Graba-Caja(
                        CcbCCaja.Coddiv,
                        2,
                        CcbCCaja.ImpUsa[1],
                        '@CL',
                        CcbCCaja.Codcli,
                        SUBSTRING(CcbCCaja.Voucher[1],4,15),
                        CcbCCaja.FchVto[3],
                        x-tpocmb).
                END.
                IF CcbCCaja.ImpNac[1] <> 0 THEN DO:
                    x-codcbd = "34".
                    x-codcta = cb-cfgcja.codcta1[7].
                    x-tpocmb = 0.
                    x-glodoc = "Devocución-A/R" + CcbCcaja.CodDoc + '-' + CcbCCaja.Nrodoc + ' ' + ' Cli.' + CcbCCaja.Codcli + ' ' + SUBSTRING(CcbCCaja.Glosa,1,14,"CHARACTER").
                    x-tpomov = FALSE.
                    RUN proc_Graba-Caja(
                        CcbCCaja.Coddiv,
                        1,
                        CcbCCaja.ImpNac[1],
                        '@CL',
                        CcbCCaja.Codcli,
                        SUBSTRING(CcbCCaja.Voucher[1],4,15),
                        CcbCCaja.FchVto[3],
                        x-tpocmb).
                    x-codcta = cb-cfgcja.codcta1[1].
                    x-tpomov = TRUE.
                    RUN proc_Graba-Caja(
                        CcbCCaja.Coddiv,
                        1,
                        CcbCCaja.ImpNac[1],
                        '@CL',
                        CcbCCaja.Codcli,
                        SUBSTRING(CcbCCaja.Voucher[1],4,15),
                        CcbCCaja.FchVto[3],
                        x-tpocmb).
                END.
            END.
            OTHERWISE DO:
                x-tpomov = FALSE.
                IF CcbCCaja.ImpUsa[1] <> 0 THEN DO:
                    CASE CcbCCaja.Tipo:
                        WHEN "REMEBOV" THEN x-codcta = cb-cfgcja.CodCta_2[1].
                        WHEN "REMECJC" THEN x-codcta = cb-cfgcja.CodCta_2[2].
                        OTHERWISE x-codcta = cb-cfgcja.CodCta2[1].
                    END CASE.
                    x-codcbd = SUBSTRING(CcbCCaja.Voucher[1],1,3).
                    x-tpocmb = CcbCCaja.Tpocmb.
                    x-glodoc = CcbCcaja.CodDoc + '-' + CcbCCaja.Nrodoc + ' ' + ' Cli.' + CcbCCaja.Codcli + ' ' + SUBSTRING(CcbCCaja.Glosa,1,14,"CHARACTER").         
                    RUN proc_Graba-Caja(
                        CcbCCaja.Coddiv,
                        2,
                        CcbCCaja.ImpUsa[1],
                        '@CL',
                        CcbCCaja.Codcli,
                        SUBSTRING(CcbCCaja.Voucher[1],4,15),
                        CcbCCaja.FchVto[3],
                        x-tpocmb).
                END.
                IF CcbCCaja.ImpNac[1] <> 0 THEN DO:
                    CASE CcbCCaja.Tipo:
                        WHEN "REMEBOV" THEN x-codcta = cb-cfgcja.CodCta_1[1].
                        WHEN "REMECJC" THEN x-codcta = cb-cfgcja.CodCta_1[2].
                        OTHERWISE x-codcta = cb-cfgcja.CodCta1[1].
                    END CASE.
                    x-codcbd = SUBSTRING(CcbCCaja.Voucher[1],1,3).
                    x-tpocmb = 0.
                    x-glodoc = CcbCcaja.CodDoc + '-' + CcbCCaja.Nrodoc + ' ' + ' Cli.' + CcbCCaja.Codcli + ' ' + SUBSTRING(CcbCCaja.Glosa,1,14,"CHARACTER").         
                    RUN proc_Graba-Caja(
                        CcbCCaja.Coddiv,
                        1,
                        CcbCCaja.ImpNac[1],
                        '@CL',
                        CcbCCaja.Codcli,
                        SUBSTRING(CcbCCaja.Voucher[1],4,15),
                        CcbCCaja.FchVto[3],
                        x-tpocmb).
                END.
            END.
        END CASE.
    END.

END PROCEDURE.


PROCEDURE proc_Ingresos:

DEFINE VAR x-fchvto AS DATE NO-UNDO.
DEFINE VAR x-imptot AS DECIMAL NO-UNDO.
DEFINE VAR x-codaux AS CHAR.

FOR EACH CcbCCaja NO-LOCK WHERE
    CcbCCaja.CodCia = s-codcia AND
    CcbCCaja.FlgCie = 'C' AND
    CcbCCaja.FchCie = FILL-IN-fchast AND
    CcbCCaja.CodDiv = F-DIVISION AND
    CcbCCaja.Coddoc = "I/C" AND
    CcbCCaja.FlgEst <> "A"
    BREAK BY CcbCCaja.Usuario BY CcbCCaja.HorCie:
    FOR EACH CcbDCaja OF CcbCCaja NO-LOCK:
        /* Cancelo provision del documento */
        x-glodoc = 'Canc.' + CcbDCaja.CodRef + '-' + CcbDCaja.NroRef + ' Cli.' + CcbDCaja.Codcli.
        FIND FacDocum WHERE
            FacDocum.CodCia = s-codcia AND
            FacDocum.CodDoc = CcbDCaja.CodRef
            NO-LOCK NO-ERROR.
        IF AVAILABLE FacDocum THEN
            ASSIGN
                x-ctalet[1] = FacDocum.CodCta[1]
                x-ctalet[2] = FacDocum.CodCta[2]
                x-tpomov = FacDocum.TpoDoc
                x-codcbd = FacDocum.CodCbd
                x-tpocmb = 0.
        IF LOOKUP(CcbDCaja.CodRef,'TCK,FAC,BOL,N/C,N/D,LET') > 0 THEN DO:
            RUN proc_Graba-Documento(CcbCCaja.CodDiv, CcbDCaja.CodMon, CcbDCaja.ImpTot, '@CL', CcbDCaja.CodRef, CcbDCaja.Nroref, x-fchvto, x-tpocmb).
        END.
    END.        
    IF CcbCCaja.ImpUsa[1] <> 0 THEN DO:
        x-tpocmb = CcbCCaja.TpoCmb.
        IF CcbCCaja.Tipo = "ANTREC" THEN DO:
            x-codcbd = '34'.
            x-codcta = cb-cfgcja.codcta2[7].
            x-glodoc = 'Anticipo-Recibido' + CcbCcaja.CodDoc + '-' + CcbCCaja.Nrodoc + ' Cli.' + CcbCCaja.Codcli.
            x-tpomov = TRUE.
            RUN proc_Graba-Caja(CcbCCaja.Coddiv, 2, CcbCCaja.ImpUsa[1], '@CL', CcbCCaja.Codcli, SUBSTRING(CcbCCaja.Voucher[1],4,15), x-fchvto, x-tpocmb).
            x-codcta = cb-cfgcja.codcta2[1].
            x-tpomov = FALSE.
            RUN proc_Graba-Caja(CcbCCaja.Coddiv, 2, CcbCCaja.ImpUsa[1], '@CL', CcbCCaja.Codcli, SUBSTRING(CcbCCaja.Voucher[1],4,15), x-fchvto, x-tpocmb).
        END.
        ELSE IF CcbCCaja.Tipo = "DEVONC" THEN DO:
            x-codcbd = SUBSTRING(CcbCcaja.Voucher[1],1,3).
            x-codcta = CCbCCaja.CodCta[1].
            x-glodoc = CcbCcaja.CodDoc + '-' + CcbCCaja.Nrodoc + ' ' + SUBSTRING(CcbCCaja.Glosa,1,14,"CHARACTER").
            x-tpomov = TRUE.
            RUN proc_Graba-Caja(CcbCCaja.Coddiv, 2, CcbCCaja.ImpUsa[1], '', "", SUBSTRING(CcbCcaja.Voucher[1],4,15), CcbCCaja.FchVto[3], x-tpocmb).
            x-tpomov = FALSE.
            RUN proc_Graba-Caja(CcbCCaja.Coddiv, 2, CcbCCaja.ImpUsa[1], '', "", SUBSTRING(CcbCcaja.Voucher[1],4,15), CcbCCaja.FchVto[3], x-tpocmb).
        END.
    END.                
    IF CcbCCaja.ImpNac[1] <> 0 THEN DO:
        x-tpocmb = 0.
        IF CcbCCaja.Tipo = "ANTREC" THEN DO:
            x-codcbd = '34'.
            x-codcta = cb-cfgcja.codcta1[7].
            x-glodoc = 'Anticipo-Recibido' + CcbCcaja.CodDoc + '-' + CcbCCaja.Nrodoc + ' Cli.' + CcbCCaja.Codcli.
            x-tpomov = TRUE.
            RUN proc_Graba-Caja(CcbCCaja.Coddiv, 1, CcbCCaja.ImpNac[1], '@CL', CcbCCaja.Codcli, SUBSTRING(CcbCCaja.Voucher[1],4,15), x-fchvto, x-tpocmb).
            x-codcta = cb-cfgcja.codcta1[1].
            x-tpomov = FALSE.
            RUN proc_Graba-Caja(CcbCCaja.Coddiv, 1, CcbCCaja.ImpNac[1], '@CL', CcbCCaja.Codcli, SUBSTRING(CcbCCaja.Voucher[1],4,15), x-fchvto, x-tpocmb).
        END.
        ELSE IF CcbCCaja.Tipo = "DEVONC" THEN DO:
            x-codcbd = SUBSTRING(CcbCcaja.Voucher[1],1,3).
            x-codcta = CCbCCaja.CodCta[1].
            x-glodoc = CcbCcaja.CodDoc + '-' + CcbCCaja.Nrodoc + ' ' + SUBSTRING(CcbCCaja.Glosa,1,14,"CHARACTER").
            x-tpomov = TRUE.
            RUN proc_Graba-Caja(CcbCCaja.Coddiv, 1, CcbCCaja.ImpNac[1], '', "", SUBSTRING(CcbCcaja.Voucher[1],4,15), CcbCCaja.FchVto[3], x-tpocmb).
            x-tpomov = FALSE.
            RUN proc_Graba-Caja(CcbCCaja.Coddiv, 1, CcbCCaja.ImpNac[1], '', "", SUBSTRING(CcbCcaja.Voucher[1],4,15), CcbCCaja.FchVto[3], x-tpocmb).
        END.
    END.

    /* ASIGNAMOS LA MISMA DIVISION QUE LA PROVISION */
    DEF VAR x-CodDiv AS CHAR NO-UNDO.       
    x-CodDiv = CcbCCaja.CodDiv.
    FIND FIRST CcbDCaja OF CcbCCaja NO-LOCK NO-ERROR.
    IF AVAILABLE CcbDCaja THEN DO:
        FIND CcbCDocu WHERE
            CcbCDocu.CodCia = s-codcia AND
            CcbCDocu.CodDoc = CcbDCaja.CodRef AND
            CcbCDocu.NroDoc = CcbDcaja.nroref
            NO-LOCK NO-ERROR.
        IF AVAILABLE CcbCDocu THEN x-CodDiv = ccbcdocu.coddiv.
    END.
    x-tpomov = FALSE.
    x-tpocmb = 0.
    IF CcbCCaja.ImpNac[2] > 0 THEN DO:
        x-codcbd = '32'.
        x-codcta = cb-cfgcja.codcta1[2].
        x-tpocmb = 0.
        x-glodoc = 'Canc-Cheque' + CcbCcaja.CodDoc + '-' + CcbCCaja.Nrodoc + ' Cli.' + CcbCCaja.Codcli.
        RUN proc_Graba-Caja(x-Coddiv, 1, CcbCCaja.ImpNac[2], '', "", CcbCCaja.Voucher[2], x-fchvto, x-tpocmb).
    END.
    IF CcbCCaja.ImpNac[3] > 0 THEN DO:
        x-codcbd = '32'.
        x-codcta = cb-cfgcja.codcta1[3].       
        x-tpocmb = 0.
        x-glodoc = 'Canc-Cheque' + CcbCcaja.CodDoc + '-' + CcbCCaja.Nrodoc + ' Cli.' + CcbCCaja.Codcli.       
        RUN proc_Graba-Caja(x-Coddiv, 1, CcbCCaja.ImpNac[3], '', "", CcbCCaja.Voucher[3], CcbCCaja.FchVto[3], x-tpocmb).
    END.
    IF CcbCCaja.ImpNac[4] > 0 THEN DO:
        x-codcta = cb-cfgcja.codcta1[4].
        x-codaux = ''.
        x-codcbd = ''.
        x-tpocmb = 0.
        x-glodoc = 'Canc-Tarjeta-Credito' + CcbCcaja.CodDoc + '-' + CcbCCaja.Nrodoc + ' Cli.' + CcbCCaja.Codcli.
        /* BUSCAMOS DATOS EL LA CUENTA */
        FIND FIRST CB-CTAS WHERE
            CB-CTAS.codcia = cb-codcia AND
            CB-CTAS.codcta = x-codcta NO-LOCK NO-ERROR.
        IF AVAILABLE CB-CTAS THEN x-codcbd = cb-ctas.Coddoc.
        FIND FacTabla WHERE
            FacTabla.CodCia = s-CodCia AND
            FacTabla.Tabla = "TC" AND
            FacTabla.Codigo = SUBSTRING(CcbCCaja.Voucher[9],1,2)
            NO-LOCK NO-ERROR.
        IF AVAILABLE FacTabla THEN x-codaux = STRING(FacTabla.Valor[1],"99999999").
        RUN proc_Graba-Caja(x-Coddiv, 1, CcbCCaja.ImpNac[4], '', x-codaux, CcbCCaja.Voucher[4], x-fchvto, x-tpocmb).
    END.
    IF CcbCCaja.ImpNac[5] > 0 THEN DO:
        x-codcta = "".
        x-codcbd = '36'.
        x-tpocmb = 0.
        x-glodoc = 'Canc-Deposito' + CcbCcaja.CodDoc + '-' + CcbCCaja.Nrodoc + ' Cli.' + CcbCCaja.Codcli.       
        FIND ccbcdocu WHERE
            ccbcdocu.CodCia = S-CodCia AND
            ccbcdocu.CodDoc = "BD" AND
            ccbcdocu.CodCli = CcbCCaja.Codcli AND
            ccbcdocu.nrodoc = CcbCCaja.Voucher[5]
            NO-LOCK NO-ERROR.
        IF NOT AVAILABLE ccbcdocu THEN DO:
            MESSAGE 'Depósito no se encuentra registrado ' + CcbCCaja.Codcli + ' ' + CcbCCaja.Voucher[5]
                VIEW-AS ALERT-BOX WARNING.
        END.
        IF AVAILABLE ccbcdocu THEN DO:
            x-codcbd = '34'.
            x-codcta = '12211100'.
            RUN proc_Graba-Caja(ccbcdocu.coddiv, 1, CcbCCaja.ImpNac[5],'@CLI', ccbcdocu.CodCli, CcbCCaja.Voucher[5], x-fchvto, x-tpocmb).
        END.
    END.
    /* Nota de Crédito */
    IF CcbCCaja.ImpNac[6] > 0 THEN DO:
        x-codcta = cb-cfgcja.codcta1[6].
        x-codcbd = '07'.
        x-tpocmb = 0.
        x-glodoc = 'Canc-N/C' + CcbCcaja.CodDoc + '-' + CcbCCaja.Nrodoc + ' Cli.' + CcbCCaja.Codcli.
        /* Aplicación Notas de Créditos */
        FOR EACH CCBDMOV WHERE
            CCBDMOV.CodCia = CcbCCaja.CodCia AND
            CCBDMOV.CodDiv = CcbCCaja.CodDiv AND
            CCBDMOV.CodRef = CcbCCaja.coddoc AND
            CCBDMOV.NroRef = CcbCCaja.nrodoc NO-LOCK:
            IF CCBDMOV.CodDoc <> "N/C" OR CCBDMOV.CodMon <> 1 THEN NEXT.   /* N/C en Soles */
            FIND FIRST CcbCDocu WHERE
                CcbCDocu.CodCia = CCBDMOV.CodCia AND
                CcbCDocu.CodCli = CCBDMOV.CodCli AND
                CcbCDocu.CodDoc = CCBDMOV.CodDoc AND
                CcbCDocu.NroDoc = CCBDMOV.NroDoc
                NO-LOCK NO-ERROR.
            IF NOT AVAILABLE CcbCDocu THEN DO:
                MESSAGE
                    'Nota de Crédito no existe ' + CcbCCaja.Codcli + ' ' + CCBDMOV.NroDoc SKIP
                    Ccbccaja.coddoc Ccbccaja.nrodoc
                    VIEW-AS ALERT-BOX ERROR.
                NEXT.
            END.
            IF ccbcdocu.CodCli BEGINS "11111111" THEN x-codaux = "11111111111".
            ELSE x-codaux = ccbcdocu.CodCli.
            RUN proc_Graba-Caja(ccbcdocu.coddiv, 1, CCBDMOV.ImpTot, '@CLI', x-codaux, CcbCDocu.NroDoc, x-fchvto, x-tpocmb).
        END.
    END.
    /* Anticipos */
    IF CcbCCaja.ImpNac[7] > 0 THEN DO:
        x-codcta = cb-cfgcja.codcta1[7].
        x-codcbd = '34'.
        x-tpocmb = 0.
        x-glodoc = 'Canc-Anticip' + CcbCcaja.CodDoc + '-' + CcbCCaja.Nrodoc + ' Cli.' + CcbCCaja.Codcli.
        FIND FIRST CcbCDocu WHERE CcbCDocu.CodCia = s-codcia 
            AND CcbCDocu.CodCli = CcbCCaja.Codcli 
            AND CcbCDocu.CodDoc = CcbCCaja.CodBco[7]       /*'A/R' AND*/
            AND CcbCDocu.NroDoc = CcbCCaja.Voucher[7]
            NO-LOCK NO-ERROR.
        IF NOT AVAILABLE CcbCDocu THEN DO:
            MESSAGE 'Anticipo no existe ' CcbCCaja.Codcli  CcbCCaja.Voucher[7] SKIP
                ccbccaja.coddiv ccbccaja.coddoc ccbccaja.nrodoc
                VIEW-AS ALERT-BOX ERROR.
        END.
        ELSE DO:
            x-codcbd = '34'.
            RUN proc_Graba-Caja(ccbcdocu.coddiv, 1, CcbCCaja.ImpNac[7], '@CLI', ccbcdocu.CodCli, CcbCCaja.Voucher[7], x-fchvto, x-tpocmb).
        END.
    END.
    /* Comisiones */
    IF CcbCCaja.ImpNac[8] > 0 THEN DO:
        x-codcbd = '32'.
        x-tpocmb = 0.
        x-codcta = cb-cfgcja.codcta1[8].
        x-glodoc = 'Comision Factoring' + CcbCcaja.CodDoc + '-' + CcbCCaja.Nrodoc + ' Cli.' + CcbCCaja.Codcli.
        RUN proc_Graba-Caja(CcbCCaja.CodDiv, 1, CcbCCaja.ImpNac[8], '', "", CcbCCaja.Voucher[8], x-fchvto, CcbCCaja.TpoCmb).
    END.
    /* Retenciones */
    IF CcbCCaja.ImpNac[9] > 0 THEN DO:
        x-codcbd = '20'.
        x-tpocmb = 0.
        x-codcta = cb-cfgcja.codcta1[9].
        x-glodoc = 'Retenciones' + CcbCcaja.CodDoc + '-' + CcbCCaja.Nrodoc + ' Cli.' + CcbCCaja.Codcli.
        RUN proc_Graba-Caja(CcbCCaja.CodDiv, 1, CcbCCaja.ImpNac[9], '', "", CcbCCaja.Voucher[9], x-fchvto, CcbCCaja.TpoCmb).
    END.
    /* Vales Consumo */
    IF CcbCCaja.ImpNac[10] > 0 THEN DO:
        x-codcbd = '32'.
        x-tpocmb = 0.
        x-codcta = cb-cfgcja.codcta1[10].
        x-glodoc = 'Vales Consumo' + CcbCcaja.CodDoc + '-' + CcbCCaja.Nrodoc + ' Cli.' + CcbCCaja.Codcli.
        RUN proc_Graba-Caja(CcbCCaja.CodDiv, 1, CcbCCaja.ImpNac[10], '', "", '', x-fchvto, CcbCCaja.TpoCmb).
    END.

    x-tpocmb = 0.
    FIND Gn-tcmb WHERE Gn-tcmb.Fecha = CcbCCaja.FchDoc NO-LOCK NO-ERROR.
    IF AVAILABL Gn-tcmb THEN x-tpocmb = Gn-tcmb.compra.

    IF CcbCCaja.ImpUsa[2] > 0 THEN DO:
        x-codcbd = '32'.
        x-codcta = cb-cfgcja.codcta2[2].
        x-glodoc = 'Canc-Cheque' + CcbCcaja.CodDoc + '-' + CcbCCaja.Nrodoc + ' Cli.' + CcbCCaja.Codcli.
        RUN proc_Graba-Caja(x-Coddiv, 2, CcbCCaja.ImpUsa[2], '', "", CcbCCaja.Voucher[2], x-fchvto, CcbCCaja.TpoCmb).
    END.
    IF CcbCCaja.ImpUsa[3] > 0 THEN DO:
        x-codcbd = '32'.
        x-codcta = cb-cfgcja.codcta2[3].       
        x-glodoc = 'Canc-Cheque' + CcbCcaja.CodDoc + '-' + CcbCCaja.Nrodoc + ' Cli.' + CcbCCaja.Codcli.
        RUN proc_Graba-Caja(x-Coddiv, 2, CcbCCaja.ImpUsa[3], '', "", CcbCCaja.Voucher[3], CcbCCaja.FchVto[3], CcbCCaja.TpoCmb).
    END.
    IF CcbCCaja.ImpUsa[4] > 0 THEN DO:
        x-codcta = cb-cfgcja.codcta2[4].
        x-codaux = ''.
        x-codcbd = ''.
        x-glodoc = 'Canc-Tarjeta-Credito' + CcbCcaja.CodDoc + '-' + CcbCCaja.Nrodoc + ' Cli.' + CcbCCaja.Codcli.
        /* BUSCAMOS DATOS EL LA CUENTA */
        FIND FIRST CB-CTAS WHERE
            CB-CTAS.codcia = cb-codcia AND
            CB-CTAS.codcta = x-codcta NO-LOCK NO-ERROR.
        IF AVAILABLE CB-CTAS THEN x-codcbd = cb-ctas.Coddoc.
        FIND FacTabla WHERE
            FacTabla.CodCia = s-CodCia AND
            FacTabla.Tabla = "TC" AND
            FacTabla.Codigo = SUBSTRING(CcbCCaja.Voucher[9],1,2)
            NO-LOCK NO-ERROR.
        IF AVAILABLE FacTabla THEN x-codaux = STRING(FacTabla.Valor[1],"99999999").
        RUN proc_Graba-Caja(x-Coddiv, 2, CcbCCaja.ImpUsa[4], '', x-codaux, CcbCCaja.Voucher[4], x-fchvto, x-tpocmb).
    END.
    IF CcbCCaja.ImpUsa[5] > 0 THEN DO:
        x-codcbd = '34'.
        x-glodoc = 'Canc-Deposito' + CcbCcaja.CodDoc + '-' + CcbCCaja.Nrodoc + ' Cli.' + CcbCCaja.Codcli.
        FIND ccbcdocu WHERE
            ccbcdocu.CodCia = S-CodCia AND
            ccbcdocu.CodDoc = "BD" AND
            ccbcdocu.CodCli = CcbCCaja.Codcli AND
            ccbcdocu.nrodoc = CcbCCaja.Voucher[5]
            NO-LOCK NO-ERROR.
        IF NOT AVAILABLE ccbcdocu THEN DO:
            MESSAGE
                'Depósito no se encuentra registrado ' + CcbCCaja.Codcli + ' ' + CcbCCaja.Voucher[5] 
                VIEW-AS ALERT-BOX WARNING.
        END.
        ELSE DO:
            x-codcbd = '34'.
            x-codcta = '12211110'.
            RUN proc_Graba-Caja(ccbcdocu.coddiv, 2, CcbCCaja.ImpUsa[5], '@CLI', ccbcdocu.CodCli, CcbCCaja.Voucher[5], x-fchvto, CcbCCaja.TpoCmb).
        END.
    END.
    /* Nota de Crédito */
    IF CcbCCaja.ImpUsa[6] > 0 THEN DO:
        x-codcta = cb-cfgcja.codcta2[6].
        x-codcbd = '07'.
        x-glodoc = 'Canc-N/C' + CcbCcaja.CodDoc + '-' + CcbCCaja.Nrodoc + ' Cli.' + CcbCCaja.Codcli.
        /* Aplicación Notas de Créditos */
        FOR EACH CCBDMOV WHERE
            CCBDMOV.CodCia = CcbCCaja.CodCia AND
            CCBDMOV.CodDiv = CcbCCaja.CodDiv AND
            CCBDMOV.CodRef = CcbCCaja.coddoc AND
            CCBDMOV.NroRef = CcbCCaja.nrodoc NO-LOCK:
            IF CCBDMOV.CodDoc <> "N/C" OR CCBDMOV.CodMon <> 2 THEN NEXT.   /* N/C en Dolares */
            FIND FIRST CcbCDocu WHERE
                CcbCDocu.CodCia = CCBDMOV.CodCia AND
                CcbCDocu.CodCli = CCBDMOV.CodCli AND
                CcbCDocu.CodDoc = CCBDMOV.CodDoc AND
                CcbCDocu.NroDoc = CCBDMOV.NroDoc
                NO-LOCK NO-ERROR.
            IF NOT AVAILABLE CcbCDocu THEN DO:
                MESSAGE
                    'Nota de Crédito no existe ' + CcbCCaja.Codcli + ' ' + CCBDMOV.NroDoc SKIP
                    Ccbccaja.coddoc Ccbccaja.nrodoc
                    VIEW-AS ALERT-BOX ERROR.
                NEXT.
            END.
            IF ccbcdocu.CodCli BEGINS "11111111" THEN x-codaux = "11111111111".
            ELSE x-codaux = ccbcdocu.CodCli.
            RUN proc_Graba-Caja(ccbcdocu.coddiv, 2, CCBDMOV.ImpTot, '@CLI', x-codaux, CCBDMOV.NroDoc, x-fchvto, CcbCCaja.TpoCmb).
        END.
    END.
    /* Anticipos */
    IF CcbCCaja.ImpUsa[7] > 0 THEN DO:
        x-codcta = cb-cfgcja.codcta2[7].
        x-codcbd = '34'.
        x-glodoc = 'Canc-Anticip' + CcbCcaja.CodDoc + '-' + CcbCCaja.Nrodoc + ' Cli.' + CcbCCaja.Codcli.
        FIND FIRST CcbCDocu WHERE
            CcbCDocu.CodCia = s-codcia AND
            CcbCDocu.CodCli = CcbCCaja.Codcli AND
            CcbCDocu.CodDoc = 'A/R' AND
            CcbCDocu.NroDoc = CcbCCaja.Voucher[7]
            NO-LOCK NO-ERROR.
        IF NOT AVAILABLE CcbCDocu THEN DO:
            MESSAGE
                'Anticipo no existe ' + CcbCCaja.Codcli + ' ' + CcbCCaja.Voucher[7]
                VIEW-AS ALERT-BOX ERROR.
        END.
        IF AVAILABLE ccbcdocu THEN DO:
            x-codcbd = '34'.
            RUN proc_Graba-Caja(ccbcdocu.coddiv, 2, CcbCCaja.ImpUsa[7], '@CLI', ccbcdocu.CodCli, CcbCCaja.Voucher[7], x-fchvto, CcbCCaja.TpoCmb).
        END.
    END.
    /* Comisiones Factoring */
    IF CcbCCaja.ImpUsa[8] > 0 THEN DO:
        x-codcbd = '32'.
        x-codcta = cb-cfgcja.codcta2[8].
        x-glodoc = 'Comisiones Factoring' + CcbCcaja.CodDoc + '-' + CcbCCaja.Nrodoc + ' Cli.' + CcbCCaja.Codcli.
        RUN proc_Graba-Caja(CcbCCaja.CodDiv, 2, CcbCCaja.ImpUsa[8], '', "", CcbCCaja.Voucher[8], x-fchvto, CcbCCaja.TpoCmb).
    END.
    /* Retenciones */
    IF CcbCCaja.ImpUsa[9] > 0 THEN DO:
        x-codcbd = '20'.
        x-codcta = cb-cfgcja.codcta2[9].
        x-glodoc = 'Retenciones' + CcbCcaja.CodDoc + '-' + CcbCCaja.Nrodoc + ' Cli.' + CcbCCaja.Codcli.
        RUN proc_Graba-Caja(CcbCCaja.CodDiv, 2, CcbCCaja.ImpUsa[9], '', "", CcbCCaja.Voucher[9], x-fchvto, CcbCCaja.TpoCmb).
    END.
    /* Vales Consumo */
    IF CcbCCaja.ImpUsa[10] > 0 THEN DO:
        x-codcbd = '32'.
        x-codcta = cb-cfgcja.codcta2[10].
        x-glodoc = 'Vales Consumo' + CcbCcaja.CodDoc + '-' + CcbCCaja.Nrodoc + ' Cli.' + CcbCCaja.Codcli.
        RUN proc_Graba-Caja(CcbCCaja.CodDiv, 2, CcbCCaja.ImpUsa[10], '', "", '', x-fchvto, CcbCCaja.TpoCmb).
    END.
END. /* FOR EACH CcbCCaja... */

END PROCEDURE.


PROCEDURE proc_Graba-Documento:

    DEFINE INPUT PARAMETER x-coddiv AS CHAR.
    DEFINE INPUT PARAMETER x-codmon AS INTEGER.
    DEFINE INPUT PARAMETER x-import AS DECIMAL.
    DEFINE INPUT PARAMETER x-clfaux AS CHAR.
    DEFINE INPUT PARAMETER x-codref AS CHAR.
    DEFINE INPUT PARAMETER x-nrodoc AS CHAR.
    DEFINE INPUT PARAMETER x-fchvto AS DATE.
    DEFINE INPUT PARAMETER x-tpocmb AS DECIMAL.

    DEFINE VAR x-detalle AS LOGICAL NO-UNDO.
    DEFINE VAR x-codaux AS CHAR.
    DEFINE VAr x-signo  AS DECI.
    DEFINE VAR x-ImpPer AS DEC NO-UNDO.

    FIND CcbCDocu WHERE
        CcbCDocu.CodCia = s-codcia AND
        CcbCDocu.CodDoc = x-CodRef AND 
        CcbCDocu.NroDoc = x-nrodoc
        NO-LOCK NO-ERROR.
    IF NOT AVAILABLE CcbCDocu THEN DO:
        MESSAGE
            'Documento no encontrado en los registros ' +
            x-codref + ' ' + x-nrodoc VIEW-AS ALERT-BOX WARNING.
        RETURN.
    END.
    IF CcbCdocu.FlgEst = "A" THEN RETURN.

    /* RHC 03.09.04 TOMAMOS LA DIVISION DEL DOCUMENTO ORIGEN PARA MATAR SALDOS */
    x-CodDiv = CCBCDOCU.CodDiv.

    /*MLR* 17/01/08 ***/
    IF ccbcdocu.coddoc <> "LET" THEN DO:
        IF LOOKUP(Ccbcdocu.coddoc, 'N/D,N/C') > 0
        THEN FIND Cb-cfgrv WHERE
            Cb-cfgrv.Codcia = S-CODCIA AND
            Cb-cfgrv.CodDiv = Ccbcdocu.CodDiv AND
            Cb-cfgrv.Coddoc = Ccbcdocu.Coddoc AND
            Cb-cfgrv.Fmapgo = Ccbcdocu.Fmapgo AND
            Cb-cfgrv.Codmon = Ccbcdocu.Codmon AND
            Cb-cfgrv.CodRef = Ccbcdocu.CodRef NO-LOCK NO-ERROR.
        ELSE FIND Cb-cfgrv WHERE
            Cb-cfgrv.Codcia = S-CODCIA AND
            Cb-cfgrv.CodDiv = Ccbcdocu.CodDiv AND
            Cb-cfgrv.Coddoc = Ccbcdocu.Coddoc AND
            Cb-cfgrv.Fmapgo = Ccbcdocu.Fmapgo AND
            Cb-cfgrv.Codmon = Ccbcdocu.Codmon NO-LOCK NO-ERROR.
        IF NOT AVAILABLE Cb-cfgrv OR Cb-cfgrv.Codcta = "" THEN DO:
            MESSAGE
                'Cuenta No Configurada Para ' +
                CcbCDocu.Coddoc + ' ' + CcbCDocu.Nrodoc SKIP
                'División:' ccbcdocu.coddiv SKIP
                'Documento:' ccbcdocu.coddoc SKIP
                'Condición de venta:' ccbcdocu.fmapgo SKIP
                'Moneda:' ccbcdocu.codmon SKIP
                'Referencia:' ccbcdocu.codref
                VIEW-AS ALERT-BOX WARNING.
            RETURN.
        END.
    END.
    IF CcbCdocu.CodMon = 2 THEN DO: 
        FIND Gn-tcmb WHERE Gn-tcmb.Fecha = CcbCdocu.FchDoc NO-LOCK NO-ERROR.
        IF AVAILABLE Gn-tcmb THEN x-tpocmb = Gn-tcmb.Venta.
        /* RHC 15.06.06 N/C toma el T.C. del documento de referencia */
        IF Ccbcdocu.coddoc = 'N/C' THEN DO:
            FIND B-CDOCU WHERE
                B-CDOCU.codcia = Ccbcdocu.codcia AND
                B-CDOCU.coddoc = Ccbcdocu.codref AND
                B-CDOCU.nrodoc = Ccbcdocu.nroref NO-LOCK NO-ERROR.
            IF AVAILABLE B-CDOCU THEN DO:
                FIND Gn-tcmb WHERE Gn-tcmb.Fecha = B-CDOCU.FchDoc NO-LOCK NO-ERROR.
                IF AVAILABLE Gn-tcmb THEN x-tpocmb = Gn-tcmb.Venta.
            END.
        END.
    END.
    /* TOMAMOS EL AUXILIAR DEL DOCUMENTO */
    X-CODAUX = CcbCDocu.CodCli.

    IF LOOKUP(CcbCDocu.CodDoc,'BOL,TCK') > 0 THEN
        x-CodAux = IF CcbCDocu.NroCard <> '' THEN CcbCDocu.NroCard ELSE '11111111111'.

    IF ccbcdocu.coddoc <> "LET" THEN DO:
        x-codcta  = Cb-cfgrv.Codcta.
        x-detalle = Cb-cfgrv.Detalle.
        x-glodoc  = 'Canc.' + CcbDCaja.CodRef + '-' + CcbDCaja.NroRef + ' Cli.' + CcbDCaja.Codcli.
    END.
    ELSE DO:
        x-codcta = x-ctalet[ccbcdocu.codmon].
        x-detalle = TRUE.
    END.

    X-NRODOC = IF X-DETALLE THEN CcbCDocu.Nrodoc ELSE "111111111".
    X-NRODOC = IF X-DETALLE THEN CcbCDocu.Nrodoc ELSE SUBSTRING(CcbCDocu.Nrodoc,1,3) + "111111".

    FIND cb-ctas WHERE
        cb-ctas.CodCia = cb-codcia AND
        cb-ctas.CodCta = x-codcta
        NO-LOCK NO-ERROR.

    IF AVAILABLE cb-ctas AND NOT cb-ctas.PidAux THEN
        ASSIGN
            x-codaux = ""
            x-clfaux = "".

    IF NOT x-detalle THEN DO:
        x-glodoc  = 'Resumen ' + '-' + CcbDCaja.CodRef.
    END.
    x-signo = IF CcbCCaja.Coddoc = "I/C" THEN 1 ELSE -1.

    FIND t-prev WHERE t-prev.coddoc = x-codcbd 
        AND t-prev.nrodoc = x-nrodoc 
        AND t-prev.codcta = x-codcta
        NO-ERROR.
    IF NOT AVAILABLE t-prev THEN DO:
        CREATE t-prev.
        ASSIGN
            t-prev.tipo    = STRING(CcbCCaja.FchCie, '99/99/9999') + CcbCCaja.Usuario + CcbCCaja.HorCie  /*CcbCCaja.Usuario + CcbCCaja.HorCie*/
            t-prev.periodo = x-periodo
            t-prev.nromes  = x-nromes
            t-prev.codope  = x-codope
            t-prev.coddiv  = x-CodDiv
            t-prev.codmon  = x-codmon
            t-prev.codcta  = x-codcta
            t-prev.fchdoc  = CcbCCaja.FchDoc 
            t-prev.fchvto  = x-fchvto
            t-prev.tpomov  = x-tpomov
            t-prev.clfaux  = x-clfaux
            t-prev.codaux  = x-codaux
            t-prev.coddoc  = x-Codcbd
            t-prev.nrodoc  = x-nrodoc
            t-prev.Tpocmb  = x-TpoCmb
            t-prev.glodoc  = x-glodoc
            t-prev.nroref  = CcbCCaja.Nrodoc.
    END.
    /* PAGO SIN INCLUIR LA PERCEPCION (UNA PROPORCION DE LA PERCEPCION) */
    IF Ccbcdocu.ImpTot <> 0 THEN
        ASSIGN
        x-ImpPer = Ccbcdocu.AcuBon[5] * ( x-Import / Ccbcdocu.ImpTot )
        x-Import = x-Import - x-ImpPer.
    IF x-codmon = 1 THEN
        ASSIGN
            t-prev.impmn1  = t-prev.impmn1 + x-import * x-signo.
    ELSE
        ASSIGN
            t-prev.impmn2  = t-prev.impmn2 + x-import * x-signo
            t-prev.impmn1  = t-prev.impmn1 + ROUND((x-import * x-tpocmb),2) * x-signo.

    /* PAGO DE LA PERCEPCION */
    IF LOOKUP(Ccbcdocu.coddoc, 'FAC,BOL,TCK') > 0 AND Ccbcdocu.AcuBon[5] > 0 THEN DO:
        x-CodCta = "40111301".
        FIND t-prev WHERE t-prev.coddoc = x-codcbd 
            AND t-prev.nrodoc = x-nrodoc 
            AND t-prev.codcta = x-codcta
            NO-ERROR.
        IF NOT AVAILABLE t-prev THEN DO:
            CREATE t-prev.
            ASSIGN
                t-prev.tipo    = STRING(CcbCCaja.FchCie, '99/99/9999') + CcbCCaja.Usuario + CcbCCaja.HorCie /*CcbCCaja.Usuario + CcbCCaja.HorCie*/
                t-prev.periodo = x-periodo
                t-prev.nromes  = x-nromes
                t-prev.codope  = x-codope
                t-prev.coddiv  = x-CodDiv
                t-prev.codmon  = x-codmon
                t-prev.codcta  = x-codcta
                t-prev.fchdoc  = CcbCCaja.FchDoc 
                t-prev.fchvto  = x-fchvto
                t-prev.tpomov  = x-tpomov
                t-prev.clfaux  = x-clfaux
                t-prev.codaux  = x-codaux
                t-prev.coddoc  = x-Codcbd
                t-prev.nrodoc  = x-nrodoc
                t-prev.Tpocmb  = x-TpoCmb
                t-prev.glodoc  = x-glodoc
                t-prev.nroref  = CcbCCaja.Nrodoc.
        END.
        /* PAGO SIN INCLUIR LA PERCEPCION */
        /*x-Import = Ccbcdocu.AcuBon[5].*/
        IF x-codmon = 1 THEN
            ASSIGN
                t-prev.impmn1  = t-prev.impmn1 + x-ImpPer * x-signo.
        ELSE
            ASSIGN
                t-prev.impmn2  = t-prev.impmn2 + x-ImpPer * x-signo
                t-prev.impmn1  = t-prev.impmn1 + ROUND((x-ImpPer * x-tpocmb),2) * x-signo.
    END.


    RELEASE t-prev.
   
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

    CREATE t-prev.
    ASSIGN
        t-prev.tipo    = STRING(CcbCCaja.FchCie, '99/99/9999') + CcbCCaja.Usuario + CcbCCaja.HorCie /*CcbCCaja.Usuario + CcbCCaja.HorCie*/
        t-prev.periodo = x-periodo
        t-prev.nromes  = x-nromes
        t-prev.codope  = x-codope
        t-prev.coddiv  = x-CodDiv 
        t-prev.codmon  = x-codmon
        t-prev.codcta  = x-codcta
        t-prev.fchdoc  = CcbCCaja.FchDoc
        t-prev.fchvto  = x-fchvto
        t-prev.tpomov  = x-tpomov
        t-prev.clfaux  = x-clfaux
        t-prev.codaux  = x-codaux
        t-prev.coddoc  = x-Codcbd
        t-prev.nrodoc  = x-nrodoc
        t-prev.Tpocmb  = x-TpoCmb
        t-prev.glodoc  = x-glodoc
/*MLR-1*/ t-prev.Codref = CcbCCaja.Coddoc
        t-prev.nroref  = CcbCCaja.Nrodoc.

    IF x-codmon = 1 THEN
        ASSIGN t-prev.impmn1  = t-prev.impmn1 + x-import.
    ELSE
        ASSIGN
            t-prev.impmn2  = t-prev.impmn2 + x-import
            t-prev.impmn1  = t-prev.impmn1 + ROUND((x-import * x-tpocmb),2).
    RELEASE t-prev.

END PROCEDURE.

PROCEDURE Graba-BD:

    DEFINE INPUT PARAMETER x-coddiv AS CHAR.
    DEFINE INPUT PARAMETER x-codmon AS INTEGER.
    DEFINE INPUT PARAMETER x-import AS DECIMAL.
    DEFINE INPUT PARAMETER x-clfaux AS CHAR.
    DEFINE INPUT PARAMETER x-codref AS CHAR.
    DEFINE INPUT PARAMETER x-nrodoc AS CHAR.
    DEFINE INPUT PARAMETER x-fchvto AS DATE.
    DEFINE INPUT PARAMETER x-tpocmb AS DECIMAL.

    DEFINE VAR x-detalle AS LOGICAL NO-UNDO.
    DEFINE VAR x-codaux AS CHAR.
    DEFINE VAR x-signo  AS DECI.

    x-signo = 1.

    FIND FIRST CcbCDocu WHERE CcbCDocu.CodCia = s-codcia AND
        CcbCDocu.CodDoc = x-CodRef AND 
        CcbCDocu.NroDoc = x-nrodoc
        NO-LOCK NO-ERROR.
    IF NOT AVAILABLE CcbCDocu THEN DO:
       RETURN.
    END.   
    IF CcbCdocu.FlgEst = "A" THEN RETURN.

    /* RHC 03.09.04 TOMAMOS LA DIVISION DEL DOCUMENTO ORIGEN PARA MATAR SALDOS */
    x-CodDiv = CCBCDOCU.CodDiv.
    /* ***************************************************** */
    IF CcbCdocu.CodMon = 2 THEN DO: 
       FIND Gn-tcmb WHERE Gn-tcmb.Fecha = CcbCdocu.FchDoc
                          NO-LOCK NO-ERROR.
       IF AVAILABL Gn-tcmb THEN x-tpocmb = Gn-tcmb.Venta.                    
    END.                      

    /* RHC 03.09.04 TOMAMOS EL AUXILIAR DEL CLIENTE */
    X-CODAUX = CcbCDocu.CodCli.

    FIND cb-ctas WHERE cb-ctas.CodCia = cb-codcia AND
                       cb-ctas.CodCta = x-codcta
                       NO-LOCK NO-ERROR.

    IF AVAILABLE cb-ctas AND NOT cb-ctas.PidAux 
    THEN ASSIGN
          x-codaux = ""
          x-clfaux = "".

    CREATE t-prev.
    ASSIGN
        t-prev.tipo    = CcbCCaja.Usuario + CcbCCaja.HorCie
        t-prev.periodo = x-periodo
        t-prev.nromes  = x-nromes
        t-prev.codope  = x-codope
        t-prev.coddiv  = x-CodDiv 
        t-prev.codmon  = x-codmon
        t-prev.codcta  = x-codcta
        t-prev.fchdoc  = CcbCCaja.FchDoc 
        t-prev.fchvto  = x-fchvto
        t-prev.tpomov  = x-tpomov
        t-prev.clfaux  = x-clfaux
        t-prev.codaux  = x-codaux
        t-prev.coddoc  = x-Codcbd
        t-prev.nrodoc  = x-nrodoc
        t-prev.Tpocmb  = x-TpoCmb
        t-prev.glodoc  = x-glodoc
        t-prev.nroref  = CcbCCaja.Nrodoc.
    IF x-codmon = 1 THEN
        ASSIGN
           t-prev.impmn1  = t-prev.impmn1 + x-import * x-signo.
    ELSE 
        ASSIGN
           t-prev.impmn2  = t-prev.impmn2 + x-import * x-signo
           t-prev.impmn1  = t-prev.impmn1 + ROUND((x-import * x-tpocmb),2) * x-signo.
    RELEASE t-prev.

END PROCEDURE.

PROCEDURE Graba-N/C:

    DEFINE INPUT PARAMETER x-coddiv AS CHAR.
    DEFINE INPUT PARAMETER x-codmon AS INTEGER.
    DEFINE INPUT PARAMETER x-import AS DECIMAL.
    DEFINE INPUT PARAMETER x-clfaux AS CHAR.
    DEFINE INPUT PARAMETER x-codref AS CHAR.
    DEFINE INPUT PARAMETER x-nrodoc AS CHAR.
    DEFINE INPUT PARAMETER x-fchvto AS DATE.
    DEFINE INPUT PARAMETER x-tpocmb AS DECIMAL.

    DEFINE VAR x-detalle AS LOGICAL NO-UNDO.
    DEFINE VAR x-codaux AS CHAR.
    DEFINE VAr x-signo  AS DECI.

    x-signo = 1.

    FIND CcbCDocu WHERE CcbCDocu.CodCia = s-codcia AND
                        /*CcbCDocu.CodDiv = x-coddiv AND*/
                        CcbCDocu.CodDoc = x-CodRef AND 
                        CcbCDocu.NroDoc = x-nrodoc
                        NO-LOCK NO-ERROR.

    IF NOT AVAILABLE CcbCDocu THEN DO:
    /*   MESSAGE 
     *         'Documento no encontrado en los registros ' + x-codref + ' ' + x-nrodoc  VIEW-AS ALERT-BOX.*/
       RETURN.
    END.   
    IF CcbCdocu.FlgEst = "A" THEN RETURN.

    /* RHC 03.09.04 TOMAMOS LA DIVISION DEL DOCUMENTO ORIGEN PARA MATAR SALDOS */
    x-CodDiv = CCBCDOCU.CodDiv.
    /* ***************************************************** */

    FIND B-CDocu WHERE B-CDocu.Codcia = S-CODCIA AND
                       /*B-CDocu.Coddiv = CcbCDocu.Coddiv AND*/
                       B-CDocu.Coddoc = CcbCDocu.Codref AND
                       B-CDocu.Nrodoc = CcbCDocu.Nroref NO-LOCK NO-ERROR.

    IF NOT AVAILABLE B-CDocu THEN DO:
      MESSAGE 'Documento ' + CcbCDocu.Coddoc + ' ' + CcbCDocu.Nrodoc SKIP
              'Referencia  ' + CcbCDocu.CodRef + ' ' + CcbCDocu.NroRef + ' No Existe ' 
              VIEW-AS ALERT-BOX.
      RETURN .
    END.

    FIND Cb-cfgrv WHERE Cb-cfgrv.Codcia = S-CODCIA 
        AND Cb-cfgrv.CodDiv = F-DIVISION 
        AND Cb-cfgrv.Coddoc = X-codref 
        AND Cb-cfgrv.CodRef = CcbCDocu.Codref 
        AND Cb-cfgrv.Fmapgo = B-CDocu.Fmapgo 
        AND Cb-cfgrv.Codmon = Ccbcdocu.Codmon NO-LOCK NO-ERROR.
    IF NOT AVAILABLE Cb-cfgrv OR Cb-cfgrv.Codcta = "" THEN DO:
        /*MESSAGE 'Cuenta No Configurada Para ' + CcbCDocu.Coddoc + ' ' + CcbCDocu.Nrodoc  VIEW-AS ALERT-BOX.*/
        MESSAGE 'Cuenta No Configurada' SKIP
            'División:' f-Division SKIP
            'Documento:' x-codref SKIP
            'Referencia:' ccbcdocu.codref SKIP
            'Fma de pago:' b-cdocu.fmapgo SKIP
            'Moneda:' ccbcdocu.codmon
            VIEW-AS ALERT-BOX.
        RETURN.
    END.

    IF CcbCdocu.CodMon = 2 THEN DO: 
       FIND Gn-tcmb WHERE Gn-tcmb.Fecha = CcbCdocu.FchDoc
                          NO-LOCK NO-ERROR.
       IF AVAILABL Gn-tcmb THEN x-tpocmb = Gn-tcmb.Venta.                    
    END.                      

    /* RHC 03.09.04 TOMAMOS EL AUXILIAR DEL CLIENTE */
    X-CODAUX = CcbCDocu.CodCli.

    /*X-CODAUX = CCbcdocu.Ruccli.
     * IF LENGTH(X-CODAUX) > 8 THEN X-CODAUX = SUBSTRING(X-CODAUX,3,8).*/

    ASSIGN
        x-codcta  = Cb-cfgrv.Codcta
        x-detalle = Cb-cfgrv.Detalle
        x-glodoc  = 'Canc.' + x-codref + '-' + x-nrodoc + ' Cli.' + CcbCCaja.Codcli.      

    /*MLR* 10/12/07 ***/
    X-NRODOC = IF X-DETALLE THEN CcbCDocu.Nrodoc ELSE "111111111".
    X-NRODOC = IF X-DETALLE THEN CcbCDocu.Nrodoc ELSE SUBSTRING(CcbCDocu.Nrodoc,1,3) + "111111".
    /* ***/

    FIND cb-ctas WHERE cb-ctas.CodCia = cb-codcia AND
                       cb-ctas.CodCta = x-codcta
                       NO-LOCK NO-ERROR.

    IF AVAILABLE cb-ctas AND NOT cb-ctas.PidAux 
    THEN ASSIGN
          x-codaux = ""
          x-clfaux = "".

    FIND t-prev WHERE t-prev.coddoc = x-codcbd AND
                      t-prev.nrodoc = x-nrodoc 
                      NO-ERROR.
    IF NOT AVAILABLE t-prev THEN DO:                  

     CREATE t-prev.
     ASSIGN
        t-prev.tipo    = CcbCCaja.Usuario + CcbCCaja.HorCie
        t-prev.periodo = x-periodo
        t-prev.nromes  = x-nromes
        t-prev.codope  = x-codope
        t-prev.coddiv  = x-CodDiv 
        t-prev.codmon  = x-codmon
        t-prev.codcta  = x-codcta
        t-prev.fchdoc  = CcbCCaja.FchDoc 
        t-prev.fchvto  = x-fchvto
        t-prev.tpomov  = x-tpomov
        t-prev.clfaux  = x-clfaux
        t-prev.codaux  = x-codaux
        t-prev.coddoc  = x-Codcbd
        t-prev.nrodoc  = x-nrodoc
        t-prev.Tpocmb  = x-TpoCmb
        t-prev.glodoc  = x-glodoc
        t-prev.nroref  = CcbCCaja.Nrodoc.
    END.    
     IF x-codmon = 1 THEN
        ASSIGN
           t-prev.impmn1  = t-prev.impmn1 + x-import * x-signo.
    /*       t-prev.impmn2  = t-prev.impmn2 + ROUND((x-import / CcbCCaja.TpoCmb),2).*/
     ELSE 
        ASSIGN
           t-prev.impmn2  = t-prev.impmn2 + x-import * x-signo
           t-prev.impmn1  = t-prev.impmn1 + ROUND((x-import * x-tpocmb),2) * x-signo.


     RELEASE t-prev.

END PROCEDURE.

PROCEDURE proc_Graba-Diferencia:

    DEFINE VAR x-s AS DECIMAL NO-UNDO.
    DEFINE VAR x-d AS DECIMAL NO-UNDO.
    DEFINE BUFFER B-prev FOR t-prev.

    FOR EACH t-prev NO-LOCK
        BREAK BY t-prev.tipo:
        IF FIRST-OF(t-prev.tipo) THEN DO:
            x-s = 0.
            x-d = 0.
        END.
        x-s = x-s + (t-prev.ImpMn1 * (IF t-prev.tpomov THEN -1 ELSE 1) ).
        x-d = x-d + (t-prev.ImpMn2 * (IF t-prev.tpomov THEN -1 ELSE 1) ).
        IF LAST-OF(t-prev.tipo) THEN DO:
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
        B-prev.coddiv  = f-Division     /*t-prev.CodDiv */
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

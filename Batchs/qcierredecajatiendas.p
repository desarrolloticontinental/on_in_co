&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12
&ANALYZE-RESUME
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS Procedure 
/*------------------------------------------------------------------------
    File        : qregistroventas.p
    Purpose     : Genera un registro en la tabla wmigrv

    Syntax      :

    Description :  Migración de ventas al contado en base a los cierres del día anterior

    Author(s)   :
    Created     :
    Notes       :
  ----------------------------------------------------------------------*/
/*          This .W file was created with the Progress AppBuilder.      */
/*----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Procedure
&Scoped-define DB-AWARE no



/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: Procedure
   Allow: 
   Frames: 0
   Add Fields to: Neither
   Other Settings: CODE-ONLY COMPILE
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
/* DESIGN Window definition (used by the UIB) 
  CREATE WINDOW Procedure ASSIGN
         HEIGHT             = 5.92
         WIDTH              = 60.
/* END WINDOW DEFINITION */
                                                                        */
&ANALYZE-RESUME

 


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK Procedure 


/* ***************************  Main Block  *************************** */
DEF VAR s-codcia AS INT INIT 001 NO-UNDO.
DEF VAR s-user-id AS CHAR INIT "AUTOMATICO".
DEF VAR cl-codcia AS INT INIT 000.
DEF VAR cb-codcia AS INT INIT 000.

DEFINE VAR x-tpomov  AS LOGICAL NO-UNDO.
DEFINE VAR x-codcbd  AS CHAR    NO-UNDO.
DEFINE VAR x-glodoc  AS CHAR    NO-UNDO.
DEFINE VAR x-resumen AS LOGICAL NO-UNDO.
DEFINE VAR x-tpocmb  AS DECI NO-UNDO.
DEFINE VAR x-codcta  AS CHAR    NO-UNDO.
DEFINE VAR x-coddiv  AS CHAR NO-UNDO.
DEFINE VAR x-fchvto  AS DATE NO-UNDO.
DEFINE VAR x-codaux  AS CHAR NO-UNDO.

x-resumen = FALSE.

DEFINE TEMP-TABLE t-wmigmc LIKE wmigmc.

FIND cb-cfgg WHERE cb-cfgg.CodCia = cb-codcia 
    AND cb-cfgg.Codcfg = 'R02' NO-LOCK NO-ERROR.
IF NOT AVAILABLE cb-cfgg THEN DO:
    PUT UNFORMATTED
        "ERROR: NO está configurado las cuentas R02 en el cb-cfgg" SKIP.
    RETURN.
END.



/* LOS INGRESOS A CAJA */
FOR EACH CcbCierr NO-LOCK WHERE CcbCierr.codcia = s-codcia 
    /*AND CcbCierr.FchCie = TODAY - 1:*/
    AND fchcie = 06/01/2012
    AND INDEX(usuario, '-01') > 0:
    PUT UNFORMATTED CcbCierr.FchCie CcbCierr.HorCie CcbCierr.usuario SKIP.
    EMPTY TEMP-TABLE t-wmigmc.
    FOR EACH CcbCCaja NO-LOCK WHERE CcbCCaja.CodCia = Ccbcierr.codcia 
        AND CcbCCaja.FlgCie = 'C' 
        AND CcbCCaja.FchCie = CcbCierr.FchCie
        AND CcbCCaja.Coddoc = "I/C"
        AND CcbCCaja.FlgEst <> "A":
        FIND cb-cfgcja WHERE cb-cfgcja.Codcia = Ccbcierr.codcia 
            AND cb-cfgcja.CodDiv = Ccbccaja.coddiv
            NO-LOCK NO-ERROR.
        IF NOT AVAILABLE cb-cfgcja THEN DO:
            NEXT.
        END.
        ASSIGN
            x-coddiv = Ccbccaja.coddiv
            x-tpomov = FALSE
            x-tpocmb = 0
            x-tpomov = FALSE.
        /* EFECTIVO */
        ASSIGN
            x-tpocmb = 0
            x-codcbd = 'EF'
            x-glodoc = 'Efectivo-Recibido' + CcbCcaja.CodDoc + '-' + CcbCCaja.Nrodoc + ' Cli.' + CcbCCaja.Codcli.
        IF CcbCCaja.ImpNac[1] <> 0 THEN DO:
            x-codcta = cb-cfgcja.codcta1[1].
            RUN proc_Graba-Caja(CcbCCaja.Coddiv, 1, CcbCCaja.ImpNac[1], 'CL', CcbCCaja.Codcli, SUBSTRING(CcbCCaja.Voucher[1],4,15), x-fchvto, x-tpocmb).
        END.
        IF CcbCCaja.ImpUsa[1] <> 0 THEN DO:
            x-tpocmb = CcbCCaja.TpoCmb.
            x-codcta = cb-cfgcja.codcta2[1].
            RUN proc_Graba-Caja(CcbCCaja.Coddiv, 2, CcbCCaja.ImpUsa[1], 'CL', CcbCCaja.Codcli, SUBSTRING(CcbCCaja.Voucher[1],4,15), x-fchvto, x-tpocmb).
        END.                
        /* CHEQUES */
        ASSIGN
            x-codcbd = 'CH'     /*'32'*/
            x-glodoc = 'Canc-Cheque' + CcbCcaja.CodDoc + '-' + CcbCCaja.Nrodoc + ' Cli.' + CcbCCaja.Codcli.
        IF CcbCCaja.ImpNac[2] > 0 THEN DO:
            x-codcta = cb-cfgcja.codcta1[2].
            x-tpocmb = 0.
            RUN proc_Graba-Caja(x-Coddiv, 1, CcbCCaja.ImpNac[2], '', "", CcbCCaja.Voucher[2], x-fchvto, x-tpocmb).
        END.
        IF CcbCCaja.ImpUsa[2] > 0 THEN DO:
            x-codcta = cb-cfgcja.codcta2[2].
            RUN proc_Graba-Caja(x-Coddiv, 2, CcbCCaja.ImpUsa[2], '', "", CcbCCaja.Voucher[2], x-fchvto, CcbCCaja.TpoCmb).
        END.
        /* CHEQUES DIFERIDOS */
        ASSIGN
            x-codcbd = 'CH'     /*'32'*/
            x-glodoc = 'Canc-Cheque' + CcbCcaja.CodDoc + '-' + CcbCCaja.Nrodoc + ' Cli.' + CcbCCaja.Codcli.
        IF CcbCCaja.ImpNac[3] > 0 THEN DO:
            x-codcta = cb-cfgcja.codcta1[3].       
            x-tpocmb = 0.
            RUN proc_Graba-Caja(x-Coddiv, 1, CcbCCaja.ImpNac[3], '', "", CcbCCaja.Voucher[3], CcbCCaja.FchVto[3], x-tpocmb).
        END.
        IF CcbCCaja.ImpUsa[3] > 0 THEN DO:
            x-codcta = cb-cfgcja.codcta2[3].       
            RUN proc_Graba-Caja(x-Coddiv, 2, CcbCCaja.ImpUsa[3], '', "", CcbCCaja.Voucher[3], CcbCCaja.FchVto[3], CcbCCaja.TpoCmb).
        END.
        /* TARJETA DE CREDITO */
        ASSIGN
            x-codcbd = 'TC'
            x-glodoc = 'Canc-Tarjeta-Credito' + CcbCcaja.CodDoc + '-' + CcbCCaja.Nrodoc + ' Cli.' + CcbCCaja.Codcli.
        IF CcbCCaja.ImpNac[4] > 0 THEN DO:
            x-codcta = cb-cfgcja.codcta1[4].
            x-codaux = ''.
            x-tpocmb = 0.
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
            RUN proc_Graba-Caja(x-Coddiv, 1, CcbCCaja.ImpNac[4], 'OT', x-codaux, CcbCCaja.Voucher[4], x-fchvto, x-tpocmb).
        END.
        x-tpocmb = 0.
        FIND Gn-tcmb WHERE Gn-tcmb.Fecha = CcbCCaja.FchDoc NO-LOCK NO-ERROR.
        IF AVAILABL Gn-tcmb THEN x-tpocmb = Gn-tcmb.compra.
        IF CcbCCaja.ImpUsa[4] > 0 THEN DO:
            x-codcta = cb-cfgcja.codcta2[4].
            x-codaux = ''.
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
            RUN proc_Graba-Caja(x-Coddiv, 2, CcbCCaja.ImpUsa[4], 'OT', x-codaux, CcbCCaja.Voucher[4], x-fchvto, x-tpocmb).
        END.
        /* DEPOSITOS */
        ASSIGN
            x-codcbd = 'BD'     /*'36'*/
            x-codcta = '12211100'
            x-glodoc = 'Canc-Deposito' + CcbCcaja.CodDoc + '-' + CcbCCaja.Nrodoc + ' Cli.' + CcbCCaja.Codcli
            x-tpocmb = 0.
        FIND ccbcdocu WHERE
            ccbcdocu.CodCia = S-CodCia AND
            ccbcdocu.CodDoc = "BD" AND
            ccbcdocu.CodCli = CcbCCaja.Codcli AND
            ccbcdocu.nrodoc = CcbCCaja.Voucher[5]
            NO-LOCK NO-ERROR.
        IF CcbCCaja.ImpNac[5] > 0 THEN DO:
            IF NOT AVAILABLE ccbcdocu THEN DO:
                MESSAGE 'Depósito no se encuentra registrado ' + CcbCCaja.Codcli + ' ' + CcbCCaja.Voucher[5]
                    VIEW-AS ALERT-BOX WARNING.
            END.
            ELSE DO:
                RUN proc_Graba-Caja(ccbcdocu.coddiv, 1, CcbCCaja.ImpNac[5],'@CLI', ccbcdocu.CodCli, CcbCCaja.Voucher[5], x-fchvto, x-tpocmb).
            END.
        END.
        IF CcbCCaja.ImpUsa[5] > 0 THEN DO:
            IF NOT AVAILABLE ccbcdocu THEN DO:
                MESSAGE
                    'Depósito no se encuentra registrado ' + CcbCCaja.Codcli + ' ' + CcbCCaja.Voucher[5] 
                    VIEW-AS ALERT-BOX WARNING.
            END.
            ELSE DO:
                RUN proc_Graba-Caja(ccbcdocu.coddiv, 2, CcbCCaja.ImpUsa[5], '@CLI', ccbcdocu.CodCli, CcbCCaja.Voucher[5], x-fchvto, CcbCCaja.TpoCmb).
            END.
        END.
        /* NOTA DE CREDITO */
        ASSIGN
            x-codcbd = 'NC'     /*'07'*/
            x-glodoc = 'Canc-N/C' + CcbCcaja.CodDoc + '-' + CcbCCaja.Nrodoc + ' Cli.' + CcbCCaja.Codcli
            x-tpocmb = 0.
        IF CcbCCaja.ImpNac[6] > 0 THEN DO:
            x-codcta = cb-cfgcja.codcta1[6].
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
                RUN proc_Graba-Caja(ccbcdocu.coddiv, 1, CCBDMOV.ImpTot, 'CL', x-codaux, CcbCDocu.NroDoc, x-fchvto, x-tpocmb).
            END.
        END.
        IF CcbCCaja.ImpUsa[6] > 0 THEN DO:
            x-codcta = cb-cfgcja.codcta2[6].
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
                RUN proc_Graba-Caja(ccbcdocu.coddiv, 2, CCBDMOV.ImpTot, 'CL', x-codaux, CCBDMOV.NroDoc, x-fchvto, CcbCCaja.TpoCmb).
            END.
        END.
        /* ANTICIPOS RECIBIDOS */
        ASSIGN
            x-codcbd = 'IC'     /*'34'*/
            x-glodoc = 'Canc-Anticip' + CcbCcaja.CodDoc + '-' + CcbCCaja.Nrodoc + ' Cli.' + CcbCCaja.Codcli
            x-tpocmb = 0.
        FIND FIRST CcbCDocu WHERE
            CcbCDocu.CodCia = s-codcia AND
            CcbCDocu.CodCli = CcbCCaja.Codcli AND
            CcbCDocu.CodDoc = 'A/R' AND
            CcbCDocu.NroDoc = CcbCCaja.Voucher[7]
            NO-LOCK NO-ERROR.
        IF CcbCCaja.ImpNac[7] > 0 THEN DO:
            x-codcta = cb-cfgcja.codcta1[7].
            IF NOT AVAILABLE CcbCDocu THEN DO:
                MESSAGE
                    'Anticipo no existe ' + CcbCCaja.Codcli + ' ' + CcbCCaja.Voucher[7]
                    VIEW-AS ALERT-BOX ERROR.
            END.
            ELSE DO:
                RUN proc_Graba-Caja(ccbcdocu.coddiv, 1, CcbCCaja.ImpNac[7], '@CLI', ccbcdocu.CodCli, CcbCCaja.Voucher[7], x-fchvto, x-tpocmb).
            END.
        END.
        IF CcbCCaja.ImpUsa[7] > 0 THEN DO:
            x-codcta = cb-cfgcja.codcta2[7].
            IF NOT AVAILABLE CcbCDocu THEN DO:
                MESSAGE
                    'Anticipo no existe ' + CcbCCaja.Codcli + ' ' + CcbCCaja.Voucher[7]
                    VIEW-AS ALERT-BOX ERROR.
            END.
            ELSE DO:
                RUN proc_Graba-Caja(ccbcdocu.coddiv, 2, CcbCCaja.ImpUsa[7], '@CLI', ccbcdocu.CodCli, CcbCCaja.Voucher[7], x-fchvto, CcbCCaja.TpoCmb).
            END.
        END.
        /* COMISIONES */
        ASSIGN
            x-codcbd = 'IC'     /*'32'*/
            x-tpocmb = 0
            x-glodoc = 'Comision Factoring' + CcbCcaja.CodDoc + '-' + CcbCCaja.Nrodoc + ' Cli.' + CcbCCaja.Codcli.
        IF CcbCCaja.ImpNac[8] > 0 THEN DO:
            x-codcta = cb-cfgcja.codcta1[8].
            RUN proc_Graba-Caja(CcbCCaja.CodDiv, 1, CcbCCaja.ImpNac[8], '', "", CcbCCaja.Voucher[8], x-fchvto, CcbCCaja.TpoCmb).
        END.
        IF CcbCCaja.ImpUsa[8] > 0 THEN DO:
            x-codcta = cb-cfgcja.codcta2[8].
            RUN proc_Graba-Caja(CcbCCaja.CodDiv, 2, CcbCCaja.ImpUsa[8], '', "", CcbCCaja.Voucher[8], x-fchvto, CcbCCaja.TpoCmb).
        END.
        /* RETENCIONES */
        ASSIGN
            x-codcbd = 'RE'     /*'20'*/
            x-tpocmb = 0
            x-glodoc = 'Retenciones' + CcbCcaja.CodDoc + '-' + CcbCCaja.Nrodoc + ' Cli.' + CcbCCaja.Codcli.
        IF CcbCCaja.ImpNac[9] > 0 THEN DO:
            x-codcta = cb-cfgcja.codcta1[9].
            RUN proc_Graba-Caja(CcbCCaja.CodDiv, 1, CcbCCaja.ImpNac[9], '', "", CcbCCaja.Voucher[9], x-fchvto, CcbCCaja.TpoCmb).
        END.
        IF CcbCCaja.ImpUsa[9] > 0 THEN DO:
            x-codcta = cb-cfgcja.codcta2[9].
            RUN proc_Graba-Caja(CcbCCaja.CodDiv, 2, CcbCCaja.ImpUsa[9], '', "", CcbCCaja.Voucher[9], x-fchvto, CcbCCaja.TpoCmb).
        END.
        /* VALES DE CONSUMO */
        ASSIGN
            x-codcbd = 'VL'     /*'32'*/
            x-tpocmb = 0
            x-glodoc = 'Vales Consumo' + CcbCcaja.CodDoc + '-' + CcbCCaja.Nrodoc + ' Cli.' + CcbCCaja.Codcli.
        IF CcbCCaja.ImpNac[10] > 0 THEN DO:
            x-codcta = cb-cfgcja.codcta1[10].
            RUN proc_Graba-Caja(CcbCCaja.CodDiv, 1, CcbCCaja.ImpNac[10], 'OT', "", '', x-fchvto, CcbCCaja.TpoCmb).
        END.
        IF CcbCCaja.ImpUsa[10] > 0 THEN DO:
            x-codcta = cb-cfgcja.codcta2[10].
            RUN proc_Graba-Caja(CcbCCaja.CodDiv, 2, CcbCCaja.ImpUsa[10], 'OT', "", '', x-fchvto, CcbCCaja.TpoCmb).
        END.
    END.
    /* BARREMOS los I/C por MOSTRADOR */
    RUN Transferir-Asiento.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&IF DEFINED(EXCLUDE-Carga-FAC-BOL) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Carga-FAC-BOL Procedure 
PROCEDURE Carga-FAC-BOL :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

DEF VAR x-codcta AS CHAR.
DEF VAR x-codigv AS CHAR.
DEF VAR x-ctadto AS CHAR.
DEF VAR x-ctaisc AS CHAR.
DEF VAR x-ctaigv AS CHAR.
DEF VAR x-fchvto AS DATE.
DEF VAR x-coddoc AS CHAR.
DEF VAR x-nrodoc AS CHAR.
DEF VAR x-codcli AS CHAR.
DEF VAR x-codmon AS INT.
DEF VAR x-fchdoc AS INT.

DEF VAR z-nrodoc AS CHAR.
DEF VAR lHora AS CHAR.

DEFINE VAR x-detalle AS LOGICAL NO-UNDO.
DEFINE VAR x-cco     AS CHAR    NO-UNDO.

/*
FIND TabDistr WHERE TabDistr.CodDepto = gn-clie.coddep
    AND TabDistr.CodProvi = gn-clie.codprov
    AND TabDistr.CodDistr = gn-clie.coddist
    NO-LOCK NO-ERROR.
FIND Cb-cfgrv WHERE Cb-cfgrv.Codcia = Ccbcdocu.Codcia 
    AND Cb-cfgrv.CodDiv = Ccbcdocu.Coddiv 
    AND Cb-cfgrv.Coddoc = Ccbcdocu.Coddoc 
    AND Cb-cfgrv.Fmapgo = Ccbcdocu.Fmapgo 
    AND Cb-cfgrv.Codmon = Ccbcdocu.Codmon 
    NO-LOCK NO-ERROR.
IF NOT AVAILABLE Cb-cfgrv OR Cb-cfgrv.Codcta = "" 
    THEN ASSIGN
            x-detalle = YES
            x-codcta = (IF ccbcdocu.codmon = 1 THEN FacDocum.CodCta[1] ELSE FacDocum.CodCta[2]).
    ELSE ASSIGN
            x-detalle = Cb-cfgrv.Detalle
            x-codcta = Cb-cfgrv.codcta.
ASSIGN
    x-ctaisc = cb-cfgg.codcta[2] 
    x-ctaigv = cb-cfgg.codcta[3]
    x-ctadto = cb-cfgg.codcta[10]
    x-fchvto = (IF ccbcdocu.fchvto < ccbcdocu.fchdoc THEN ccbcdocu.fchdoc ELSE ccbcdocu.fchvto)
    x-coddoc = Ccbcdocu.coddoc
    x-nrodoc = Ccbcdocu.nrodoc
    x-codcli = Ccbcdocu.codcli
    x-codmon = (IF ccbcdocu.codmon = 1 THEN 00 ELSE 01)
    x-fchdoc = YEAR(ccbcdocu.fchdoc) * 10000 + MONTH(ccbcdocu.fchdoc) * 100 + DAY(ccbcdocu.fchdoc).
CASE ccbcdocu.coddoc:
    WHEN 'FAC' THEN x-coddoc = "FC".
    WHEN 'BOL' THEN x-coddoc = "BV".
    WHEN 'TCK' THEN x-coddoc = "TK".
    WHEN 'LET' THEN x-coddoc = "LT".
    WHEN 'N/C' THEN x-coddoc = "NC".
    WHEN 'N/D' THEN x-coddoc = "ND".
    WHEN 'CHQ' THEN x-coddoc = "CD".
END CASE.
IF x-Detalle = NO THEN DO:
    /* UN SOLO NUMERO DE DOCUMENTO (SIN DETALLE) */
    z-nrodoc = STRING(YEAR(Ccbcier.fchcie), '9999') +
                STRING(MONTH(Ccbcier.fchcie), '99') +
                STRING(DAY(ccbcier.fchcie), '99') +
                STRING(REPLACE(ccbcier.horcie,':',""),"9999").
    ASSIGN
        X-NRODOC = SUBSTRING(Ccbcdocu.Nrodoc,1,3) + z-nrodoc.
        X-CODCLI = '1111111111'.
END.
ELSE DO:
    x-codcli = (IF gn-clie.codant = '' THEN SUBSTRING(gn-clie.codcli,1,10) ELSE gn-clie.codant).
END.
IF Ccbcdocu.FlgEst = "A" THEN x-codcta = (IF Ccbcdocu.Coddoc = "FAC" THEN "12122100" ELSE "12121140").

FIND T-WMIGRV WHERE T-WMIGRV.wvtdoc = x-coddoc
    AND T-WMIGRV.wvndoc = x-nrodoc
    AND T-WMIGRV.wvmone = x-codmon
    AND T-WMIGRV.wvcpvt = x-codcta
    AND T-WMIGRV.wvfech = x-FchDoc
    NO-ERROR.
IF NOT AVAILABLE T-WMIGRV THEN CREATE T-WMIGRV.
ASSIGN
    T-WMIGRV.FlagTipo = "I"
    T-WMIGRV.FlagUsuario = s-user-id.
ASSIGN
    T-WMIGRV.wsecue = 0001
    T-WMIGRV.wvejer = YEAR(ccbcdocu.fchdoc)
    T-WMIGRV.wvperi = MONTH(ccbcdocu.fchdoc)
    T-WMIGRV.wvtdoc = x-CodDoc
    T-WMIGRV.wvndoc = x-NroDoc
    T-WMIGRV.wvfech = x-FchDoc
    T-WMIGRV.wvccli = x-codcli
    T-WMIGRV.wvclie = SUBSTRING (gn-clie.nomcli, 1, 40)
    T-WMIGRV.wvcdir = (IF gn-clie.dircli <> '' THEN SUBSTRING (gn-clie.dircli, 1, 40) ELSE 'LIMA')
    T-WMIGRV.wvcdis = (IF AVAILABLE TabDistr THEN SUBSTRING (TabDistr.NomDistr, 1, 40) ELSE 'LIMA')
    T-WMIGRV.wvref3 = (IF ccbcdocu.codcli BEGINS '2' THEN "PJ" ELSE "NI")
    T-WMIGRV.wvtido = (IF ccbcdocu.codcli BEGINS '2' THEN "RU" ELSE "")
    T-WMIGRV.wvnudo = (IF ccbcdocu.codcli BEGINS '2' THEN gn-clie.ruc ELSE SUBSTRING(gn-clie.codcli, 3, 8))
    T-WMIGRV.wvmone = x-codmon
    T-WMIGRV.wvtcam = 0.00.            /* ccbcdocu.tpocmb. */
IF Ccbcdocu.flgest = "A" THEN DO:       /* ANULADO */
    ASSIGN
        T-WMIGRV.wvfepr = YEAR(TODAY) * 10000 + MONTH(TODAY) * 100 + DAY(TODAY)
        T-WMIGRV.wvfeve = YEAR(x-fchvto) * 10000 + MONTH(x-fchvto) * 100 + DAY(x-fchvto)
        T-WMIGRV.wvruc = ccbcdocu.ruccli
        T-WMIGRV.wvndom = (IF T-WMIGRV.wvruc = '' THEN "N" ELSE (IF T-WMIGRV.wvref3 = "PJ" THEN "S" ELSE "N"))
        T-WMIGRV.wvcpag = "888"         /* ccbcdocu.fmapgo */
        T-WMIGRV.wvsitu = "99"        /* ANULADO */
        T-WMIGRV.wvcost = "9999999"
        T-WMIGRV.wvcven = ccbcdocu.codven
        T-WMIGRV.wvacti = ccbcdocu.coddiv
        T-WMIGRV.wvnbco = ''  /* ver ticketeras */
        T-WMIGRV.wvusin = STRING(ccbcdocu.usuario,"x(10)")
        T-WMIGRV.wvfein = YEAR(TODAY) * 10000 + MONTH(TODAY) * 100 + DAY(TODAY).
    RETURN.
END.
IF ccbcdocu.impbrt > 0 
    THEN ASSIGN
            T-WMIGRV.wvvalv = T-WMIGRV.wvvalv + CcbCDocu.ImpBrt
            T-WMIGRV.wvcval = cb-cfgg.codcta[5]
            T-WMIGRV.wvmval = (IF ccbcdocu.coddoc = "N/C" THEN "C" ELSE "A").
IF ccbcdocu.impexo > 0 
    THEN ASSIGN
            T-WMIGRV.wvvali = T-WMIGRV.wvvali + ccbcdocu.impexo
            T-WMIGRV.wvcvai = cb-cfgg.codcta[6]
            T-WMIGRV.wvmvai = ( IF ccbcdocu.coddoc = "N/C" THEN "C" ELSE "A" ).
IF ccbcdocu.impdto > 0 
    THEN ASSIGN
            T-WMIGRV.wvdsct = T-WMIGRV.wvdsct + ccbcdocu.impdto
            T-WMIGRV.wvcdsc = x-ctadto
            T-WMIGRV.wvmdsc = (IF ccbcdocu.coddoc = "N/C" THEN "A" ELSE "C").
IF ccbcdocu.impigv > 0 
    THEN ASSIGN
            T-WMIGRV.wvigv = T-WMIGRV.wvigv + ccbcdocu.impigv
            T-WMIGRV.wvcigv = x-ctaigv
            T-WMIGRV.wvmigv = (IF ccbcdocu.coddoc = "N/C" THEN "C" ELSE "A").
ASSIGN
    T-WMIGRV.wvpvta = T-WMIGRV.wvpvta + ccbcdocu.imptot
    T-WMIGRV.wvcpvt = x-codcta
    T-WMIGRV.wvmpvt = (IF ccbcdocu.coddoc = "N/C" THEN "A" ELSE "C").
IF ccbcdocu.coddoc = 'N/C' OR ccbcdocu.coddoc = 'N/D' THEN DO:
    CASE ccbcdocu.codref:
        WHEN 'FAC' THEN T-WMIGRV.wvtref = "FC".
        WHEN 'BOL' THEN T-WMIGRV.wvtref = "BV".
        WHEN 'TCK' THEN T-WMIGRV.wvtref = "TK".
        WHEN 'LET' THEN T-WMIGRV.wvtref = "LT".
        WHEN 'N/C' THEN T-WMIGRV.wvtref = "NC".
        WHEN 'N/D' THEN T-WMIGRV.wvtref = "ND".
        WHEN 'CHQ' THEN T-WMIGRV.wvtref = "CD".
    END CASE.
    ASSIGN
        T-WMIGRV.wvnref = ccbcdocu.nroref.
END.
ELSE DO:
    IF Ccbcdocu.codped = "PED" 
        THEN ASSIGN
                T-WMIGRV.wvtref = "PD"
                T-WMIGRV.wvnref = Ccbcdocu.nroped.
END.
ASSIGN
    T-WMIGRV.wvfepr = YEAR(TODAY) * 10000 + MONTH(TODAY) * 100 + DAY(TODAY)
    T-WMIGRV.wvfeve = YEAR(x-fchvto) * 10000 + MONTH(x-fchvto) * 100 + DAY(x-fchvto)
    T-WMIGRV.wvruc = ccbcdocu.ruccli
    T-WMIGRV.wvndom = (IF T-WMIGRV.wvruc = '' THEN "N" ELSE (IF T-WMIGRV.wvref3 = "PJ" THEN "S" ELSE "N"))
    T-WMIGRV.wvcpag = "888"         /*ccbcdocu.fmapgo*/
    T-WMIGRV.wvsitu = "02"        /* Graba en Cuentas por Cobrar */
    T-WMIGRV.wvcost = "9999999"
    T-WMIGRV.wvcven = ccbcdocu.codven
    T-WMIGRV.wvacti = ccbcdocu.coddiv
    T-WMIGRV.wvnbco = ''  /* ver ticketeras */
    T-WMIGRV.wvusin = string(ccbcdocu.usuario,"X(10)")
    T-WMIGRV.wvfein = YEAR(TODAY) * 10000 + MONTH(TODAY) * 100 + DAY(TODAY).
*/

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-proc_Graba-Caja) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE proc_Graba-Caja Procedure 
PROCEDURE proc_Graba-Caja :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

    DEFINE INPUT PARAMETER x-coddiv AS CHAR.
    DEFINE INPUT PARAMETER x-codmon AS INTEGER.
    DEFINE INPUT PARAMETER x-import AS DECIMAL.
    DEFINE INPUT PARAMETER x-clfaux AS CHAR.
    DEFINE INPUT PARAMETER x-codaux AS CHAR.
    DEFINE INPUT PARAMETER x-nrodoc AS CHAR.
    DEFINE INPUT PARAMETER x-fchvto AS DATE.
    DEFINE INPUT PARAMETER x-tpocmb AS DECIMAL.

    DEF VAR x-fchdoc AS INT.

    x-fchdoc = YEAR(ccbcierr.fchcie) * 10000 + MONTH(ccbcier.fchcie) * 100 + DAY(ccbcier.fchcie).
    CREATE T-WMIGMC.
    ASSIGN
        T-WMIGMC.FlagTipo = "I"
        T-WMIGMC.FlagUsuario = s-user-id.
    ASSIGN
        T-WMIGMC.wsecue = 0001
        T-WMIGMC.wmejer = YEAR(ccbcier.fchcie)
        T-WMIGMC.wmperi = MONTH(ccbcier.fchcie)
        T-WMIGMC.wtope  = "I"
        T-WMIGMC.wfpago = x-CodCbd
        T-WMIGMC.wndpag = x-NroDoc
        T-WMIGMC.wcdbco = '02'      /* ¿? */
        T-WMIGMC.wfcobr = x-FchDoc
        T-WMIGMC.wdescr = x-GloDoc
        T-WMIGMC.wmonor = (IF x-CodMon = 01 THEN 00 ELSE 01)
        T-WMIGMC.wimpor = x-Import
        T-WMIGMC.wtpcbo = 0
        T-WMIGMC.wtpcbe = 0
        T-WMIGMC.wimpmn = 0
        T-WMIGMC.wimpme = 0
        T-WMIGMC.wtpaux = x-ClfAux
        T-WMIGMC.wcdaux = x-CodAux
        T-WMIGMC.wtdapl = ''
        T-WMIGMC.wndapl = ''
        T-WMIGMC.wccost = ''
        T-WMIGMC.wactiv = x-CodDiv
        T-WMIGMC.wtpgas = ''.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-Transferir-Asiento) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Transferir-Asiento Procedure 
PROCEDURE Transferir-Asiento :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

/* BLOQUEAMOS Sí o Sí el correlativo */
REPEAT:
  FIND wmigcorr WHERE wmigcorr.Proceso = "RV"
      AND wmigcorr.Periodo = YEAR(Ccbcierr.fchcie)
      AND wmigcorr.Mes = MONTH(Ccbcierr.fchcie)
      EXCLUSIVE-LOCK NO-ERROR NO-WAIT.
  IF NOT AVAILABLE wmigcorr THEN DO:
      IF NOT LOCKED wmigcorr THEN DO:
          /* CREAMOS EL CONTROL */
          CREATE wmigcorr.
          ASSIGN
              wmigcorr.Correlativo = 1
              wmigcorr.Mes = MONTH(Ccbcierr.fchcie)
              wmigcorr.Periodo = YEAR(Ccbcierr.fchcie)
              wmigcorr.Proceso = "RV".
          LEAVE.
      END.
      ELSE UNDO, RETRY.
  END.
  LEAVE.
END.
FOR EACH T-WMIGMC NO-LOCK:
  CREATE WMIGMC.
  BUFFER-COPY T-WMIGMC
      TO WMIGMC
      ASSIGN
      wmigrv.FlagFecha = DATETIME(TODAY, MTIME)
      wmigrv.FlagTipo = "I"
      wmigrv.FlagUsuario = s-user-id
      wmigrv.wcorre = STRING(wmigcorr.Periodo, '9999') + STRING(wmigcorr.Mes, '99') + 
                      STRING(wmigcorr.Correlativo, '9999')
      wmigrv.wsecue = 0001
      wmigrv.wvejer = wmigcorr.Periodo
      wmigrv.wvperi = wmigcorr.Mes.
  ASSIGN
      wmigcorr.Correlativo = wmigcorr.Correlativo + 1.
END.
IF AVAILABLE(wmigcorr) THEN RELEASE wmigcorr.
IF AVAILABLE(wmigrv)   THEN RELEASE wmigrv.
EMPTY TEMP-TABLE T-WMIGMC.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF


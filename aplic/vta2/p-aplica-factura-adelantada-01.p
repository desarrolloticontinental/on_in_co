&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12
&ANALYZE-RESUME
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS Procedure 
/*------------------------------------------------------------------------
    File        : 
    Purpose     : Generar los movimientos por aplicaci�n de facturas adelantas
                  y actualizar el saldo por aplicar

    Syntax      :

    Description :

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
         HEIGHT             = 4.5
         WIDTH              = 60.
/* END WINDOW DEFINITION */
                                                                        */
&ANALYZE-RESUME

 


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK Procedure 


/* ***************************  Main Block  *************************** */

DEF INPUT PARAMETER pRowid AS ROWID.

DEF SHARED VAR s-codcia AS INT.
DEF SHARED VAR s-coddiv AS CHAR.
DEF SHARED VAR s-user-id AS CHAR.

DEF VAR x-Saldo-Actual  AS DEC NO-UNDO.
DEF VAR x-Saldo-Control AS DEC NO-UNDO.     /* SALDO PARA CALCULAR LOS DESCUENTOS */
DEF VAR x-Monto-Aplicar  AS DEC NO-UNDO.
DEF VAR x-TpoCmb-Compra AS DEC INIT 1 NO-UNDO.
DEF VAR x-TpoCmb-Venta  AS DEC INIT 1 NO-UNDO.
DEF VAR x-ImpMn AS DEC NO-UNDO.
DEF VAR x-ImpMe AS DEC NO-UNDO.

DEFINE BUFFER F-CDOCU FOR CcbCDocu.
DEFINE BUFFER B-CDOCU FOR CcbCDocu.

DEFINE VAR x-PorDtoAdelanto AS DEC NO-UNDO. 
DEFINE VAR x-ImpDtoAdelanto AS DEC NO-UNDO.
DEFINE VAR x-ImpDtoAplicado AS DEC NO-UNDO.
DEFINE VAR x-ItemAdelanto   AS INT NO-UNDO.

DEFINE VAR m-PorDcto_Adelanto LIKE CcbDDocu.PorDcto_Adelanto NO-UNDO.

DEFINE VAR k AS INT NO-UNDO.

PRINCIPAL:
DO TRANSACTION ON ERROR UNDO, RETURN "ADM-ERROR" ON STOP UNDO, RETURN "ADM-ERROR":
    FIND Ccbcdocu WHERE ROWID(Ccbcdocu) = pRowid EXCLUSIVE-LOCK NO-ERROR.
    IF NOT AVAILABLE Ccbcdocu THEN DO:
        MESSAGE 'NO se pudo aplicar los adelantos' VIEW-AS ALERT-BOX ERROR.
        RETURN "ADM-ERROR".
    END.
    FIND LAST Gn-tccja WHERE Gn-tccja.Fecha <= TODAY NO-LOCK NO-ERROR.
    IF AVAILABLE Gn-TcCja 
        THEN ASSIGN
                x-TpoCmb-Compra = Gn-Tccja.Compra.
                x-TpoCmb-Venta  = Gn-Tccja.Venta.
    /* SE SUPONE QUE LA FACTURA ES NUEVA */
    ASSIGN
        Ccbcdocu.ImpTot2 = 0                /* limpiamos el acumulador */
        x-Monto-Aplicar = 0
        x-Saldo-Actual = Ccbcdocu.ImpTot
        x-Saldo-Control = Ccbcdocu.ImpTot
        m-PorDcto_Adelanto = 0.     /* MATRIZ DE CONTROL 3.5|1.5|1.0 */
    /* Barremos todos los A/C por aplicar */
    FOR EACH  F-CDOCU USE-INDEX Llave06 WHERE F-CDOCU.codcia = CcbCDocu.codcia
        AND F-CDOCU.codcli = CcbCDocu.codcli
        AND F-CDOCU.flgest = 'P'
        AND F-CDOCU.coddoc = 'A/C'
        AND F-CDOCU.AcuBon[1] > 0:      /* % de Descuento */
        /* *************************************** */
        /* 1ro. APLICAMOS EL DESCUENTO A CADA ITEM */
        /* *************************************** */
        ASSIGN
            x-PorDtoAdelanto = 0
            x-ItemAdelanto = 0.     /* Posici�n del %Dcto en la matriz */
        ADELANTO:
        DO k = 1 TO 5:
            IF m-PorDcto_Adelanto[k] = 0 OR m-PorDcto_Adelanto[k] = F-CDOCU.AcuBon[1] THEN DO:
                ASSIGN
                    m-PorDcto_Adelanto[k] = F-CDOCU.AcuBon[1]    /* % de Dscto */
                    x-PorDtoAdelanto = F-CDOCU.AcuBon[1]
                    x-ItemAdelanto = k.
                LEAVE ADELANTO.
            END.
        END.
        IF x-ItemAdelanto = 0 THEN NEXT.

        /* Determinamos el monto m�ximo a descontar */
        IF CcbCDocu.codmon = F-CDOCU.codmon
        THEN x-ImpDtoAdelanto = ROUND(MINIMUM(F-CDOCU.SdoAct, x-Saldo-Control) * x-PorDtoAdelanto / 100, 2).
        ELSE IF CcbCDocu.codmon = 1
            THEN x-ImpDtoAdelanto = ROUND(MINIMUM(F-CDOCU.SdoAct, x-Saldo-Control) * x-TpoCmb-Compra * x-PorDtoAdelanto / 100, 2).
            ELSE x-ImpDtoAdelanto = ROUND(MINIMUM(F-CDOCU.SdoAct, x-Saldo-Control) / x-TpoCmb-Venta  * x-PorDtoAdelanto / 100, 2).
        x-ImpDtoAdelanto = MINIMUM(x-Saldo-Control, x-ImpDtoAdelanto).   /*  TOMAMOS EL MENOR MONTO */
        /* REPARTIMOS POR CADA ITEM */
        x-ImpDtoAplicado = 0.
        FOR EACH Ccbddocu OF Ccbcdocu:
            ASSIGN
                x-ImpDtoAplicado = x-ImpDtoAplicado + ROUND(Ccbddocu.ImpLin / Ccbcdocu.ImpTot * x-ImpDtoAdelanto, 2)
                Ccbddocu.PorDcto_Adelanto[x-ItemAdelanto] = x-PorDtoAdelanto
                Ccbddocu.ImpDcto_Adelanto[x-ItemAdelanto] = Ccbddocu.ImpDcto_Adelanto[x-ItemAdelanto] + 
                                            ROUND(Ccbddocu.ImpLin / Ccbcdocu.ImpTot * x-ImpDtoAdelanto, 2).
            /* SI SOBREPASA EN CENTIMOS LO REDONDEAMOS */
            IF x-ImpDtoAplicado > x-ImpDtoAdelanto THEN DO:
                ASSIGN
                    Ccbddocu.ImpDcto_Adelanto[x-ItemAdelanto] = Ccbddocu.ImpDcto_Adelanto[x-ItemAdelanto] -
                                            ABSOLUTE(x-ImpDtoAplicado - x-ImpDtoAdelanto)
                    x-ImpDtoAplicado = x-ImpDtoAdelanto.
            END.
        END.
        /* DISMINUIMOS EL SALDO DEL DOCUMENTO CON LOS DESCUENTOS APLICADOS */
        ASSIGN
            x-Saldo-Actual = x-Saldo-Actual - x-ImpDtoAplicado.     /* x-ImpDtoAdelanto. */
        IF x-Saldo-Actual <= 0 THEN NEXT.       /* En caso los descuentos cubrar el total (�?) */
        /* ************************************************ */
        /* 2DO APLICAMOS EL ADELANTO AL SALDO DE LA FACTURA */
        /* ************************************************ */
        IF CcbCDocu.codmon = F-CDOCU.codmon THEN x-Monto-Aplicar  = MINIMUM(x-Saldo-Actual, F-CDOCU.SdoAct).
        ELSE IF CcbCDocu.codmon = 1 
            THEN x-Monto-Aplicar  = ROUND(MINIMUM(F-CDOCU.SdoAct * x-TpoCmb-Compra, x-Saldo-Actual),2).
            ELSE x-Monto-Aplicar  = ROUND(MINIMUM(F-CDOCU.SdoAct / x-TpoCmb-Venta , x-Saldo-Actual),2).
        /* Guardamos el monto aplicado a la Factura */
        ASSIGN
            Ccbcdocu.ImpTot2 = Ccbcdocu.ImpTot2 + x-Monto-Aplicar     /* ACUMULAMOS */
            x-Saldo-Actual = x-Saldo-Actual - x-Monto-Aplicar
            x-Saldo-Control = x-Saldo-Control - x-Monto-Aplicar.
        /* Control de descarga de facturas adelantadas */
        CREATE Ccbdcaja.
        ASSIGN
          CcbDCaja.CodCia = Ccbcdocu.codcia
          CcbDCaja.CodCli = Ccbcdocu.codcli
          CcbDCaja.CodDiv = Ccbcdocu.coddiv
          CcbDCaja.CodDoc = F-CDOCU.CodDoc
          CcbDCaja.NroDoc = F-CDOCU.NroDoc
          CcbDCaja.CodMon = Ccbcdocu.codmon
          CcbDCaja.CodRef = Ccbcdocu.coddoc
          CcbDCaja.NroRef = Ccbcdocu.nrodoc
          CcbDCaja.FchDoc = TODAY
          CcbDCaja.ImpTot = x-Monto-Aplicar
          CcbDCaja.TpoCmb = (IF CcbCDocu.codmon = 1 THEN x-TpoCmb-Compra ELSE x-TpoCmb-Venta).
        IF CcbDCaja.CodMon = 1 
            THEN ASSIGN
            x-ImpMn = CcbDCaja.ImpTot
            x-ImpMe = CcbDCaja.ImpTot / Ccbdcaja.TpoCmb.
        ELSE ASSIGN
            x-ImpMn = CcbDCaja.ImpTot * Ccbdcaja.TpoCmb
            x-ImpMe = CcbDCaja.ImpTot.
        RUN proc_AplicaDoc(
              F-CDOCU.CodDoc,         /* A/C */
              F-CDOCU.NroDoc,
              CcbDCaja.tpocmb,
              x-ImpMn,
              x-impMe
              ).
        IF RETURN-VALUE = 'ADM-ERROR' THEN UNDO, RETURN 'ADM-ERROR'.
        IF x-Saldo-Actual <= 0 THEN LEAVE.
    END.
    /* CONSISTENCIA FINAL: PUEDE QUE SUPERE EN CENTIMOS */
    IF Ccbcdocu.ImpTot2 > Ccbcdocu.ImpTot THEN Ccbcdocu.ImpTot2 = Ccbcdocu.ImpTot.
END.

RELEASE F-CDOCU.
RELEASE Ccbdcaja.

RETURN "OK".

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&IF DEFINED(EXCLUDE-proc_AplicaDoc) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE proc_AplicaDoc Procedure 
PROCEDURE proc_AplicaDoc :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

    DEFINE INPUT PARAMETER para_CodDoc LIKE CcbCDocu.CodDoc.
    DEFINE INPUT PARAMETER para_NroDoc LIKE CcbDMov.NroDoc.
    DEFINE INPUT PARAMETER para_TpoCmb LIKE CCBDMOV.TpoCmb.
    DEFINE INPUT PARAMETER para_ImpNac LIKE CcbDMov.ImpTot.
    DEFINE INPUT PARAMETER para_ImpUSA LIKE CcbDMov.ImpTot.

    DEFINE BUFFER B-CDocu FOR CcbCDocu.

    /* Tipo de Documento */
    FIND FacDoc WHERE
        FacDoc.CodCia = s-CodCia AND
        FacDoc.CodDoc = para_CodDoc
        NO-LOCK NO-ERROR.
    IF NOT AVAILABLE FacDoc THEN DO:
        MESSAGE
            para_CodDoc 'NO CONFIGURADO'
            VIEW-AS ALERT-BOX ERROR.
        RETURN "ADM-ERROR".
    END.

    DO TRANSACTION ON ERROR UNDO, RETURN 'ADM-ERROR' ON STOP UNDO, RETURN 'ADM-ERROR':
        /* Busca Documento */
        FIND FIRST B-CDocu WHERE
            B-CDocu.CodCia = s-codcia AND
            B-CDocu.CodDoc = para_CodDoc AND
            B-CDocu.NroDoc = para_NroDoc
            EXCLUSIVE-LOCK NO-ERROR.
        IF NOT AVAILABLE B-CDocu THEN DO:
            MESSAGE
                "DOCUMENTO" para_CodDoc para_NroDoc "NO REGISTRADO"
                VIEW-AS ALERT-BOX ERROR.
            RETURN "ADM-ERROR".
        END.
        /* Crea Detalle de la Aplicaci�n */
        CREATE CCBDMOV.
        ASSIGN
            CCBDMOV.CodCia = s-CodCia
            CCBDMOV.CodDiv = s-CodDiv
            CCBDMOV.CodDoc = B-CDocu.CodDoc
            CCBDMOV.NroDoc = B-CDocu.NroDoc
            CCBDMOV.CodMon = B-CDocu.CodMon
            CCBDMOV.CodRef = CcbCDocu.CodDoc        /* LA FACTURA ALA CUAL SE APLICA */
            CCBDMOV.NroRef = Ccbcdocu.NroDoc        /* EL ADELANTO */
            CCBDMOV.CodCli = B-CDocu.CodCli
            CCBDMOV.FchDoc = B-CDocu.FchDoc
            CCBDMOV.HraMov = STRING(TIME,"HH:MM:SS")
            CCBDMOV.TpoCmb = para_tpocmb
            CCBDMOV.usuario = s-User-ID.
        IF B-CDocu.CodMon = 1 THEN
            ASSIGN CCBDMOV.ImpTot = para_ImpNac.
        ELSE ASSIGN CCBDMOV.ImpTot = para_ImpUSA.
        IF FacDoc.TpoDoc THEN
            ASSIGN B-CDocu.SdoAct = B-CDocu.SdoAct + CCBDMOV.ImpTot.
        ELSE
            ASSIGN B-CDocu.SdoAct = B-CDocu.SdoAct - CCBDMOV.ImpTot.
        /* Cancela Documento */
        IF B-CDocu.SdoAct = 0 THEN
            ASSIGN 
                B-CDocu.FlgEst = "C"
                B-CDocu.FchCan = TODAY.
        ELSE
            ASSIGN
                B-CDocu.FlgEst = "P"
                B-CDocu.FchCan = ?.

        RELEASE B-CDocu.
        RELEASE Ccbdmov.
    END. /* DO TRANSACTION... */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF


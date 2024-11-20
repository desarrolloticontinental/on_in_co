&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12
&ANALYZE-RESUME
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS Procedure 
/*------------------------------------------------------------------------
    File        : 
    Purpose     :

    Syntax      :

    Description :

    Author(s)   :
    Created     :
    Notes       :
  ----------------------------------------------------------------------*/
/*          This .W file was created with the Progress AppBuilder.      */
/*----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

DISABLE TRIGGERS FOR LOAD OF gn-cliem.

DEF VAR s-codcia AS INT INIT 001.
DEF VAR cl-codcia AS INT INIT 000.
DEF VAR x-Deudas AS DEC FORMAT '->>>,>>>,>>9.99' EXTENT 3.
DEF VAR x-Importe  AS DEC NO-UNDO.
DEF VAR x-Aplicado AS DEC NO-UNDO.
DEF TEMP-TABLE Detalle
    FIELD codcia LIKE ccbcdocu.codcia
    FIELD codcli LIKE ccbcdocu.codcli
    FIELD nomcli LIKE ccbcdocu.nomcli
    FIELD sdoact AS DEC FORMAT '>>>,>>>,>>9.99' EXTENT 3
    INDEX Llave01 IS PRIMARY UNIQUE codcia codcli.

/* documentos de cargo */
FOR EACH ccbcdocu NO-LOCK WHERE codcia = s-codcia AND flgest = 'P' 
    AND fchvto < TODAY AND NOT Ccbcdocu.codcli BEGINS '11111111',
    FIRST facdocum OF ccbcdocu NO-LOCK WHERE facdocum.tpodoc = YES:
    FIND Detalle OF Ccbcdocu EXCLUSIVE-LOCK NO-ERROR.
    IF NOT AVAILABLE Detalle THEN DO:
        CREATE Detalle.
        ASSIGN
            Detalle.codcia = Ccbcdocu.codcia
            Detalle.codcli = Ccbcdocu.codcli
            Detalle.nomcli = Ccbcdocu.nomcli.
    END.
    x-Importe = Ccbcdocu.sdoact.
    IF Ccbcdocu.codmon = 2 THEN x-Importe = x-Importe * Ccbcdocu.tpocmb.
    IF (TODAY - Ccbcdocu.FchVto) < 15 THEN Detalle.SdoAct[1] = Detalle.SdoAct[1] + x-Importe.
    IF (TODAY - Ccbcdocu.FchVto) >= 15 AND (TODAY - Ccbcdocu.FchVto) < 30 THEN Detalle.SdoAct[2] = Detalle.SdoAct[2] + x-Importe.
    IF (TODAY - Ccbcdocu.FchVto) >= 30 THEN Detalle.SdoAct[3] = Detalle.SdoAct[3] + x-Importe.
END.

/* documentos de abono */
FOR EACH ccbcdocu NO-LOCK WHERE codcia = s-codcia AND flgest = 'P',
    FIRST facdocum OF ccbcdocu NO-LOCK WHERE facdocum.tpodoc = NO:
    FIND Detalle WHERE Detalle.codcia = Ccbcdocu.codcia
        AND Detalle.codcli = Ccbcdocu.codcli EXCLUSIVE-LOCK NO-ERROR.
    IF NOT AVAILABLE Detalle THEN NEXT.
    x-Importe = Ccbcdocu.sdoact.
    IF Ccbcdocu.codmon = 2 THEN x-Importe = x-Importe * Ccbcdocu.tpocmb.
    IF Detalle.SdoAct[3] > 0 THEN DO:
        x-Aplicado = MINIMUM(Detalle.SdoAct[3], x-Importe).
        Detalle.SdoAct[3] = Detalle.SdoAct[3] - x-Aplicado.
        x-Importe = x-Importe - x-Aplicado.
    END.
    IF Detalle.SdoAct[2] > 0 THEN DO:
        x-Aplicado = MINIMUM(Detalle.SdoAct[2], x-Importe).
        Detalle.SdoAct[2] = Detalle.SdoAct[2] - x-Aplicado.
        x-Importe = x-Importe - x-Aplicado.
    END.
    IF Detalle.SdoAct[1] > 0 THEN DO:
        x-Aplicado = MINIMUM(Detalle.SdoAct[1], x-Importe).
        Detalle.SdoAct[1] = Detalle.SdoAct[1] - x-Aplicado.
    END.
END.

/* Mensaje final */
FOR EACH gn-cliem WHERE  gn-cliem.codcia = cl-codcia AND gn-cliem.Origen = 'AUT':
    DELETE gn-cliem.
END.

FOR EACH Detalle NO-LOCK WHERE Detalle.SdoAct[2] > 0 OR Detalle.SdoAct[3] > 0:
    CREATE gn-cliem.
    ASSIGN
        gn-cliem.CodCia = cl-codcia
        gn-cliem.CodCli = Detalle.codcli
        gn-cliem.FchIni = TODAY
        gn-cliem.FchVto = TODAY
        gn-cliem.Fecha  = TODAY
        gn-cliem.Hora   = STRING(TIME, 'HH:MM:SS')
        gn-cliem.Origen = 'AUT'
        gn-cliem.NroMsg = NEXT-VALUE ( Mensaje_a_clientes ).
    IF Detalle.SdoAct[2] > 0 THEN DO:
        ASSIGN
            gn-cliem.ImpMn   = Detalle.SdoAct[2]
            gn-cliem.Mensaje = 'DEUDA PENDIENTE MENOR A 30 DIAS DE S/.' + TRIM ( STRING(Detalle.SdoAct[2], '>>>,>>>,>>9.99') ).
            gn-cliem.Tipo    = 'N'.
    END.
    IF Detalle.SdoAct[3] > 0 THEN DO:
        ASSIGN
            gn-cliem.ImpMn   = Detalle.SdoAct[3]
            gn-cliem.Mensaje = 'DEUDA PENDIENTE MAYOR A 30 DIAS DE S/.' + TRIM ( STRING(Detalle.SdoAct[3], '>>>,>>>,>>9.99') ).
            gn-cliem.Tipo    = 'S'.
    END.
    DISPLAY
        gn-cliem.codcli
        gn-cliem.fecha
        gn-cliem.hora
        gn-cliem.nromsg
        gn-cliem.mensaje
        WITH STREAM-IO NO-BOX WIDTH 200.
    PAUSE 0.
END.

QUIT.

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
         HEIGHT             = 4.38
         WIDTH              = 60.
/* END WINDOW DEFINITION */
                                                                        */
&ANALYZE-RESUME

 


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK Procedure 


/* ***************************  Main Block  *************************** */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME



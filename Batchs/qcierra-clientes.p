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

/* 09/12/2019
Sistemas debe desarrollar una funcionalidad para automatizar la desactivación en el maestro a los clientes que no tienen movimiento comercial (Solamente ventas), después de 18 meses, con la finalidad de realizar el mantenimiento.

-          La funcionalidad no debe afectar a las cobranzas

-          Daniel Llicán comunicará cuando se encuentre puesta en marcha esta funcionalidad.


*/
DEF VAR cl-codcia AS INT INIT 000.
DEF VAR s-codcia AS INT INIT 001.
DEF VAR s-user-id AS CHAR INIT 'SYSTEM' NO-UNDO.

DISABLE TRIGGERS FOR LOAD OF gn-clie.
DEF BUFFER b-gn-clie FOR gn-clie.

DEF VAR x-FchDoc AS DATE NO-UNDO.
DEF VAR x-Meses AS INT INIT 18 NO-UNDO.
DEF VAR x-FechaLimite AS DATE NO-UNDO.

x-FechaLimite = DATE( ADD-INTERVAL (NOW, (-1 * x-Meses), 'months' ) ).

FIND TabGener WHERE TabGener.CodCia = s-CodCia AND
    TabGener.Clave = "CIERRE" AND
    TabGener.Codigo = "CLIENTES"
    NO-LOCK NO-ERROR.
IF AVAILABLE TabGener AND TabGener.Libre_d01 > 0 THEN x-Meses = TabGener.Libre_d01.

/* Solo clientes con mas de 18 meses de antiguedad */
PUT UNFORMATTE 'INICIO: ' NOW SKIP.
FOR EACH gn-clie NO-LOCK WHERE gn-clie.codcia = cl-codcia AND 
    gn-clie.flgsit = "A" AND
    gn-clie.fching <> ? AND
    gn-clie.fching <= x-FechaLimite:
    IF gn-clie.codcli BEGINS '1111111111' THEN NEXT.
    /* *********************************************** */
    /* Buscamos si tiene deuda pendiente */
    FIND FIRST Ccbcdocu WHERE Ccbcdocu.codcia = s-CodCia AND
        Ccbcdocu.codcli = gn-clie.codcli AND
        Ccbcdocu.flgest = 'P' AND
        LOOKUP(Ccbcdocu.coddoc, 'FAC,BOL,N/C,N/D,LET,CHQ,DOC,A/C,A/R,BD') > 0
        NO-LOCK NO-ERROR.
    IF AVAILABLE Ccbcdocu THEN NEXT.
    /* Buscamos últimos movimientos comerciales */
    x-FchDoc = ?.
    FOR EACH gn-divi NO-LOCK WHERE gn-divi.codcia = s-codcia,
        EACH Faccpedi NO-LOCK WHERE Faccpedi.codcia = s-codcia AND
        faccpedi.coddiv = gn-divi.coddiv AND
        faccpedi.coddoc = 'COT' AND
        faccpedi.flgest = 'P' AND 
        faccpedi.codcli = gn-clie.codcli:
        IF x-FchDoc = ? THEN x-FchDoc = faccpedi.fchped.
        ELSE x-FchDoc = MAXIMUM(x-FchDoc, faccpedi.fchped).
    END.
    IF x-FchDoc <> ? AND x-FchDoc > x-FechaLimite THEN NEXT.
    FOR EACH gn-divi NO-LOCK WHERE gn-divi.codcia = s-codcia,
        EACH Faccpedi NO-LOCK WHERE Faccpedi.codcia = s-codcia AND
        faccpedi.coddiv = gn-divi.coddiv AND
        faccpedi.coddoc = 'PCO' AND
        faccpedi.flgest = 'P' AND 
        faccpedi.codcli = gn-clie.codcli:
        IF x-FchDoc = ? THEN x-FchDoc = faccpedi.fchped.
        ELSE x-FchDoc = MAXIMUM(x-FchDoc, faccpedi.fchped).
    END.
    IF x-FchDoc <> ? AND x-FchDoc > x-FechaLimite THEN NEXT.
    FOR EACH gn-divi NO-LOCK WHERE gn-divi.codcia = s-codcia,
        EACH Faccpedi NO-LOCK WHERE Faccpedi.codcia = s-codcia AND
        faccpedi.coddiv = gn-divi.coddiv AND
        faccpedi.coddoc = 'PED' AND
        faccpedi.flgest = 'P' AND 
        faccpedi.codcli = gn-clie.codcli:
        IF x-FchDoc = ? THEN x-FchDoc = faccpedi.fchped.
        ELSE x-FchDoc = MAXIMUM(x-FchDoc, faccpedi.fchped).
    END.
    IF x-FchDoc <> ? AND x-FchDoc > x-FechaLimite THEN NEXT.
    FOR EACH gn-divi NO-LOCK WHERE gn-divi.codcia = s-codcia,
        EACH Faccpedi NO-LOCK WHERE Faccpedi.codcia = s-codcia AND
        faccpedi.coddiv = gn-divi.coddiv AND
        faccpedi.coddoc = 'O/D' AND
        faccpedi.flgest = 'P' AND 
        faccpedi.codcli = gn-clie.codcli:
        IF x-FchDoc = ? THEN x-FchDoc = faccpedi.fchped.
        ELSE x-FchDoc = MAXIMUM(x-FchDoc, faccpedi.fchped).
    END.
    IF x-FchDoc <> ? AND x-FchDoc > x-FechaLimite THEN NEXT.
    /* Buscamos ultima venta */
    x-FchDoc = ?.
    FOR EACH ccbcdocu NO-LOCK WHERE ccbcdocu.codcia = s-codcia AND
        ccbcdocu.codcli = gn-clie.codcli AND
        LOOKUP(ccbcdocu.coddoc , 'FAC,BOL') > 0
        BY ccbcdocu.fchdoc DESC:
        IF x-FchDoc = ? THEN x-FchDoc = ccbcdocu.fchdoc.
        ELSE x-FchDoc = MAXIMUM(x-FchDoc, ccbcdocu.fchdoc).
        LEAVE.
    END.
    /*IF x-FchDoc = ? THEN NEXT.*/
    IF x-FchDoc <> ? AND x-FchDoc > x-FechaLimite THEN NEXT.
    /* *********************************************** */
    {lib/lock-genericov3.i ~
        &Tabla="b-gn-clie" ~
        &Condicion="ROWID(b-gn-clie) = ROWID(gn-clie)" ~
        &Bloqueo="EXCLUSIVE-LOCK NO-ERROR NO-WAIT" ~
        &Accion="RETRY" ~
        &Mensaje="NO" ~
        &TipoError="UNDO, NEXT"}
    ASSIGN
        b-gn-clie.FlgSit = "C"
        b-gn-clie.FchCes = TODAY.
    ASSIGN
        b-gn-clie.FlagFecha = STRING(NOW)
        b-gn-clie.FlagMigracion = "N"
        b-gn-clie.FlagTipo      = "U"
        b-gn-clie.FlagUsuario   = s-user-id.
    RUN Replicar.
    CREATE LogTabla.
    ASSIGN
        logtabla.codcia = s-CodCia 
        logtabla.Dia = TODAY
        logtabla.Evento = 'DESACTIVADO'
        logtabla.Hora = STRING(TIME, 'HH:MM:SS')
        logtabla.Tabla = "gn-clie"
        logtabla.Usuario = s-user-id
        logtabla.ValorLlave = STRING(gn-clie.codcia) + '|' + gn-clie.codcli.
    PUT 'CLIENTE: ' gn-clie.codcli ' ' NOW SKIP.
END.
PUT 'FIN: ' NOW SKIP.
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
         HEIGHT             = 4.88
         WIDTH              = 60.
/* END WINDOW DEFINITION */
                                                                        */
&ANALYZE-RESUME

 


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK Procedure 


/* ***************************  Main Block  *************************** */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&IF DEFINED(EXCLUDE-Replicar) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Replicar Procedure 
PROCEDURE Replicar :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

    {rpl/reptrig.i
    &Table  = b-gn-clie
    &Key    =  "string(b-gn-clie.codcia,'999') + string(b-gn-clie.codcli, 'x(11)')"
    &Prg    = r-gn-clie
    &Event  = WRITE
    &FlgDb0 = NO    /* Replicar sede remota a la base principal */
    &FlgDB1 = NO    /* Replicar a la División 00501 (Plaza Norte) */
    &FlgDB2 = NO    /* Replicar a la División 00023 (Surquillo) */
    &FlgDB3 = NO
    &FlgDB4 = NO
    &FlgDB5 = NO
    &FlgDB6 = NO
    &FlgDB7 = NO
    &FlgDB8 = NO    /* La Rambla 00507 */
    &FlgDB9 = NO    /* San Isidro 00508 */
    &FlgDB10 = NO   /* 00065 */
    &FlgDB11 = NO   /* Atocongo 00510 */
    &FlgDB12 = NO   /* Angamos 00511 */
    &FlgDB13 = NO   /* Salaverry 00512 */
    &FlgDB14 = NO   /* Centro Civico 00513 */
    &FlgDB15 = NO   /* Primavera 00514 */
    &FlgDB16 = NO   /* Bellavista 00516 */
    &FlgDB17 = NO
    &FlgDB18 = NO   /* AREQUIPA */
    &FlgDB19 = NO       /* EXPOLIBRERIA */
    &FlgDB20 = NO   /* SERVIDOR CENTRAL */
    &FlgDB30 = NO   /* SERVIDOR ATE */
    }

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF


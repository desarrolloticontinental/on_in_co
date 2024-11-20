&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12
&ANALYZE-RESUME
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS Method-Library 
/*--------------------------------------------------------------------------
    Library     : 
    Purpose     :

    Syntax      :

    Description :

    Author(s)   :
    Created     :
    Notes       :
  ------------------------------------------------------------------------*/
/*          This .W file was created with the Progress UIB.             */
/*----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */



/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: Method-Library
   Allow: 
   Frames: 0
   Add Fields to: Neither
   Other Settings: INCLUDE-ONLY
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
/* DESIGN Window definition (used by the UIB) 
  CREATE WINDOW Method-Library ASSIGN
         HEIGHT             = 2
         WIDTH              = 40.
/* END WINDOW DEFINITION */
                                                                        */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _INCLUDED-LIB Method-Library 
/* ************************* Included-Libraries *********************** */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


 


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK Method-Library 


/* ***************************  Main Block  *************************** */

    if ccbcdocu.flgest = 'a' then next.
    find detalle where detalle.codmat = ccbddocu.codmat
        exclusive-lock no-error.
    if not available detalle then do:
        create detalle.
        buffer-copy almmmatg to detalle.
    end.
    
    if ccbcdocu.fchdoc >= 12/17/07 and ccbcdocu.fchdoc <= 12/23/07 then x-semana = 1.
    if ccbcdocu.fchdoc >= 12/24/07 and ccbcdocu.fchdoc <= 12/30/07 then x-semana = 2.
    if ccbcdocu.fchdoc >= 12/31/07 and ccbcdocu.fchdoc <= 01/06/07 then x-semana = 3.
    if ccbcdocu.fchdoc >= 01/07/08 and ccbcdocu.fchdoc <= 01/13/08 then x-semana = 4.
    if ccbcdocu.fchdoc >= 01/14/08 and ccbcdocu.fchdoc <= 01/20/08 then x-semana = 5.
    if ccbcdocu.fchdoc >= 01/21/08 and ccbcdocu.fchdoc <= 01/27/08 then x-semana = 6.
    if ccbcdocu.fchdoc >= 01/28/08 and ccbcdocu.fchdoc <= 02/03/08 then x-semana = 7.
    if ccbcdocu.fchdoc >= 02/04/08 and ccbcdocu.fchdoc <= 02/10/08 then x-semana = 8.
    if ccbcdocu.fchdoc >= 02/11/08 and ccbcdocu.fchdoc <= 02/17/08 then x-semana = 9.
    if ccbcdocu.fchdoc >= 02/18/08 and ccbcdocu.fchdoc <= 02/24/08 then x-semana = 10.
    if ccbcdocu.fchdoc >= 02/25/08 and ccbcdocu.fchdoc <= 03/02/08 then x-semana = 11.
    if ccbcdocu.fchdoc >= 03/03/08 and ccbcdocu.fchdoc <= 03/09/08 then x-semana = 12.
    if ccbcdocu.fchdoc >= 03/10/08 and ccbcdocu.fchdoc <= 03/16/08 then x-semana = 13.
    if ccbcdocu.fchdoc >= 03/17/08 and ccbcdocu.fchdoc <= 03/23/08 then x-semana = 14.
    if ccbcdocu.fchdoc >= 03/24/08 and ccbcdocu.fchdoc <= 03/30/08 then x-semana = 15.
    if ccbcdocu.fchdoc >= 03/31/08 and ccbcdocu.fchdoc <= 04/06/08 then x-semana = 16.
    
    x-cantidad = (ccbddocu.candes * ccbddocu.factor).
    IF ccbcdocu.coddoc = 'N/C'
    THEN detalle.vtasem[x-semana] = detalle.vtasem[x-semana] - x-cantidad.
    ELSE detalle.vtasem[x-semana] = detalle.vtasem[x-semana] + x-cantidad.
    find last gn-tcmb where gn-tcmb.fecha <= ccbcdocu.fchdoc no-lock.
    if ccbcdocu.codmon = 1
    then x-importe = ccbddocu.implin.
    else x-importe = ccbddocu.implin * gn-tcmb.venta.
    IF ccbcdocu.coddoc = 'N/C'
    THEN detalle.impsem[x-semana] = detalle.impsem[x-semana] - x-importe.
    ELSE detalle.impsem[x-semana] = detalle.impsem[x-semana] + x-importe.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME



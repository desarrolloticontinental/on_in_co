&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12
&ANALYZE-RESUME
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS Include 
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

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */



/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: Include
   Allow: 
   Frames: 0
   Add Fields to: Neither
   Other Settings: INCLUDE-ONLY
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
/* DESIGN Window definition (used by the UIB) 
  CREATE WINDOW Include ASSIGN
         HEIGHT             = 15
         WIDTH              = 60.
/* END WINDOW DEFINITION */
                                                                        */
&ANALYZE-RESUME

 


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK Include 


/* ***************************  Main Block  *************************** */


ASSIGN
    {&Tabla}.CanxMes   = {&Tabla}.CanxMes   + estavtas.EvtAll01.CanxMes
    {&Tabla}.VtaxMesMe = {&Tabla}.VtaxMesMe + estavtas.EvtAll01.VtaxMesMe 
    {&Tabla}.VtaxMesMn = {&Tabla}.VtaxMesMn + estavtas.EvtAll01.VtaxMesMn
    {&Tabla}.CtoxMesMe = {&Tabla}.CtoxMesMe + estavtas.EvtAll01.CtoxMesMe
    {&Tabla}.CtoxMesMn = {&Tabla}.CtoxMesMn + estavtas.EvtAll01.CtoxMesMn
    {&Tabla}.ProxMesMe = {&Tabla}.ProxMesMe + estavtas.EvtAll01.ProxMesMe
    {&Tabla}.ProxMesMn = {&Tabla}.ProxMesMn + estavtas.EvtAll01.ProxMesMn.                  
DO I = 1 TO 31 :
    {&Tabla}.CanxDia[I]   = {&Tabla}.CanxDia[I]   + estavtas.EvtAll01.CanxDia[I].
    {&Tabla}.VtaxDiaMn[I] = {&Tabla}.VtaxDiaMn[I] + estavtas.EvtAll01.VtaxDiaMn[I].
    {&Tabla}.VtaxDiaMe[I] = {&Tabla}.VtaxDiaMe[I] + estavtas.EvtAll01.VtaxDiaMe[I].
    {&Tabla}.CtoxDiaMn[I] = {&Tabla}.CtoxDiaMn[I] + estavtas.EvtAll01.CtoxDiaMn[I].
    {&Tabla}.CtoxDiaMe[I] = {&Tabla}.CtoxDiaMe[I] + estavtas.EvtAll01.CtoxDiaMe[I].
    {&Tabla}.ProxDiaMn[I] = {&Tabla}.ProxDiaMn[I] + estavtas.EvtAll01.ProxDiaMn[I].
    {&Tabla}.ProxDiaMe[I] = {&Tabla}.ProxDiaMe[I] + estavtas.EvtAll01.ProxDiaMe[I].
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME



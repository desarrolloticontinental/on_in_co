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

    FOR EACH Pl-Bole NO-LOCK WHERE
        Pl-Bole.CodPln = Pl-Calc.CodPln AND
        Pl-Bole.CodCal = Pl-Calc.CodCal AND
        (Pl-Bole.TpoBol = "Remuneraciones" OR
        Pl-Bole.TpoBol = "Descuentos" OR
        Pl-Bole.TpoBol = "Aportes") ,
        FIRST Pl-Conc OF Pl-Bole NO-LOCK:
        FOR EACH Pl-Mov-Mes NO-LOCK WHERE
           Pl-Mov-Mes.CodCia = s-codcia AND
           Pl-Mov-Mes.Periodo = FILL-IN-Periodo AND
           Pl-Mov-Mes.NroMes >= 1 AND
           Pl-Mov-Mes.NroMes <= 12 AND
           Pl-Mov-Mes.CodPln = Pl-Bole.CodPln AND
           Pl-Mov-Mes.CodCal = Pl-Bole.CodCal AND
           Pl-Mov-Mes.CodMov = Pl-Bole.CodMov:
        
           FIND Detalle WHERE
                Detalle.CodCia = s-CodCia AND
                Detalle.TpoBol = Pl-Bole.TpoBol AND
                Detalle.CodMov = Pl-Bole.CodMov 
           EXCLUSIVE-LOCK NO-ERROR.
              
           IF NOT AVAILABLE Detalle THEN DO:
               CREATE Detalle.
               ASSIGN 
                   Detalle.CodCia = s-codcia
                   Detalle.TpoBol = Pl-Bole.TpoBol
                   Detalle.CodMov = Pl-Bole.CodMov
                   Detalle.DesMov = Pl-Conc.DesMov
                   Detalle.CodCta = Pl-Bole.CodCta.
           END.        
           Detalle.ValCal[Pl-Mov-Mes.NroMes] = Detalle.ValCal[Pl-Mov-Mes.NroMes] + Pl-Mov-Mes.ValCal-Mes. 
        END.
        DISPLAY
          Detalle.CodMov @ Fi-Mensaje
          LABEL "  Cargando Datos" FORMAT "X(13)"
          WITH FRAME F-Proceso.      
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME



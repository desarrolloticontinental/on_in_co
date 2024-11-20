O:
      MESSAGE "Condicion Venta no debe ser blanco" VIEW-AS ALERT-BOX ERROR.
      APPLY "ENTRY" TO CcbCDocu.FmaPgo.
      RETURN "ADM-ERROR".   
   END.
   FOR EACH DETA NO-LOCK: 
       F-Tot = F-Tot + DETA.ImpLin.
   END.
   IF F-Tot = 0 THEN DO:
      MESSAGE "Importe total debe ser mayor a cero" VIEW-AS ALERT-BOX ERROR.
      APPLY "ENTRY" TO CcbCDocu.CodCli.
      RETURN "ADM-ERROR".   
   END.
   
END.

RETURN "OK".
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE valida-Anula V-table-Win 
PROCEDURE valida-Anula :
/*------------------------------------------------------------------------------
  Purpose:     Rutina de validacion en caso de Anulacion
  Parameters:  Regresar "ADM-ERROR" si no se quiere Anular
  Notes:       
------------------------------------------------------------------------------*/
IF NOT AVAILABLE ccbcdocu THEN RETURN "ADM-ERROR".
IF LOOKUP(ccbcdocu.FlgEst,"C,A") > 0 THEN  RETURN "ADM-ERROR".
DEFINE VAR RPTA AS CHAR NO-UNDO.
FIND FIRST FacCfgGn WHERE FacCfgGn.CodCia = S-CODCIA NO-LOCK NO-ERROR.
RUN ALM/D-CLAVE.R(FacCfgGn.Cla_Venta,OUTPUT RPTA). 
IF RPTA = 
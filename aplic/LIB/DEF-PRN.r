UB-TOTAL BY (DMOV.FchVto) DMOV.DbeMn1 @ DMOV.DbeMn1
                       ACCUM SUB-TOTAL BY (DMOV.FchVto) DMOV.HbeMn1 @ DMOV.HbeMn1
                       ACCUM SUB-TOTAL BY (DMOV.FchVto) DMOV.DbeMn2 @ DMOV.DbeMn2
                       ACCUM SUB-TOTAL BY (DMOV.FchVto) DMOV.HbeMn2 @ DMOV.HbeMn2
                       */
                       WITH FRAME F-Cab.
        UNDERLINE STREAM report DMOV.GloDoc
                                DMOV.ImpMn1
                                DMOV.ImpMn2
                                /*
                                DMOV.DbeMn1
                                DMOV.HbeMn1
                                DMOV.DbeMn2
                                DMOV.HbeMn2
                                */
                                WITH FRAME f-cab.
     END.
     IF LAST-OF(DMOV.CodCia) THEN DO:
        UNDERLINE STREAM report DMOV.GloDoc
                                DMOV.ImpMn1
                                DMOV.ImpMn2
                                /*
                                DMOV.DbeMn1
                                DMOV.HbeMn1
                                DMOV.DbeMn2
                                DMOV.HbeMn2
                                */
                                WITH FRAME f-cab.
        DOWN STREAM report 1 WITH  FRAME f-cab.                        
        DISPLAY STREAM report  "TOTAL GENERAL " @ DMOV.GloDoc
                       ACCUM TOTAL BY (DMOV.CodCia) DMOV.ImpMn1 @ DMOV.ImpMn1
                       ACCUM TOTAL BY (DMOV.CodCia) DMOV.ImpMn2 @ DMOV.ImpMn2
                       /*
                       ACCUM TOTAL BY (DMOV.CodCia) DMOV.DbeMn1 @ DMOV.DbeMn1
                       ACCUM TOTAL BY (DMOV.CodCia) DMOV.HbeMn1 @ DMOV.HbeMn1
                       ACCUM TOTAL BY (DMOV.CodCia) DMOV.DbeMn2 @ DMOV.DbeMn2
                       ACCUM TOTAL BY (DMOV.CodCia) DMOV.HbeMn2 @ DMOV.HbeMn2
                       */
                       WITH FRAME F-Cab.
     END.
 END.
 PAGE STREAM report.
 OUTPUT STREAM report CLOSE.
 
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-busca B-table-Win 
PROCEDURE local-busca :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE VARIABLE OK-WAIT-STATE AS LOGICAL NO-UNDO.
  ASSIGN  input-var-1 = ""
          input-var-2 = ""
          input-var-3 = ""
          output-var-1 = ?
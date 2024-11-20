/* recalular precios expolibreria enero 2011 */

DEF VAR s-codcia AS INT INIT 001 NO-UNDO.
DEF VAR s-coddiv AS CHAR INIT '00015' NO-UNDO.
DEF VAR f-factor AS DEC NO-UNDO.
DEF VAR x-canped LIKE facdpedi.canped NO-UNDO.
DEF VAR s-undvta LIKE facdpedi.undvta NO-UNDO.
DEF VAR s-codcli LIKE faccpedi.codcli NO-UNDO.
DEF VAR s-codmon LIKE faccpedi.codmon NO-UNDO.
DEF VAR s-tpocmb LIKE faccpedi.tpocmb NO-UNDO.
DEF VAR s-cndvta LIKE faccpedi.fmapgo NO-UNDO.
DEF VAR f-prebas AS DEC NO-UNDO.
DEF VAR f-prevta AS DEC NO-UNDO.
DEF VAR f-dsctos AS DEC NO-UNDO.
DEF VAR y-dsctos AS DEC NO-UNDO.
DEF VAR z-dsctos AS DEC NO-UNDO.
DEF VAR X-TIPDTO AS CHAR NO-UNDO.

FIND FIRST faccfggn WHERE faccfggn.codcia = s-codcia NO-LOCK.
DEF TEMP-TABLE t-cped LIKE faccpedi.
DEF TEMP-TABLE t-dped LIKE facdpedi.

/* CARGAMOS LOS TEMPORALES */
/* FOR EACH faccpedi NO-LOCK WHERE faccpedi.codcia = s-codcia */
/*     AND faccpedi.coddiv = '00025'                          */
/*     AND faccpedi.coddoc = 'cot':                           */
/*     CREATE t-cped.                                         */
/*     BUFFER-COPY faccpedi TO t-cped.                        */
/*     FOR EACH facdpedi OF faccpedi NO-LOCK:                 */
/*         CREATE t-dped.                                     */
/*         BUFFER-COPY facdpedi TO t-dped.                    */
/*     END.                                                   */
/* END.                                                       */

/* RECALCULAMOS */
FOR EACH faccpedi WHERE faccpedi.codcia = s-codcia
    AND faccpedi.coddiv = s-coddiv                    
    AND faccpedi.coddoc = 'cot'                     
    AND faccpedi.nroped = '015113007'
    AND LOOKUP(faccpedi.flgest, 'p,c') > 0 :
    ASSIGN
        s-codcli = faccpedi.codcli
        s-codmon = faccpedi.codmon
        s-tpocmb = faccpedi.tpocmb
        s-cndvta = faccpedi.fmapgo.
    FOR EACH facdpedi OF faccpedi, FIRST Almmmatg OF facdpedi:
        F-FACTOR = facdpedi.Factor.
        x-CanPed = facdpedi.CanPed.
        s-UndVta = facdpedi.UndVta.
        RUN vtagn/PrecioListaMayorista-1 (
                            s-CodDiv,
                            s-CodCli,
                            s-CodMon,
                            s-TpoCmb,
                            OUTPUT s-UndVta,
                            OUTPUT f-Factor,
                            Almmmatg.CodMat,
                            s-CndVta,
                            x-CanPed,
                            4,
                            OUTPUT f-PreBas,
                            OUTPUT f-PreVta,
                            OUTPUT f-Dsctos,
                            OUTPUT y-Dsctos,
                            OUTPUT z-Dsctos,
                            OUTPUT x-tipdto
                            ).
        IF RETURN-VALUE = "ADM-ERROR" THEN DO:
            NEXT.
        END.
        ASSIGN 
            facdpedi.Factor = f-Factor
            facdpedi.UndVta = s-UndVta
            facdpedi.PreUni = F-PREVTA
            facdpedi.PreBas = F-PreBas 
            facdpedi.PorDto = F-DSCTOS  /* Ambos descuentos afectan */
            facdpedi.PorDto2 = 0        /* el precio unitario */
            facdpedi.Por_Dsctos[2] = z-Dsctos
            facdpedi.Por_Dsctos[3] = Y-DSCTOS 
            facdpedi.AftIgv = Almmmatg.AftIgv
            facdpedi.AftIsc = Almmmatg.AftIsc
            facdpedi.ImpIsc = 0
            facdpedi.ImpIgv = 0.
        ASSIGN
            facdpedi.ImpLin = facdpedi.CanPed * facdpedi.PreUni * 
                        ( 1 - facdpedi.Por_Dsctos[1] / 100 ) *
                        ( 1 - facdpedi.Por_Dsctos[2] / 100 ) *
                        ( 1 - facdpedi.Por_Dsctos[3] / 100 ).
        IF facdpedi.Por_Dsctos[1] = 0 AND facdpedi.Por_Dsctos[2] = 0 AND facdpedi.Por_Dsctos[3] = 0 
            THEN facdpedi.ImpDto = 0.
        ELSE facdpedi.ImpDto = facdpedi.CanPed * facdpedi.PreUni - facdpedi.ImpLin.
        ASSIGN
            facdpedi.ImpLin = ROUND(facdpedi.ImpLin, 2)
            facdpedi.ImpDto = ROUND(facdpedi.ImpDto, 2).
        IF facdpedi.AftIsc THEN 
            facdpedi.ImpIsc = ROUND(facdpedi.PreBas * facdpedi.CanPed * (Almmmatg.PorIsc / 100),4).
        IF facdpedi.AftIgv THEN  
            facdpedi.ImpIgv = facdpedi.ImpLin - ROUND(facdpedi.ImpLin  / (1 + (FacCfgGn.PorIgv / 100)),4).
    END.
  RUN graba-totales.
END.

/* /* CAMBIAMOS DE DIVISION */                                    */
/* FOR EACH t-cped NO-LOCK:                                       */
/*     DISPLAY t-cped.fchped t-cped.nroped.                       */
/*     PAUSE 0.                                                   */
/*     CREATE faccpedi.                                           */
/*     BUFFER-COPY t-cped                                         */
/*         TO faccpedi                                            */
/*         ASSIGN                                                 */
/*         faccpedi.coddiv = '00015'                              */
/*         faccpedi.nroped = '015' + SUBSTRING(t-cped.nroped, 4). */
/*     FOR EACH t-dped OF t-cped NO-LOCK:                         */
/*         CREATE facdpedi.                                       */
/*         BUFFER-COPY                                            */
/*             t-dped                                             */
/*             TO facdpedi                                        */
/*             ASSIGN                                             */
/*             facdpedi.coddiv = faccpedi.coddiv                  */
/*             facdpedi.nroped = faccpedi.nroped.                 */
/*     END.                                                       */
/* END.                                                           */



PROCEDURE graba-totales:

    DEFINE VARIABLE F-IGV AS DECIMAL NO-UNDO.
    DEFINE VARIABLE F-ISC AS DECIMAL NO-UNDO.

    ASSIGN
      faccpedi.ImpDto = 0
      faccpedi.ImpIgv = 0
      faccpedi.ImpIsc = 0
      faccpedi.ImpTot = 0
      faccpedi.ImpExo = 0
      faccpedi.Importe[3] = 0
      F-IGV = 0
      F-ISC = 0.
    FOR EACH facdpedi OF faccpedi NO-LOCK: 
      F-Igv = F-Igv + facdpedi.ImpIgv.
      F-Isc = F-Isc + facdpedi.ImpIsc.
      faccpedi.ImpTot = faccpedi.ImpTot + facdpedi.ImpLin.
      IF NOT facdpedi.AftIgv THEN faccpedi.ImpExo = faccpedi.ImpExo + facdpedi.ImpLin.
      IF facdpedi.AftIgv = YES
      THEN faccpedi.ImpDto = faccpedi.ImpDto + ROUND(facdpedi.ImpDto / (1 + faccpedi.PorIgv / 100), 2).
      ELSE faccpedi.ImpDto = faccpedi.ImpDto + facdpedi.ImpDto.
    END.
    ASSIGN
      faccpedi.ImpIgv = ROUND(F-IGV,2)
      faccpedi.ImpIsc = ROUND(F-ISC,2)
      faccpedi.ImpVta = faccpedi.ImpTot - faccpedi.ImpExo - faccpedi.ImpIgv.
    IF faccpedi.PorDto > 0 THEN DO:
      ASSIGN
          faccpedi.ImpDto = faccpedi.ImpDto + ROUND((faccpedi.ImpVta + faccpedi.ImpExo) * faccpedi.PorDto / 100, 2)
          faccpedi.ImpTot = ROUND(faccpedi.ImpTot * (1 - faccpedi.PorDto / 100),2)
          faccpedi.ImpVta = ROUND(faccpedi.ImpVta * (1 - faccpedi.PorDto / 100),2)
          faccpedi.ImpExo = ROUND(faccpedi.ImpExo * (1 - faccpedi.PorDto / 100),2)
          faccpedi.ImpIgv = faccpedi.ImpTot - faccpedi.ImpExo - faccpedi.ImpVta.
    END.
    faccpedi.ImpBrt = faccpedi.ImpVta + faccpedi.ImpIsc + faccpedi.ImpDto + faccpedi.ImpExo.
END PROCEDURE.


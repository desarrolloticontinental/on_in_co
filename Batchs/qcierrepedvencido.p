/* CIERRE AUTOMATICO DE PEDIDOS Y COTIZACIONES VENCIDAS */
/* SOLO ATE, otras divisiones revisar qborraped */

DISABLE TRIGGERS FOR LOAD OF faccpedi.
DISABLE TRIGGERS FOR LOAD OF facdpedi.

DEF VAR s-codcia AS INT INIT 001 NO-UNDO.
DEF BUFFER B-CPEDI FOR Faccpedi.
DEF BUFFER B-DPEDI FOR Facdpedi.

/* FOR EACH gn-divi NO-LOCK WHERE gn-divi.codcia = s-codcia                                  */
/*         AND gn-divi.coddiv = '00000':   /* POR AHORA SOLO ATE */                          */
/*     FOR EACH FacCPedi WHERE FacCPedi.CodCia = s-codcia                                    */
/*             AND FacCPedi.CodDiv = gn-divi.coddiv                                          */
/*             AND FacCPedi.CodDoc = 'PED'                                                   */
/*             AND LOOKUP(FacCPedi.FlgEst, 'P,X') > 0                                        */
/*             AND FacCPedi.FchVen < TODAY:                                                  */
/*         FIND FIRST Facdpedi OF Faccpedi WHERE Facdpedi.canate > 0                         */
/*             NO-LOCK NO-ERROR.                                                             */
/*         IF AVAILABLE Facdpedi THEN NEXT.                                                  */
/*         DISPLAY                                                                           */
/*             Faccpedi.coddiv                                                               */
/*             Faccpedi.coddoc                                                               */
/*             Faccpedi.nroped                                                               */
/*             Faccpedi.fchped                                                               */
/*             Faccpedi.fchven                                                               */
/*             WITH STREAM-IO NO-BOX.                                                        */
/*         PAUSE 0.                                                                          */
/*         ASSIGN                                                                            */
/*           FacCPedi.FlgEst = 'E'                                                           */
/*           FacCPedi.Libre_f01 = TODAY.                                                     */
/*         FOR EACH Facdpedi OF Faccpedi:                                                    */
/*             /* BORRAMOS SALDO EN LAS COTIZACIONES */                                      */
/*             FIND B-DPEDI WHERE B-DPEDI.CodCia = Faccpedi.CodCia                           */
/*                 AND  B-DPEDI.CodDoc = "COT"                                               */
/*                 AND  B-DPEDI.NroPed = Faccpedi.NroRef                                     */
/*                 AND  B-DPEDI.CodMat = Facdpedi.CodMat                                     */
/*                 EXCLUSIVE-LOCK NO-ERROR.                                                  */
/*             IF AVAILABLE B-DPEDI                                                          */
/*             THEN ASSIGN                                                                   */
/*                   B-DPEDI.FlgEst = 'P'                                                    */
/*                   B-DPEDI.CanAte = B-DPEDI.CanAte - Facdpedi.CanPed.  /* <<<< OJO <<<< */ */
/*             RELEASE B-DPEDI.                                                              */
/*             Facdpedi.FlgEst = Faccpedi.FlgEst.   /* <<< OJO <<< */                        */
/*         END.                                                                              */
/*         FIND B-CPedi WHERE                                                                */
/*             B-CPedi.CodCia = Faccpedi.CODCIA AND                                          */
/*             B-CPedi.CodDiv = Faccpedi.CODDIV AND                                          */
/*             B-CPedi.CodDoc = "COT"    AND                                                 */
/*             B-CPedi.NroPed = Faccpedi.NroRef                                              */
/*             EXCLUSIVE-LOCK NO-ERROR.                                                      */
/*         IF AVAILABLE B-CPedi THEN B-CPedi.FlgEst = "P".                                   */
/*         RELEASE B-CPedi.                                                                  */
/*     END.                                                                                  */
/* END.                                                                                      */

FOR EACH gn-divi NO-LOCK WHERE gn-divi.codcia = s-codcia
        AND gn-divi.coddiv = '00000':   /* POR AHORA SOLO ATE */
    FOR EACH FacCPedi WHERE FacCPedi.CodCia = s-codcia
            AND FacCPedi.CodDiv = gn-divi.coddiv
            AND FacCPedi.CodDoc = 'COT'
            AND FacCPedi.FlgEst = 'P'
            AND FacCPedi.FchVen < TODAY - 7:
        FIND FIRST Facdpedi OF Faccpedi WHERE Facdpedi.canate > 0
            NO-LOCK NO-ERROR.
        IF AVAILABLE Facdpedi THEN NEXT.
        DISPLAY
            Faccpedi.coddiv
            Faccpedi.coddoc
            Faccpedi.nroped
            Faccpedi.fchped
            Faccpedi.fchven
            WITH STREAM-IO NO-BOX.
        PAUSE 0.
        ASSIGN
          FacCPedi.FlgEst = 'X'
          FacCPedi.Libre_f01 = TODAY.
    END.
END.

/* RHC 19.07.10 CIERRA PEDIDOS CON MAS DE 7 DIAS */
FOR EACH gn-divi NO-LOCK WHERE gn-divi.codcia = s-codcia
        AND gn-divi.coddiv = '00000':   /* POR AHORA SOLO ATE */
    FOR EACH FacCPedi WHERE FacCPedi.CodCia = s-codcia
            AND FacCPedi.CodDiv = gn-divi.coddiv
            AND FacCPedi.CodDoc = 'PED'
            /*AND LOOKUP(FacCPedi.FlgEst, 'P,X,G') > 0*/
            AND LOOKUP(FacCPedi.FlgEst, 'A,C,F,R,E') = 0
            AND FacCPedi.FchPed < ( TODAY - 7 ):
        DISPLAY
            Faccpedi.coddiv
            Faccpedi.coddoc
            Faccpedi.nroped
            Faccpedi.fchped
            Faccpedi.fchven
            WITH STREAM-IO NO-BOX.
        PAUSE 0.
        ASSIGN
          FacCPedi.FlgEst = 'E'
          FacCPedi.Libre_f01 = TODAY.
        FOR EACH Facdpedi OF Faccpedi:
            /* BORRAMOS SALDO EN LAS COTIZACIONES */
            FIND B-DPEDI WHERE B-DPEDI.CodCia = Faccpedi.CodCia 
                AND B-DPEDI.CodDiv = Faccpedi.CodDiv
                AND B-DPEDI.CodDoc = "COT" 
                AND B-DPEDI.NroPed = Faccpedi.NroRef
                AND B-DPEDI.CodMat = Facdpedi.CodMat 
                EXCLUSIVE-LOCK NO-ERROR.
            IF AVAILABLE B-DPEDI 
            THEN ASSIGN
                  B-DPEDI.FlgEst = 'P'
                  B-DPEDI.CanAte = B-DPEDI.CanAte - (Facdpedi.CanPed - Facdpedi.CanAte).  /* <<<< OJO <<<< */
            RELEASE B-DPEDI.
            Facdpedi.FlgEst = Faccpedi.FlgEst.   /* <<< OJO <<< */
        END.    
        FIND B-CPedi WHERE 
            B-CPedi.CodCia = Faccpedi.CODCIA AND  
            B-CPedi.CodDiv = Faccpedi.CODDIV AND  
            B-CPedi.CodDoc = "COT"    AND  
            B-CPedi.NroPed = Faccpedi.NroRef
            EXCLUSIVE-LOCK NO-ERROR.
        IF AVAILABLE B-CPedi THEN B-CPedi.FlgEst = "P".
        RELEASE B-CPedi.
    END.
END.

/* RHC 21.02.2012 CIERRA PEDIDOS CON MAS DE 2 DIAS  TIENDAS*/
FOR EACH gn-divi NO-LOCK WHERE gn-divi.codcia = s-codcia
        AND LOOKUP(gn-divi.coddiv, '00001,00002,00003,00014,00011') > 0:   
    FOR EACH FacCPedi WHERE FacCPedi.CodCia = s-codcia
            AND FacCPedi.CodDiv = gn-divi.coddiv
            AND FacCPedi.CodDoc = 'PED'
            /*AND LOOKUP(FacCPedi.FlgEst, 'P,X') > 0*/
            AND LOOKUP(FacCPedi.FlgEst, 'A,C,F,R,E') = 0
            AND FacCPedi.FchPed < ( TODAY - 2 ):
        DISPLAY
            Faccpedi.coddiv
            Faccpedi.coddoc
            Faccpedi.nroped
            Faccpedi.fchped
            Faccpedi.fchven
            WITH STREAM-IO NO-BOX.
        PAUSE 0.
        ASSIGN
          FacCPedi.FlgEst = 'E'
          FacCPedi.Libre_f01 = TODAY.
        FOR EACH Facdpedi OF Faccpedi:
            /* BORRAMOS SALDO EN LAS COTIZACIONES */
            FIND B-DPEDI WHERE B-DPEDI.CodCia = Faccpedi.CodCia 
                AND B-DPEDI.CodDiv = Faccpedi.CodDiv
                AND B-DPEDI.CodDoc = "COT" 
                AND B-DPEDI.NroPed = Faccpedi.NroRef
                AND B-DPEDI.CodMat = Facdpedi.CodMat 
                EXCLUSIVE-LOCK NO-ERROR.
            IF AVAILABLE B-DPEDI 
            THEN ASSIGN
                  B-DPEDI.FlgEst = 'P'
                  B-DPEDI.CanAte = B-DPEDI.CanAte - (Facdpedi.CanPed - Facdpedi.CanAte).  /* <<<< OJO <<<< */
            RELEASE B-DPEDI.
            Facdpedi.FlgEst = Faccpedi.FlgEst.   /* <<< OJO <<< */
        END.    
        FIND B-CPedi WHERE 
            B-CPedi.CodCia = Faccpedi.CODCIA AND  
            B-CPedi.CodDiv = Faccpedi.CODDIV AND  
            B-CPedi.CodDoc = "COT"    AND  
            B-CPedi.NroPed = Faccpedi.NroRef
            EXCLUSIVE-LOCK NO-ERROR.
        IF AVAILABLE B-CPedi THEN B-CPedi.FlgEst = "P".
        RELEASE B-CPedi.
    END.
END.

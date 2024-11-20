
DEFINE VAR lHojaDeRutas AS CHAR.
DEFINE VAR lHojaRuta AS CHAR.
DEFINE VAR x-rowid AS ROWID.

DEFINE BUFFER B-RutaC FOR di-rutaC.
DEFINE BUFFER x-di-rutaD FOR di-rutaD.
DEFINE BUFFER x-di-rutaG FOR di-rutaG.

DEFINE VAR x-sec AS INT.

lHojaDeRutas = "060002553,060002554,060002555,060002556,060002557,060002558,060002559,060002560".

REPEAT x-sec = 1 TO NUM-ENTRIES(lHojaDeRutas,","):

    lHojaRuta = ENTRY(x-sec,lHojaDeRutas,",").

    FIND FIRST b-rutaC WHERE b-rutaC.codcia = 1 AND
                             b-rutaC.coddoc = 'H/R' AND 
                             b-rutaC.nrodoc = lHojaRuta NO-LOCK NO-ERROR.
    IF AVAILABLE b-rutaC THEN DO:
        FOR EACH Di-RutaD NO-LOCK OF B-RutaC WHERE Di-RutaD.CodRef = "G/R",
          FIRST Ccbcdocu NO-LOCK WHERE Ccbcdocu.codcia = B-RutaC.codcia
          AND Ccbcdocu.coddoc = DI-RutaD.CodRef
          AND Ccbcdocu.nrodoc = DI-RutaD.NroRef,
          FIRST CcbCBult NO-LOCK WHERE CcbCBult.CodCia = Ccbcdocu.codcia
          AND CcbCBult.CodDoc = Ccbcdocu.Libre_C01      
          AND CcbCBult.NroDoc = Ccbcdocu.Libre_C02      
          /*AND CcbCBult.CHR_01 = "P"                     /* H/R Aún NO cerrada */*/
          BREAK BY CcbCBult.CodDoc BY CcbCBult.NroDoc:

          x-rowid = ROWID(di-rutaD).
          FIND FIRST x-di-rutaD WHERE ROWID(x-di-rutaD) = x-rowid NO-ERROR.
          IF AVAILABLE x-di-rutaD THEN x-di-rutaD.Libre_d02 = 0.
          
          IF FIRST-OF(CcbCBult.CodDoc) OR FIRST-OF(CcbCBult.NroDoc) THEN DO:
              x-rowid = ROWID(di-rutaD).
              FIND FIRST x-di-rutaD WHERE ROWID(x-di-rutaD) = x-rowid NO-ERROR.
              IF AVAILABLE x-di-rutaD THEN x-di-rutaD.Libre_d02 = CcbCBult.Bultos.
          END.    
        END.

        /* RHC 19.06.2012 REPARTIMOS LOS BULTOS POR LAS GUIAS DE REMISION vs ORDENES DE DESPACHO */
        FOR EACH Di-RutaG NO-LOCK OF B-RutaC,
          FIRST Almcmov NO-LOCK WHERE Almcmov.CodCia = Di-RutaG.CodCia
          AND Almcmov.CodAlm = Di-RutaG.CodAlm
          AND Almcmov.TipMov = Di-RutaG.Tipmov
          AND Almcmov.CodMov = Di-RutaG.Codmov
          AND Almcmov.NroSer = Di-RutaG.serref
          AND Almcmov.NroDoc = Di-RutaG.nroref,
          FIRST CcbCBult NO-LOCK WHERE CcbCBult.CodCia = Almcmov.codcia
          AND CcbCBult.CodDoc = Almcmov.codref      
          AND CcbCBult.NroDoc = Almcmov.nroref      
          /*AND CcbCBult.CHR_01 = "P"*/                     /* H/R Aún NO cerrada */
          BREAK BY CcbCBult.CodDoc BY CcbCBult.NroDoc:

          x-rowid = ROWID(di-rutaG).
          FIND FIRST x-di-rutaG WHERE ROWID(x-di-rutaG) = x-rowid NO-ERROR.
          IF AVAILABLE x-di-rutaG THEN x-Di-RutaG.CodMat = ''. 

          IF FIRST-OF(CcbCBult.CodDoc) OR FIRST-OF(CcbCBult.NroDoc) THEN DO:
              x-rowid = ROWID(di-rutaG).
              FIND FIRST x-di-rutaG WHERE ROWID(x-di-rutaG) = x-rowid NO-ERROR.
              IF AVAILABLE x-di-rutaG THEN x-Di-RutaG.CodMat = STRING(CcbCBult.Bultos). 

          END.
        END.
    END.
    ELSE DO:
        DISPLAY "Hoja de Ruta No existe " + lHojaRuta FORMAT 'x(45)'.
        PAUSE 0.
    END.
END.


/*           
           
           
           
/* -------------------------------------------- */           
DEFINE VAR lOrdDesp AS CHAR.
DEFINE VAR lOD AS CHAR.
DEFINE VAR lPed AS CHAR.
DEFINE VAR lOk AS LOG.

DEFINE VAR x-rowid AS ROWID.

lOrdDesp = '065002038'.

FIND FIRST faccpedi WHERE faccpedi.codcia = 1 AND faccpedi.coddoc = 'O/D' AND 
                            faccpedi.nroped = lOrdDesp NO-LOCK NO-ERROR.
IF AVAILABLE faccpedi THEN DO:
    /* Si la O/D esta rotulada */
    FIND FIRST CcbCBult WHERE CcbCBult.CodCia = faccpedi.codcia
        AND CcbCBult.CodDoc = 'O/D'      /* O/D O/M */
        AND CcbCBult.NroDoc = faccpedi.nroped
        AND CcbCBult.CHR_01 = "P"               /* P:La O/D - OTR, aun no esta en H/R */
        NO-LOCK NO-ERROR.
    IF AVAILABLE Ccbcbult THEN DO:
        /* las guias (G/R) de la O/D que esten facturadas */
        lOk = YES.
        lOd = faccpedi.nroped.
        lPed = faccpedi.nroref.
        FOR EACH ccbcdocu WHERE ccbcdocu.codcia = 1 AND 
                        (ccbcdocu.codped = 'PED' AND ccbcdocu.nroped = lPed) AND 
                        ccbcdocu.coddoc = 'G/R' /*AND 
                        (ccbcdocu.libre_c01 = 'O/D' AND ccbcdocu.libre_c02 = lOd ) AND
                        Ccbcdocu.flgest = "F" */ NO-LOCK :
            IF ccbcdocu.coddoc = 'G/R' AND (ccbcdocu.libre_c01 = 'O/D' AND ccbcdocu.libre_c02 = faccpedi.nroped )
                AND Ccbcdocu.flgest = "F" THEN DO:
                lOk = YES.
                DISPLAY "G/R" ccbcdocu.nrodoc.         
                
                /* consistencia en otros documentos */
                FOR EACH di-rutad NO-LOCK WHERE di-rutad.codcia = 1
                    AND di-rutad.coddoc = 'H/R'
                    AND di-rutad.codref = ccbcdocu.coddoc
                    AND di-rutad.nroref = ccbcdocu.nrodoc /*USE-INDEX llave02*/,
                    FIRST di-rutac OF di-rutad NO-LOCK:


                    /*DISPLAY "H/R" di-rutad.nrodoc.*/
                    IF di-rutac.flgest = "E" OR di-rutac.flgest = "P" OR di-rutac.flgest = "X" THEN DO:
                        /*MESSAGE "Documento ya se encuentra registrado en la H.R.:" di-rutac.nrodoc
                            VIEW-AS ALERT-BOX ERROR.*/
                        lOk = NO.
                        /*RETURN "ADM-ERROR".*/
                    END.
                    IF di-rutac.flgest = "C"  AND di-rutad.flgest = "C" THEN DO:
                        /*MESSAGE "Documento ya se encuentra registrado en la H.R.:" di-rutac.nrodoc
                            VIEW-AS ALERT-BOX ERROR.*/
                        lOk = NO.
                        /*RETURN "ADM-ERROR".*/
                    END.
                END.
                
                IF lOk = YES THEN DO:
                    /*DISPLAY ccbcdocu.coddoc ccbcdocu.nrodoc.*/
                END.
                PAUSE 0.
            END.

        END.
    END.
    ELSE DO:
        MESSAGE "OD ya se despacho/No esta Rotulado/No existe OD".
    END.
END.
ELSE DO:
    MESSAGE "No existe O/D " lOrdDesp.
END.

*/


/*
            IF CAN-FIND(FIRST DI-RutaD OF DI-RutaC WHERE DI-RutaD.codref = DI-RutaD.CodRef:SCREEN-VALUE IN BROWSE {&BROWSE-NAME}
                        AND DI-RutaD.nroref = DI-RutaD.NroRef:SCREEN-VALUE IN BROWSE {&BROWSE-NAME}
                        NO-LOCK)
                THEN DO:
                MESSAGE "El documento" DI-RutaD.CodRef:SCREEN-VALUE IN BROWSE {&BROWSE-NAME}
                    DI-RutaD.NroRef:SCREEN-VALUE IN BROWSE {&BROWSE-NAME}
                    "ya fue registrado en esta hoja de ruta" VIEW-AS ALERT-BOX ERROR.
                RETURN 'ADM-ERROR'.
            END.
  */
  
  
/*  
  
  
  FIND FIRST CcbCBult WHERE CcbCBult.CodCia = Ccbcdocu.codcia
      /*AND CcbCBult.CodDiv = s-coddiv*/
      AND CcbCBult.CodDoc = Ccbcdocu.Libre_C01      /* O/D O/M */
      AND CcbCBult.NroDoc = Ccbcdocu.Libre_C02
      AND CcbCBult.CHR_01 = "P"
      NO-LOCK NO-ERROR.
  IF NOT AVAILABLE Ccbcbult THEN DO:
      MESSAGE 'Falta ROTULAR la' Ccbcdocu.Libre_C01 Ccbcdocu.Libre_C02
          VIEW-AS ALERT-BOX ERROR.
      RETURN "ADM-ERROR".
  END.
  /* *************************************** */
  RUN GET-ATTRIBUTE("ADM-NEW-RECORD").
  IF RETURN-VALUE = "YES" THEN DO:
      IF CAN-FIND(FIRST DI-RutaD OF DI-RutaC WHERE DI-RutaD.codref = DI-RutaD.CodRef:SCREEN-VALUE IN BROWSE {&BROWSE-NAME}
                  AND DI-RutaD.nroref = DI-RutaD.NroRef:SCREEN-VALUE IN BROWSE {&BROWSE-NAME}
                  NO-LOCK)
          THEN DO:
          MESSAGE "El documento" DI-RutaD.CodRef:SCREEN-VALUE IN BROWSE {&BROWSE-NAME}
              DI-RutaD.NroRef:SCREEN-VALUE IN BROWSE {&BROWSE-NAME}
              "ya fue registrado en esta hoja de ruta" VIEW-AS ALERT-BOX ERROR.
          RETURN 'ADM-ERROR'.
      END.
      /* consistencia en otros documentos */
      FOR EACH b-rutad NO-LOCK WHERE b-rutad.codcia = s-codcia
          AND b-rutad.coddoc = s-coddoc
          AND b-rutad.codref = DI-RutaD.CodRef:SCREEN-VALUE IN BROWSE {&BROWSE-NAME}
          AND b-rutad.nroref = DI-RutaD.NroRef:SCREEN-VALUE IN BROWSE {&BROWSE-NAME},
          FIRST b-rutac OF b-rutad NO-LOCK:
          IF b-rutac.flgest = "E" OR b-rutac.flgest = "P" OR b-rutac.flgest = "X" THEN DO:
              MESSAGE "Documento ya se encuentra registrado en la H.R.:" b-rutac.nrodoc
                  VIEW-AS ALERT-BOX ERROR.
              RETURN "ADM-ERROR".
          END.
          IF b-rutac.flgest = "C"  AND b-rutad.flgest = "C" THEN DO:
              MESSAGE "Documento ya se encuentra registrado en la H.R.:" b-rutac.nrodoc
                  VIEW-AS ALERT-BOX ERROR.
              RETURN "ADM-ERROR".
          END.
      END.
  END.

/* VALIDACIONESSSSSSS  */  
  FIND ccbcdocu WHERE ccbcdocu.codcia = s-codcia
    AND ccbcdocu.coddoc = DI-RutaD.CodRef:SCREEN-VALUE IN BROWSE {&BROWSE-NAME}
    AND ccbcdocu.nrodoc = DI-RutaD.NroRef:SCREEN-VALUE IN BROWSE {&BROWSE-NAME}
    NO-LOCK NO-ERROR.
  IF NOT AVAILABLE ccbcdocu
  THEN DO:
    MESSAGE "El documento" DI-RutaD.CodRef:SCREEN-VALUE IN BROWSE {&BROWSE-NAME}
        DI-RutaD.NroRef:SCREEN-VALUE IN BROWSE {&BROWSE-NAME}
        "no está registrado" VIEW-AS ALERT-BOX ERROR.
    RETURN 'ADM-ERROR'.
  END.
  IF Ccbcdocu.flgest = "A" THEN DO:
      MESSAGE "El documento se encuentra anulado" VIEW-AS ALERT-BOX ERROR.
      APPLY 'entry' TO DI-RutaD.NroRef IN BROWSE {&browse-name}.
      RETURN "ADM-ERROR".
  END.
  IF Ccbcdocu.flgest <> "F" THEN DO:
      MESSAGE "El documento NO está FACTURADO" VIEW-AS ALERT-BOX ERROR.
      APPLY 'entry' TO DI-RutaD.NroRef IN BROWSE {&browse-name}.
      RETURN "ADM-ERROR".
  END.

*/

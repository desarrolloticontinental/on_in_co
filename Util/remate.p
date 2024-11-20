DEFINE VARIABLE S-CODCIA AS INTEGER INIT 001.
DEFINE VARIABLE F-FACTOR     AS DECIMAL   NO-UNDO.
DEFINE VARIABLE MaxCat LIKE ClfClie.PorDsc.
DEFINE VARIABLE MaxVta LIKE Dsctos.PorDto.
DEFINE VARIABLE F-PreVta-A AS DECIMAL NO-UNDO.
DEFINE VARIABLE F-PreVta-B AS DECIMAL NO-UNDO.
DEFINE VARIABLE F-PreVta-C AS DECIMAL NO-UNDO.
DEFINE VARIABLE F-MrgUti-A AS DECIMAL NO-UNDO.
DEFINE VARIABLE F-MrgUti-B AS DECIMAL NO-UNDO.
DEFINE VARIABLE F-MrgUti-C AS DECIMAL NO-UNDO.
DEFINE VARIABLE X-CTOUND AS DECIMAL NO-UNDO.

DEF TEMP-TABLE t-matg LIKE almmmatg.
DEF VAR x-linea AS CHAR FORMAT 'x(6)'.

FIND FacCfgGn WHERE FacCfgGn.CodCia = S-CODCIA NO-LOCK NO-ERROR.


INPUT FROM c:\tmp\remate20.prn.
REPEAT:
    IMPORT UNFORMATTED x-linea.
    CREATE t-matg.
    ASSIGN
        t-matg.codcia = s-codcia
        t-matg.codmat = x-linea.
END.                            
INPUT CLOSE.

FOR EACH t-matg WHERE t-matg.codmat <> '', FIRST Almmmatg OF t-matg:
    DISPLAY t-matg.codmat almmmatg.codmat.
    PAUSE 0.
    ASSIGN
        Almmmatg.PorMax = 20
        Almmmatg.MrgUti-A = 10
        Almmmatg.FchAct = TODAY
        Almmmatg.FchmPre[1] = TODAY.
    /* DESCUENTO POR VOLUMEN */
    ASSIGN
        Almmmatg.DtoVolR[1] = 1
        Almmmatg.DtoVolD[1] = 20.

    /* Margen de Utilidad A */
    ASSIGN
        F-MrgUti-A = Almmmatg.MrgUti-A
        X-CTOUND   = Almmmatg.CtoTot
        F-FACTOR = 1
        F-PreVta-A = 0.
     /****   Busca el Factor de conversion   ****/
     IF Almmmatg.UndA <> "" THEN DO:
         FIND Almtconv WHERE Almtconv.CodUnid = Almmmatg.UndBas
                        AND  Almtconv.Codalter = Almmmatg.UndA
                       NO-LOCK NO-ERROR.
         IF NOT AVAILABLE Almtconv THEN DO:
            MESSAGE "Codigo de unidad no exixte" Almmmatg.codmat VIEW-AS ALERT-BOX ERROR.
            NEXT.
         END.
         F-FACTOR = Almtconv.Equival.
         F-PreVta-A = ROUND(( X-CTOUND * (1 + F-MrgUti-A / 100) ), 6) * F-FACTOR.
     END.
    Prevta[2] = f-PreVta-A.

    /* Calculamos el precio de la oficina */
    RUN Precio-Oficina.
    IF RETURN-VALUE = 'ADM-ERROR' THEN NEXT.

    /* Calculos finales */
    IF Almmmatg.UndA <> "" THEN DO:
      FIND Almtconv WHERE Almtconv.CodUnid = Almmmatg.UndBas
          AND  Almtconv.Codalter = Almmmatg.UndA
          NO-LOCK NO-ERROR.
      F-FACTOR = Almtconv.Equival.
    END.

    Almmmatg.PreVta[1] = IF Almmmatg.Chr__02 = "T"  THEN Almmmatg.PreVta[2] / F-FACTOR ELSE Almmmatg.PreVta[1].

    IF Almmmatg.AftIgv THEN 
      Almmmatg.PreBas = ROUND(Almmmatg.PreVta[1] / ( 1 + FacCfgGn.PorIgv / 100), 6).

    Almmmatg.MrgUti = ((Almmmatg.Prevta[1] / Almmmatg.Ctotot) - 1 ) * 100. 

    F-FACTOR = 1.
      /****   Busca el Factor de conversion   ****/
    IF Almmmatg.UndA <> "" THEN DO:
          FIND Almtconv WHERE Almtconv.CodUnid = Almmmatg.UndBas
                         AND  Almtconv.Codalter = Almmmatg.UndA
                        NO-LOCK NO-ERROR.
          F-FACTOR = Almtconv.Equival.
          Almmmatg.Dsctos[1] =  (((Almmmatg.Prevta[2] / F-FACTOR)/ Almmmatg.Prevta[1]) - 1 ) * 100. 
    END.


    F-FACTOR = 1.
      /****   Busca el Factor de conversion   ****/
    IF Almmmatg.UndB <> "" THEN DO:
          FIND Almtconv WHERE Almtconv.CodUnid = Almmmatg.UndBas
                         AND  Almtconv.Codalter = Almmmatg.UndB
                        NO-LOCK NO-ERROR.
          F-FACTOR = Almtconv.Equival.
          Almmmatg.Dsctos[2] =  (((Almmmatg.Prevta[3] / F-FACTOR)/ Almmmatg.Prevta[1]) - 1 ) * 100. 
    END.

    F-FACTOR = 1.
      /****   Busca el Factor de conversion   ****/
    IF Almmmatg.UndC <> "" THEN DO:
          FIND Almtconv WHERE Almtconv.CodUnid = Almmmatg.UndBas
                         AND  Almtconv.Codalter = Almmmatg.UndC
                        NO-LOCK NO-ERROR.
          F-FACTOR = Almtconv.Equival.
          Almmmatg.Dsctos[3] =  (((Almmmatg.Prevta[4] / F-FACTOR)/ Almmmatg.Prevta[1]) - 1 ) * 100. 
    END.


END.
RETURN.



PROCEDURE Precio-Oficina:
/* ********************** */

DEFINE VARIABLE fmot LIKE Almmmatg.PreOfi.
DEFINE VARIABLE pre-ofi LIKE Almmmatg.PreOfi.
DEFINE VARIABLE MrgMin LIKE Almmmatg.MrgUti-A.
DEFINE VARIABLE MrgOfi LIKE Almmmatg.MrgUti-A.

MaxCat = 0.
MaxVta = 0.
fmot   = 0.
MrgMin = 100.
MrgOfi = 0.
F-FACTOR = 1.
MaxCat = 4.
MaxVta = 3.

ASSIGN
    F-MrgUti-A = Almmmatg.MrgUti-A
    F-PreVta-A = Almmmatg.Prevta[2]
    F-MrgUti-B = Almmmatg.MrgUti-B
    F-PreVta-B = Almmmatg.Prevta[3]
    F-MrgUti-C = Almmmatg.MrgUti-C
    F-PreVta-C = Almmmatg.Prevta[4].

X-CTOUND = Almmmatg.CtoTot.

/****   Busca el Factor de conversion   ****/
IF Almmmatg.Chr__01 <> "" THEN DO:
    FIND Almtconv WHERE Almtconv.CodUnid = Almmmatg.UndBas
                   AND  Almtconv.Codalter = Almmmatg.Chr__01
                  NO-LOCK NO-ERROR.
    IF NOT AVAILABLE Almtconv THEN DO:
       MESSAGE "Codigo de unidad no existe" Almmmatg.codmat VIEW-AS ALERT-BOX ERROR.
       RETURN 'ADM-ERROR'.
    END.
    F-FACTOR = Almtconv.Equival.
END.
/*******************************************/

CASE Almmmatg.Chr__02 :
    WHEN "T" THEN DO:        
        
        IF F-MrgUti-A < MrgMin AND F-MrgUti-A <> 0 THEN MrgMin = F-MrgUti-A.
        IF F-MrgUti-B < MrgMin AND F-MrgUti-B <> 0 THEN MrgMin = F-MrgUti-B.
        IF F-MrgUti-C < MrgMin AND F-MrgUti-C <> 0 THEN MrgMin = F-MrgUti-C.
        
        fmot = (1 + MrgMin / 100) / ((1 - MaxCat / 100) * (1 - MaxVta / 100)).
        
        pre-ofi = X-CTOUND * fmot * F-FACTOR .        
       
        MrgOfi = ROUND((fmot - 1) * 100, 6).

    END.
    WHEN "P" THEN DO:
       pre-ofi = Almmmatg.Prevta[1] * F-FACTOR.
       MrgOfi = ((Almmmatg.Prevta[1] / Almmmatg.Ctotot) - 1 ) * 100. 
    END. 
END.    

Almmmatg.Dec__01 = MrgOfi.
Almmmatg.PreOfi = Pre-Ofi.


END.

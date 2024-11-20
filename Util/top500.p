DEF VAR x-codmat AS CHAR FORMAT 'x(6)'.
DEF VAR s-codmon AS INT INIT 1.
DEF VAR f-prebas AS DEC.
DEF VAR f-porimp AS DEC INIT 1.
DEF VAR f-factor AS DEC INIT 1.

DEF TEMP-TABLE detalle LIKE almmmatg.

INPUT FROM m:\tmp\top500.prn.
REPEAT:
    IMPORT UNFORMATTED x-codmat.
    FIND detalle WHERE detalle.codcia = 1 AND detalle.codmat = x-codmat
        NO-LOCK NO-ERROR.
    IF NOT AVAILABLE detalle THEN DO:
        CREATE detalle.
        ASSIGN
            detalle.codcia = 1
            detalle.codmat = x-codmat.
    END.
END.

FOR EACH detalle WHERE codmat <> '', FIRST almmmatg OF detalle NO-LOCK:
    IF S-CODMON = 1 THEN DO:
       IF Almmmatg.MonVta = 1 THEN
           ASSIGN F-PREBAS = (Almmmatg.PreOfi * F-PorImp) * F-FACTOR.
       ELSE
           ASSIGN F-PREBAS = (Almmmatg.PreOfi * F-PorImp) * Almmmatg.TpoCmb * F-FACTOR.
    END.
    IF S-CODMON = 2 THEN DO:
       IF Almmmatg.MonVta = 2 THEN
          ASSIGN F-PREBAS = (Almmmatg.PreOfi * F-PorImp) * F-FACTOR.
       ELSE
          ASSIGN F-PREBAS = ((Almmmatg.PreOfi * F-PorImp) / Almmmatg.TpoCmb) * F-FACTOR.
    END.
    detalle.preofi = f-prebas.
END.
OUTPUT TO m:\tmp\top500.txt.
FOR EACH detalle, FIRST almmmatg OF detalle NO-LOCK:
    DISPLAY 
        detalle.codmat
        almmmatg.desmat
        almmmatg.preofi
        WITH STREAM-IO NO-BOX.
END.
OUTPUT CLOSE.   
       

DEF BUFFER AMATE FOR Almmmate.
DEF VAR s-codalm AS CHAR INIT '21N'.
DEF VAR x-codmat AS CHAR.
DEF VAR s-codcia AS INT INIT 001.

INPUT FROM d:\tmp\asignar21n.txt.
REPEAT:
    IMPORT UNFORMATTED x-codmat.
    IF x-codmat = '' THEN LEAVE.
    FIND Almmmatg WHERE Almmmatg.codcia = s-codcia
        AND Almmmatg.codmat = x-codmat
        NO-ERROR.
    IF NOT AVAILABLE Almmmatg THEN NEXT.
    FIND AMATE WHERE AMATE.CodCia = Almmmatg.CodCia AND
         AMATE.CodAlm = S-CODALM AND
         AMATE.CodMat = Almmmatg.CodMat NO-LOCK NO-ERROR.
    IF NOT AVAILABLE AMATE THEN DO:
       CREATE AMATE.
       ASSIGN AMATE.CodCia = Almmmatg.CodCia
              AMATE.CodAlm = S-CODALM
              AMATE.CodMat = Almmmatg.CodMat
              AMATE.DesMat = Almmmatg.DesMat
              AMATE.UndVta = Almmmatg.UndStk
              AMATE.CodMar = Almmmatg.CodMar
              AMATE.FacEqu = Almmmatg.FacEqu.
       FIND FIRST almautmv WHERE 
            almautmv.CodCia = Almmmatg.codcia AND
            almautmv.CodFam = Almmmatg.codfam AND
            almautmv.CodMar = Almmmatg.codMar AND
            almautmv.Almsol = AMATE.CodAlm NO-LOCK NO-ERROR.
       IF AVAILABLE almautmv THEN 
          ASSIGN AMATE.AlmDes = almautmv.Almdes
                 AMATE.CodUbi = almautmv.CodUbi.
       /* Actualizamos la lista de Almacenes */ 
       IF Almmmatg.almacenes = "" THEN Almmmatg.almacenes = TRIM(AMATE.CodAlm).
       IF LOOKUP(TRIM(AMATE.CodAlm),Almmmatg.almacenes) = 0 THEN
          ASSIGN Almmmatg.almacenes = TRIM(Almmmatg.almacenes) + "," + 
                                     TRIM(AMATE.CodAlm).
    END.
END.


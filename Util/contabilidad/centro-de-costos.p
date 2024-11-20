DEF TEMP-TABLE t-ccos LIKE gn-ccos.
DEF VAR x-linea AS CHAR FORMAT 'x(100)'.

INPUT FROM c:\tmp\conti.prn.
REPEAT:
    IMPORT UNFORMATTED x-linea.
    IF x-linea <> '' THEN DO:
        CREATE t-ccos.
        ASSIGN
            t-ccos.Cco = SUBSTRING(x-linea,1,7)
            t-ccos.Nivel = INTEGER(SUBSTRING(x-linea,114,1))
            t-ccos.Estado = IF SUBSTRING(x-linea,111,1) = 'N' THEN NO ELSE YES
            t-ccos.DesCco = SUBSTRING(x-linea,11,50)
            t-ccos.CodCia = 001
            t-ccos.Libre_c01 = SUBSTRING(x-linea,61,50)
            t-ccos.Libre_c02 = SUBSTRING(x-linea,115,2).
      CASE t-ccos.Nivel:
          WHEN 3 THEN t-ccos.NivAnt = SUBSTRING(t-ccos.cco,1,2).
          WHEN 5 THEN t-ccos.NivAnt = SUBSTRING(t-ccos.cco,1,3).
          WHEN 7 THEN t-ccos.NivAnt = SUBSTRING(t-ccos.cco,1,5).
      END CASE.
    END.
END.
/*
FOR EACH t-ccos:
    DISPLAY 
        t-ccos.cco
        t-ccos.descco
        t-ccos.nivel
        t-ccos.nivant
        t-ccos.estado
        t-ccos.libre_c01
        t-ccos.libre_c02
        WITH STREAM-IO NO-BOX WIDTH 200.
END.
*/

RUN migrar.

RUN costos.

RUN equivalencia.

PROCEDURE migrar:

FOR EACH gn-ccos WHERE gn-ccos.codcia = 001:
    DELETE gn-ccos.
END.
FOR EACH t-ccos NO-LOCK:
    CREATE gn-ccos.
    BUFFER-COPY t-ccos TO gn-ccos.
END.

END PROCEDURE.

PROCEDURE Costos:


    FOR EACH t-ccos NO-LOCK WHERE t-ccos.libre_c02 <> '' AND t-ccos.nivel = 7:
        FIND cb-auxi WHERE cb-auxi.codcia = 000
            AND cb-auxi.clfaux = 'cco'
            AND cb-auxi.codaux = t-ccos.libre_c02
            NO-ERROR.
        IF NOT AVAILABLE cb-auxi THEN DO:
            CREATE cb-auxi.
            ASSIGN
                cb-auxi.codcia = 000
                cb-auxi.clfaux = 'CCO'
                cb-auxi.codaux = t-ccos.libre_c02.
        END.
        ASSIGN
            cb-auxi.nomaux = t-ccos.descco
            cb-auxi.libre_c01 = t-ccos.cco.
    END.

END PROCEDURE.

PROCEDURE equivalencia:

FOR EACH cb-auxi WHERE cb-auxi.codcia = 000 AND cb-auxi.clfaux = 'CCO':
    cb-auxi.libre_c01 = ''.
    FIND gn-ccos WHERE gn-ccos.codcia = 1
        AND gn-cco.libre_c02 = cb-auxi.codaux
        NO-LOCK NO-ERROR.
    IF AVAILABLE gn-ccos THEN cb-auxi.libre_c01 = gn-ccos.cco.
END.

END PROCEDURE.


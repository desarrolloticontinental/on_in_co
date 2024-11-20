OUTPUT TO D:\SIE\BARRAS.TXT.
FOR EACH INTEGRAL.almmmatg WHERE
    almmmatg.CodCia = 1
    AND almmmatg.TpoArt <> "D" NO-LOCK,
    EACH Almmmat1 OF Almmmatg:
    DISPLAY
        Almmmatg.codmat
        Almmmatg.desmat
        Almmmatg.CODFAM
        Almmmatg.codbrr COLUMN-LABEL "EAN 13"
        Almmmat1.barras[1] COLUMN-LABEL "EAN 14 (1)"
        Almmmat1.equival[1] COLUMN-LABEL "EQUIVALENCIA (1)"
        Almmmat1.barras[2] COLUMN-LABEL "EAN 14 (2)"
        Almmmat1.equival[2] COLUMN-LABEL "EQUIVALENCIA (2)"
        Almmmat1.barras[3] COLUMN-LABEL "EAN 14 (3)"
        Almmmat1.equival[3] COLUMN-LABEL "EQUIVALENCIA (3)"
        WITH STREAM-IO WIDTH 242.
END.
OUTPUT CLOSE.

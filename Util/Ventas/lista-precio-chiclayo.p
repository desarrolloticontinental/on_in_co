/* Precio Chiclayo */
DEF VAR s-codcia AS INT INIT 001 NO-UNDO.
DEF VAR s-coddiv AS CHAR INIT '00065' NO-UNDO.
DEF VAR s-codalm AS CHAR INIT '65' NO-UNDO.
DEF VAR f-PreBas AS DEC NO-UNDO.
DEF VAR f-PreVta AS DEC NO-UNDO.
DEF VAR f-Dsctos AS DEC NO-UNDO.
DEF VAR y-Dsctos AS DEC NO-UNDO.
DEF VAR x-TipDto AS CHAR NO-UNDO.
DEF VAR f-FleteUnitario AS DEC NO-UNDO.
DEF VAR f-Factor AS DEC NO-UNDO.
DEF VAR s-UndVta AS CHAR NO-UNDO.

OUTPUT TO c:\tmp\listachiclayo.txt.
PUT UNFORMATTED
    "CODIGO|DESCRIPCION|MARCA|LINEA|SUBLINEA|UNIDAD|PRECIO BASE|FLETE|PRECIO TOTAL|COSTO UNIT C/IGV|MON DEL COSTO"
    SKIP.
FOR EACH almmmatg NO-LOCK WHERE codcia = 001 AND tpoart <> "D" AND undB <> '':
    FIND Almtconv WHERE Almtconv.CodUnid  = Almmmatg.UndBas 
        AND Almtconv.Codalter = almmmatg.undB
        NO-LOCK NO-ERROR.
    IF NOT AVAILABLE almtconv THEN NEXT.
    s-UndVta = Almmmatg.UndB.
    RUN vta2/PrecioMayorista-Cont-v2 (s-CodCia,
                                   s-CodDiv,
                                   '11111111111',
                                   1,
                                   almmmatg.tpocmb,
                                   OUTPUT f-Factor,
                                   almmmatg.codmat,
                                   '',
                                   s-UndVta,
                                   1,
                                   4,
                                   s-codalm,   /* Necesario para REMATES */
                                   OUTPUT f-PreBas,
                                   OUTPUT f-PreVta,
                                   OUTPUT f-Dsctos,
                                   OUTPUT y-Dsctos,
                                   OUTPUT x-TipDto,
                                   OUTPUT f-FleteUnitario
                                   ).
    PUT UNFORMATTED
        almmmatg.codmat '|'
        almmmatg.desmat '|'
        almmmatg.desmar '|'
        almmmatg.codfam '|'
        almmmatg.subfam '|'
        s-UndVta '|'
        f-prevta '|'
        f-fleteunitario '|'
        f-prevta + f-fleteunitario '|'
        almmmatg.ctotot '|'
        (IF almmmatg.monvta = 2 THEN 'US$' ELSE 'S/.')
        SKIP.
END.
OUTPUT CLOSE.


DEF VAR s-codcia AS INT INIT 001 NO-UNDO.
DEF VAR cl-codcia AS INT INIT 000 NO-UNDO.
DEF VAR pv-codcia AS INT INIT 000 NO-UNDO.

DEF VAR s-coddiv AS CHAR INIT '00000' NO-UNDO.
DEF VAR x-origen AS CHAR NO-UNDO.
DEF VAR x-destino AS CHAR NO-UNDO.
DEF VAR x-documento AS CHAR NO-UNDO.
OUTPUT TO d:\tmp\cd00000.txt.
PUT UNFORMATTED
    'ALMACEN|MOVIMIENTO|REFERENCIA|NRO. REFERENCIA|ORIGEN|DESTINO|FECHA|'
    'DOCUMENTO|NRO. DOCUMENTO|CODIGO|DESCRIPCION|UNIDAD|'
    'MARCA|CANTIDAD|PESO TOTAL|VOLUMEN TOTAL|VALOR TOTAL'
    SKIP.
FOR EACH Almacen NO-LOCK WHERE Almacen.codcia = s-codcia
    AND Almacen.coddiv = s-coddiv,
    EACH Almcmov OF almacen NO-LOCK WHERE almcmov.flgest <> 'A'
    AND almcmov.fchdoc >= DATE(10,01,2014),
    FIRST Almtmovm OF Almcmov NO-LOCK,
    EACH Almdmov OF almcmov NO-LOCK,
    FIRST Almmmatg OF Almdmov NO-LOCK:
    IF Almcmov.tipmov = "I" AND 
        NOT (Almcmov.codmov = 02 OR Almcmov.codmov = 03 OR Almcmov.codmov = 06
             OR Almcmov.codmov = 90)
        THEN NEXT.
    IF Almcmov.tipmov = "S" AND 
        NOT (Almcmov.codmov = 02 OR Almcmov.codmov = 03)
        THEN NEXT.
    ASSIGN
        x-Origen = ''
        x-Destino = ''.
    CASE Almcmov.tipmov:
        WHEN 'I' THEN DO:
            x-Documento = 'Parte de Ingreso'.
            IF Almtmovm.PidCli = YES THEN DO:
                x-Origen = Almcmov.nomref.
                FIND gn-clie WHERE gn-clie.codcia = cl-codcia
                    AND gn-clie.codcli = Almcmov.codcli
                    NO-LOCK NO-ERROR.
                IF AVAILABLE gn-clie THEN x-Origen = gn-clie.NomCli.
            END.
            IF Almtmovm.PidPro = YES THEN DO:
                x-Origen = Almcmov.nomref.
                FIND gn-prov WHERE gn-prov.codcia = pv-codcia
                    AND gn-prov.codpro = Almcmov.codpro
                    NO-LOCK NO-ERROR.
                IF AVAILABLE gn-prov THEN x-Origen = gn-prov.NomPro.
            END.
            IF Almcmov.codref <> '' THEN x-Documento = Almcmov.codref.
            IF Almcmov.codmov = 03 THEN ASSIGN x-Origen = Almcmov.CodAlm x-Destino = Almcmov.AlmDes.
        END.
        WHEN 'S' THEN DO:
            x-Documento = 'Parte de Salida'.
            IF Almtmovm.PidCli = YES THEN DO:
                x-Destino = Almcmov.nomref.
                FIND gn-clie WHERE gn-clie.codcia = cl-codcia
                    AND gn-clie.codcli = Almcmov.codcli
                    NO-LOCK NO-ERROR.
                IF AVAILABLE gn-clie THEN x-Destino = gn-clie.NomCli.
            END.
            IF Almtmovm.PidPro = YES THEN DO:
                x-Destino = Almcmov.nomref.
                FIND gn-prov WHERE gn-prov.codcia = pv-codcia
                    AND gn-prov.codpro = Almcmov.codpro
                    NO-LOCK NO-ERROR.
                IF AVAILABLE gn-prov THEN x-Destino = gn-prov.NomPro.
            END.
            IF Almcmov.codref <> '' THEN x-Documento = Almcmov.codref.
            IF Almcmov.codmov = 03 THEN ASSIGN x-Origen = Almcmov.CodAlm x-Destino = Almcmov.AlmDes.
        END.
    END CASE.
    PUT UNFORMATTED
        almcmov.codalm '|'
        almcmov.tipmov
        almcmov.codmov '|'
        almcmov.codref '|'
        almcmov.nroref '|'
        x-Origen '|'
        x-Destino '|'
        almcmov.fchdoc '|'
        x-Documento '|'
        STRING(almcmov.nroser,'999') ' ' STRING(almcmov.nrodoc,'999999999') '|'
        almdmov.codmat '|'
        almmmatg.desmat '|'
        almdmov.codund '|'
        almmmatg.desmar '|'
        almdmov.candes '|'
        almdmov.candes * almdmov.factor * almmmatg.pesmat '|'
        almdmov.candes * almdmov.factor * almmmatg.libre_d02 '|'
        almdmov.candes * almdmov.factor * Almdmov.VctoMn1
        SKIP.

END.

OUTPUT CLOSE.

DEF VAR f-estado AS CHAR.
DEF VAR FILL-IN-NomUsr AS CHAR.
DEF VAR FILL-IN-NomAux AS CHAR.
DEF VAR FILL-IN-NomAlm AS CHAR.
DEF VAR f-unitario LIKE AlmStkge.CtoUni.
DEF VAR x-tipo AS CHAR.

OUTPUT TO c:\tmp\usointerno.txt.
PUT UNFORMATTED 
    'FECHA|SOLICITADO POR|CCO|ALMACEN|ESTADO|TIPO'
    '|ARTICULO|CANT SOLICITADA|CANT APROBADA|UNIDAD|CANT ATENDIDA|PROM UNIT S/.'
    SKIP.
FOR EACH almcdocu no-lock WHERE codcia = 1 and coddoc = 'SUI' and lookup(flgest, 'p,c') > 0,
    EACH almddocu OF almcdocu NO-LOCK,
    FIRST almmmatg NO-LOCK WHERE Almmmatg.CodCia = AlmDDocu.CodCia
    AND Almmmatg.codmat = AlmDDocu.Codigo:
    ASSIGN
        f-estado = ''
        FILL-IN-NomUsr = ''
        FILL-IN-NomAux = ''
        FILL-IN-NomAlm = ''
        f-unitario = 0
        x-tipo = almcdocu.libre_c03
        .
    CASE almcdocu.libre_c03:
        WHEN '04' THEN x-tipo = 'Para Uso Administrativo'.
        WHEN '05' THEN x-tipo = 'Para Promociones y Publicidad'.
    END CASE.
    RUN vta2/p-faccpedi-flgest (AlmCDocu.flgest, AlmCDocu.coddoc, OUTPUT f-Estado).
    FIND _User WHERE _User._UserId = AlmCDocu.UsrCreacion NO-LOCK NO-ERROR.
    IF AVAILABLE _User THEN FILL-IN-NomUsr = _User._User-name.
    FIND cb-auxi WHERE cb-auxi.codcia = 000
        AND cb-auxi.clfaux = "CCO"
        AND cb-auxi.codaux = AlmCDocu.CodLlave NO-LOCK NO-ERROR.
    IF AVAILABLE cb-auxi THEN FILL-IN-NomAux = cb-auxi.nomaux.
    FIND almacen WHERE almacen.codcia = 001
        AND almacen.codalm = AlmCDocu.Libre_c02 NO-LOCK NO-ERROR.
    IF AVAILABLE almacen THEN FILL-IN-NomAlm = almacen.descrip.
    FIND LAST AlmStkge WHERE AlmStkge.Fecha <= almcdocu.fchdoc
        AND AlmStkge.codmat = almddocu.codigo
        AND AlmStkge.CodCia = 001
        NO-LOCK NO-ERROR.
    IF AVAILABLE almstkge THEN f-unitario = AlmStkge.CtoUni .
    
    PUT UNFORMATTED
        almcdocu.fchdoc '|'
        almcdocu.usrcreacion ' ' FILL-IN-NomUsr '|'
        almcdocu.codllave ' ' FILL-IN-NomAux '|'
        almcdocu.libre_c02 ' ' FILL-IN-NomAlm '|'
        f-estado '|'
        x-tipo '|'
        almddocu.codigo ' ' almmmatg.desmat '|'
        almddocu.libre_d01 '|'
        almddocu.libre_d02 '|'
        almmmatg.undbas '|'
        almddocu.libre_d03 '|'
        f-unitario
        SKIP.
END.

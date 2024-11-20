DEF VAR s-codcia AS INTE INIT 001 NO-UNDO.
DEF VAR x-fchini AS DATE NO-UNDO.
DEF VAR x-fchfin AS DATE NO-UNDO.
DEF VAR x-coddoc AS CHAR INIT 'o/d,otr' NO-UNDO.
DEF VAR x-Fecha-Pedido AS DATE NO-UNDO.
DEF VAR x-Hora-Pedido AS CHAR NO-UNDO.

DEF VAR x-NomCli AS CHAR NO-UNDO.
DEF VAR x-DirCli AS CHAR NO-UNDO.

DEF VAR x-LugEnt AS CHAR NO-UNDO.
DEF VAR x-Ubigeo-1 AS CHAR NO-UNDO.
DEF VAR x-Ubigeo-2 AS CHAR NO-UNDO.
DEF VAR x-Ubigeo-3 AS CHAR NO-UNDO.
DEF VAR x-Ubigeo-4 AS CHAR NO-UNDO.

DEF BUFFER PEDIDO FOR Faccpedi.

ASSIGN
    x-fchini = DATE(01,01,2019)
    x-fchfin = DATE(12,31,2019).
OUTPUT TO d:\ordenes2019.txt.
PUT UNFORMATTED
    'DIV ORIGEN|DIV DESPACHO|DOC|NUMERO|REF|NRO REF|FECHA EMISION|'
    'HORA EMISION|FECHA ENTREGA|CLIENTE|NOMBRE|DIRECCION|RUC|SEDE|TPOPED|'
    'ALMACEN|MONEDA|TPO CMB|VENDEDOR|FLGSIT|FLGEST|ENTREGA|'
    'UBIGEO 1|UBIGEO 2|UBIGEO 3|UBIGEO 4|USR APROBACION|'
    'FECHA APROBACION|USR CHEQUEO|FECHA CHEQUEO|HORA CHEQUEO|CROSS DOCKING|'
    'ALMACEN XD|ARTICULO|UNIDAD|FACTOR|CAN PEDIDA|CAN ATENDIDA|IMPORTE|'
    'CAN PICK|CAN SOL|CAN APROB|FECHA PEDIDO|HORA PEDIDO'
    SKIP.
FOR EACH faccpedi NO-LOCK WHERE faccpedi.codcia = s-codcia AND
    LOOKUP(faccpedi.coddoc, x-coddoc) > 0
    AND faccpedi.fchped >= x-fchini
    AND faccpedi.fchped <= x-fchfin:
    ASSIGN
        x-NomCli = Faccpedi.nomcli
        x-DirCli = Faccpedi.dircli.
    RUN lib/limpiar-texto-abc (INPUT Faccpedi.nomcli, INPUT " ", OUTPUT x-NomCli).
    RUN lib/limpiar-texto-abc (INPUT Faccpedi.dircli, INPUT " ", OUTPUT x-DirCli).
    RUN lib/limpiar-texto-abc (INPUT Faccpedi.lugent, INPUT " ", OUTPUT x-LugEnt).
    RUN lib/limpiar-texto-abc (INPUT Faccpedi.ubigeo[1], INPUT " ", OUTPUT x-Ubigeo-1).
    RUN lib/limpiar-texto-abc (INPUT Faccpedi.ubigeo[2], INPUT " ", OUTPUT x-Ubigeo-2).
    RUN lib/limpiar-texto-abc (INPUT Faccpedi.ubigeo[3], INPUT " ", OUTPUT x-Ubigeo-3).
    RUN lib/limpiar-texto-abc (INPUT Faccpedi.ubigeo[4], INPUT " ", OUTPUT x-Ubigeo-4).
    ASSIGN
        x-Fecha-Pedido = ?
        x-Hora-Pedido = ''.
    FIND FIRST PEDIDO WHERE PEDIDO.codcia = Faccpedi.codcia AND
        PEDIDO.coddoc = Faccpedi.codref AND
        PEDIDO.nroped = Faccpedi.nroref NO-LOCK NO-ERROR.
    IF AVAILABLE PEDIDO THEN
        ASSIGN
        x-Fecha-Pedido = PEDIDO.FchPed
        x-Hora-Pedido = PEDIDO.Hora.
    FOR EACH facdpedi OF faccpedi NO-LOCK:
        PUT UNFORMATTED
            faccpedi.coddiv '|'
            faccpedi.divdes '|'
            faccpedi.coddoc '|'
            faccpedi.nroped '|'
            faccpedi.codref '|'
            faccpedi.nroref '|'
            faccpedi.fchped '|'
            faccpedi.hora '|'
            faccpedi.fchent '|'
            faccpedi.codcli '|'
            x-nomcli '|'
            x-dircli '|'
            faccpedi.ruccli '|'
            faccpedi.sede '|'
            faccpedi.tpoped '|'
            faccpedi.codalm '|'
            faccpedi.codmon '|'
            faccpedi.tpocmb '|'
            faccpedi.codven '|'
            faccpedi.flgsit '|'
            faccpedi.flgest '|'
            x-LugEnt '|'
            x-Ubigeo-1 '|'
            x-Ubigeo-2 '|'
            x-Ubigeo-3 '|'
            x-Ubigeo-4 '|'
            faccpedi.usraprobacion '|'
            faccpedi.fchaprobacion '|'
            faccpedi.usrchq  '|'
            faccpedi.fchchq '|'
            faccpedi.horchq '|'
            faccpedi.crossdocking '|'
            faccpedi.almacenxd '|'
            facdpedi.codmat '|'
            facdpedi.undvta '|'
            facdpedi.factor '|'
            facdpedi.canped '|'
            facdpedi.canate '|'
            facdpedi.implin '|'
            facdpedi.canpick '|'
            facdpedi.cansol '|'
            facdpedi.canapr '|'
            x-Fecha-Pedido '|'
            x-Hora-Pedido
            SKIP.
    END.
END.
OUTPUT CLOSE.


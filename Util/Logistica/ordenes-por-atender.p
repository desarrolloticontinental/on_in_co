/* O/C NO Vencidas por atender */

DEF VAR s-codcia AS INT INIT 001.
DEF VAR pv-codcia AS INT INIT 000.

OUTPUT TO c:\tmp\ordesnes-por-atender.txt.
PUT UNFORMATTED
    'Numero|Proveedor|Nombre|Articulo|Descripcion|Linea|Sublinea|Pedido|Atendido|Saldo'
    SKIP.
FOR EACH lg-cocmp NO-LOCK WHERE lg-cocmp.codcia = s-codcia
    AND lg-cocmp.fchvto < TODAY
    AND lg-cocmp.flgsit = 'P',
    EACH lg-docmp OF lg-cocmp NO-LOCK,
    FIRST gn-prov WHERE gn-prov.codcia = pv-codcia AND gn-prov.codpro = lg-cocmp.codpro,
    FIRST almmmatg OF lg-docmp NO-LOCK,
    FIRST almtfami OF almmmatg NO-LOCK WHERE Almtfami.SwComercial = YES:
    PUT UNFORMATTED
        lg-docmp.nrodoc '|'
        gn-prov.codpro '|'
        gn-prov.nompro '|'
        almmmatg.codmat '|'
        almmmatg.desmat '|'
        almmmatg.codfam '|'
        almmmatg.subfam '|'
        LG-DOCmp.CanPedi '|'
        LG-DOCmp.CanAten '|'
        (LG-DOCmp.CanPedi - LG-DOCmp.CanAten)
        SKIP.


END.

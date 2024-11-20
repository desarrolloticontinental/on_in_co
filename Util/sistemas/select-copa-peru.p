
OUTPUT TO d:\AngieData.txt.

SELECT 
    faccpedi.fchped                                     LABEL 'Fecha', 
    faccpedi.hora                       FORMAT 'x(5)'   LABEL 'Hora', 
    faccpedi.nroped                     FORMAT 'x(15)'  LABEL 'Pedido',
    ENTRY(1, facdpedi.libre_c03, '|')   FORMAT 'x(15)'  LABEL 'Proveedor',
    gn-prov.nompro                      FORMAT 'x(100)' LABEL 'Nombre Proveedor',
    ENTRY(2, Facdpedi.libre_c03, '|')   FORMAT 'x(15)'  LABEL 'Promotor',
    VtaTabla.libre_c01                  FORMAT 'x(100)' LABEL 'Nombre Promotor',
    ENTRY(3, Facdpedi.libre_c03, '|')   FORMAT 'x(20)'  LABEL 'Equipo',
    facdpedi.implin                                     LABEL 'Importe'
    FROM faccpedi
    JOIN facdpedi ON (
        facdpedi.codcia = faccpedi.codcia AND
        facdpedi.coddiv = faccpedi.coddiv AND
        facdpedi.coddoc = faccpedi.coddoc AND
        facdpedi.nroped = faccpedi.nroped
        )
    JOIN gn-prov ON (
        gn-prov.codcia = 0 AND
        gn-prov.codpro = ENTRY(1,facdpedi.libre_c03,'|') 
        )
    JOIN vtatabla ON (
        vtatabla.codcia = 1 AND
        vtatabla.tabla = 'EXPOPROMOTOR' AND
        vtatabla.llave_c1 = faccpedi.lista_de_precios AND
        vtatabla.llave_c2 = ENTRY(1, facdpedi.libre_c03, '|') AND
        vtatabla.llave_c3 = ENTRY(2, Facdpedi.libre_c03, '|')
        )
    WHERE faccpedi.codcia = 1 AND
    faccpedi.coddiv = '10015' AND
    faccpedi.coddoc = 'COT' AND
    faccpedi.fchped >= TODAY - 7 AND
    faccpedi.fchped <= TODAY AND
    faccpedi.flgest <> 'A'
    WITH STREAM-IO NO-BOX WIDTH 320



DEF VAR s-codcia AS INTE INIT 001 NO-UNDO.
DEF VAR x-fchini AS DATE NO-UNDO.
DEF VAR x-fchfin AS DATE NO-UNDO.
DEF VAR x-coddoc AS CHAR INIT 'H/R,PHR' NO-UNDO.
DEF VAR x-Fecha-Pedido AS DATE NO-UNDO.
DEF VAR x-Hora-Pedido AS CHAR NO-UNDO.

DEF VAR x-NomTra AS CHAR NO-UNDO.
DEF VAR x-Observ AS CHAR NO-UNDO.
DEF VAR x-DesRut AS CHAR NO-UNDO.

DEF VAR x-GlosaAprobacion AS CHAR NO-UNDO.

DEF VAR x-CodRef AS CHAR INIT 'G/R' NO-UNDO.
DEF VAR x-NroRef AS CHAR NO-UNDO.

ASSIGN
    x-fchini = DATE(01,01,2019)
    x-fchfin = DATE(12,31,2019).
OUTPUT TO d:\ruta2019.txt.
PUT UNFORMATTED
    'CODIGO|NUMERO|FECHA|ESTADO|USUARIO|RUTA|TRANSPORTISTA|TPOTRA|VEHICULO|FCH RET|'
    'HOR SAL|HOR RET|OBSERVACIONES|DIVISION|KM INI|KM FIN|RESPONSABLE|'
    'AYUDANTE-1|AYUDANTE-2|FCH SAL|GUIA TRANSP|USR CIERRE|FCH CIERRE|LIBRE C01|'
    'LIBRE C03|LIBRE C05|LIBRE L01|LIBRE F05|USR APROB|FCH APROB|GLOSA APROB|'
    'REF|NRO REF|DET ESTADO|HOR LLEGADA|HOR PARTIDA|FLG EST DET|DET LIBRE C02|'
    'DET LIBRE C04|DET LIBRE C05|DET LIBRE D01|DET LIBNRE D02'
    SKIP.
FOR EACH di-rutac NO-LOCK WHERE di-rutac.codcia = s-codcia 
    AND LOOKUP(di-rutac.coddoc, x-coddoc) > 0
    AND di-rutac.fchdoc >= x-fchini
    AND di-rutac.fchdoc <= x-fchfin:
    ASSIGN
        x-NomTra = di-rutac.nomtra
        x-Observ = di-rutac.observ
        x-GlosaAprobacion = di-rutac.glosaaprobacion
        x-DesRut = di-rutac.desrut.
    RUN lib/limpiar-texto-abc (INPUT di-rutac.nomtra, INPUT " ", OUTPUT x-NomTra).
    RUN lib/limpiar-texto-abc (INPUT di-rutac.observ, INPUT " ", OUTPUT x-Observ).
    RUN lib/limpiar-texto-abc (INPUT di-rutac.glosaaprobacion, INPUT " ", OUTPUT x-GlosaAprobacion).
    RUN lib/limpiar-texto-abc (INPUT di-rutac.desrut, INPUT " ", OUTPUT x-DesRut).
    FOR EACH di-rutad OF di-rutac NO-LOCK:
        PUT UNFORMATTED
            di-rutac.coddoc '|'
            di-rutac.nrodoc '|'
            di-rutac.fchdoc '|'
            di-rutac.flgest '|'
            di-rutac.usuario '|'
            x-desrut '|'
            x-nomtra '|'
            di-rutac.tpotra '|'
            di-rutac.codveh '|'
            di-rutac.fchret '|'
            di-rutac.horsal '|'
            di-rutac.horret '|'
            x-observ '|'
            di-rutac.coddiv '|'
            di-rutac.kmtini '|'
            di-rutac.kmtfin '|'
            di-rutac.responsable '|'
            di-rutac.ayudante-1 '|'
            di-rutac.ayudante-2 '|'
            di-rutac.fchsal '|'
            di-rutac.guiatransportista '|'
            di-rutac.usrcierre '|'
            di-rutac.fchcierre '|'
            di-rutac.libre_c01  '|'
            di-rutac.libre_c03 '|'
            di-rutac.libre_c05 '|'
            di-rutac.libre_l01 '|'
            di-rutac.libre_f05 '|'
            di-rutac.usraprobacion '|'
            di-rutac.fchaprobacion '|'
            x-glosaaprobacion '|'

            di-rutad.codref '|'
            di-rutad.nroref '|'
            di-rutad.flgest '|'
            di-rutad.horlle '|'
            di-rutad.horpar '|'
            di-rutad.flgestdet '|'
            di-rutad.libre_c02 '|'
            di-rutad.libre_c04 '|'
            di-rutad.libre_c05 '|'
            di-rutad.libre_d01 '|'
            di-rutad.libre_d02 
            SKIP.
    END.
    FOR EACH di-rutag OF di-rutac NO-LOCK:
        x-NroRef = STRING(di-rutag.serref, '999') + STRING(di-rutag.nroref, '99999999').
        PUT UNFORMATTED
            di-rutac.coddoc '|'
            di-rutac.nrodoc '|'
            di-rutac.fchdoc '|'
            di-rutac.flgest '|'
            di-rutac.usuario '|'
            x-desrut '|'
            x-nomtra '|'
            di-rutac.tpotra '|'
            di-rutac.codveh '|'
            di-rutac.fchret '|'
            di-rutac.horsal '|'
            di-rutac.horret '|'
            x-observ '|'
            di-rutac.coddiv '|'
            di-rutac.kmtini '|'
            di-rutac.kmtfin '|'
            di-rutac.responsable '|'
            di-rutac.ayudante-1 '|'
            di-rutac.ayudante-2 '|'
            di-rutac.fchsal '|'
            di-rutac.guiatransportista '|'
            di-rutac.usrcierre '|'
            di-rutac.fchcierre '|'
            di-rutac.libre_c01  '|'
            di-rutac.libre_c03 '|'
            di-rutac.libre_c05 '|'
            di-rutac.libre_l01 '|'
            di-rutac.libre_f05 '|'
            di-rutac.usraprobacion '|'
            di-rutac.fchaprobacion '|'
            x-glosaaprobacion '|'

            x-codref '|'
            x-nroref '|'
            di-rutag.flgest '|'
            di-rutag.horlle '|'
            di-rutag.horpar '|'
            di-rutag.flgestdet '|'
            di-rutag.libre_c02 '|'
            di-rutag.libre_c04 '|'
            di-rutag.libre_c05 '|'
            di-rutag.libre_d01 '|'
            di-rutag.libre_d02 
            SKIP.
    END.
END.
OUTPUT CLOSE.

